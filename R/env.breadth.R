#' Calculates breadth of a model in environment space using latin hypercube sampling
#'
#' @param model An enmtools.model object or a model object that can be projected using the predict() function
#' @param env A raster, raster stack of environmental data, or a list of minima and maxima for the environmental space to evaluate models over.
#' @param tolerance How close do successive overlap metrics have to be before we decide we're close enough to the final answer
#' @param max.reps Maximum number of attempts that will be made to find suitable starting conditions
#' @param chunk.size How many combinations of environmental variables to try at a time.  If your niche breadth in environment space is small, increasing this value may help you get a result.
#'
#' @return A list containing the environmental space version of the B2 metric and a plot of B2 estimates as a function of sample size, used as a convergence diagnostic.
#'
#' @examples
#' \donttest{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni <- iberolacerta.clade$species$cyreni
#' cyreni.glm <- enmtools.glm(cyreni, euro.worldclim, test.prop = 0.2,
#' f = pres ~ bio1 + bio12, nback = 500)
#' env.breadth(cyreni.glm,  euro.worldclim)
#' }

env.breadth <- function(model, env, tolerance = .0001, max.reps = 10, chunk.size = 100000){

  if(inherits(model, "enmtools.model")){
    model <- model$model
  }

  # Setting it up so we can handle either a set of rasters or a list of minima and maxima
  if(inherits(env, c("raster", "RasterStack", "RasterBrick", "RasterLayer"))){
    mins <- minValue(env)
    maxes <- maxValue(env)
  } else if (inherits(env, "list")){
    mins <- unlist(lapply(env, min))
    maxes <- unlist(lapply(env, max))
  }

  # These two are tracking whether we have good enough starting conditions
  # and how many times we've tried
  continue <- FALSE
  n.reps <- 0

  # Some of the DM and BC models were barfing on certain starting conditions
  # so I've put this bit in here to make it try a few times.
  while(continue == FALSE & n.reps < max.reps){

    gens <- chunk.size

    # Draw a starting latin hypercube scheme
    this.lhs <- randomLHS(chunk.size, length(names(env)))
    predict.table <- t(t(this.lhs) * (maxes  - mins) + mins)

    # Use that sample space to get a starting overlap value
    colnames(predict.table) <- names(env)
    if(inherits(model, "DistModel")){
      pred <- as.numeric(predict(model, x = data.frame(predict.table), type = "response"))
    } else {
      if(inherits(model, "ranger")) {
        pred <- as.numeric(predict(model, data = data.frame(predict.table), type = "response")$predictions[ , 2, drop = TRUE])
      } else {
        pred <- as.numeric(predict(model, newdata = data.frame(predict.table), type = "response"))
      }
    }

    if(max(pred) == 0){
      this.B2 <- NA
    } else {
      this.B2 <- calc.B2(pred)
    }

    # Check to see if the value is usable, roll again if not
    if(!is.na(this.B2)){
      continue <- TRUE
    } else {
      n.reps <- n.reps + 1
    }
  }

  # If we fail to find useful starting conditions we'll just barf an NA and give up
  if(n.reps == max.reps){
    warning("\n\nCould not find suitable starting conditions for environmental breadth, returning NA\n\n")
    return(list(env.B2 = NA,
                B2.plot = NA))
  } else {

    # So here we've got good starting conditions and we're going to keep going
    # with the LHS design until we get a minimum difference between subsequent
    # samples (delta < tolerance)
    delta <- 1

    # print(paste(this.diff, delta))

    while(delta > tolerance){

      # Keep track of our last value
      # old.diff <- this.diff

      # Add chunk.size rows to the LHS and build a new predict table
      this.lhs <- randomLHS(chunk.size, length(names(env)))
      predict.table <- t(t(this.lhs) * (maxes  - mins) + mins)
      colnames(predict.table) <- names(env)

      # Make new predictions and recalculate metrics
      if(inherits(model, "DistModel")){
        pred <- c(pred, as.numeric(predict(model, x = data.frame(predict.table), type = "response")))
      } else {
        if(inherits(model, "ranger")) {
          pred <- as.numeric(predict(model, data = data.frame(predict.table), type = "response")$predictions[ , 2, drop = TRUE])
        } else {
          pred <- as.numeric(predict(model, newdata = data.frame(predict.table), type = "response"))
        }
      }

      if(max(pred) == 0){
        next
      } else {
        this.B2 <- c(this.B2, calc.B2(pred))
        gens <- c(gens, max(gens) + chunk.size)
      }

      # Calculate delta for this iteration
      delta <- abs(mean(this.B2) - mean(this.B2[-length(this.B2)]))

    }
  }

  output <- list(env.B2 = mean(this.B2),
                 B2.plot = qplot(gens, this.B2, ylab = "B2", xlab = "Samples"))

  return(output)
}



