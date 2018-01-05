#' Calculates overlap between models in environment space using latin hypercube sampling
#'
#' @param model.1 An enmtools.model object model object that can be projected using the predict() function
#' @param model.2 Another enmtools.model object or other model object that can be projected using the predict() function
#' @param env A raster or raster stack of environmental data.
#' @param tolerance How close do successive overlap metrics have to be before we decide we're close enough to the final answer
#' @param max.reps Maximum number of attempts that will be made to find suitable starting conditions
#' @param cor.method Which method to use for calculating correlations between models
#'
#' @export env.overlap
#'
#' @examples
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni <- iberolacerta.clade$species$cyreni
#' monticola <- iberolacerta.clade$species$monticola
#' cyreni.mx <- enmtools.maxent(cyreni, euro.worldclim, nback = 500)
#' monticola.mx <- enmtools.maxent(monticola, euro.worldclim, nback = 500)
#' env.overlap(cyreni.mx, monticola.mx, euro.worldclim)

env.overlap <- function(model.1, model.2, env, tolerance = .001, max.reps = 10, cor.method = "spearman"){

  if(inherits(model.1, "enmtools.model")){
    model.1 <- model.1$model
  }

  if(inherits(model.2, "enmtools.model")){
    model.2 <- model.2$model
  }

  # These two are tracking whether we have good enough starting conditions
  # and how many times we've tried
  continue <- FALSE
  n.reps <- 0

  # Some of the DM and BC models were barfing on certain starting conditions
  # so I've put this bit in here to make it try a few times.
  while(continue == FALSE & n.reps < max.reps){

    # Draw a starting latin hypercube scheme
    this.lhs <- randomLHS(10000, length(names(env)))

    # Setting it up so we can handle either a set of rasters or a list of minima and maxima
    if(inherits(env, c("raster", "RasterStack", "RasterBrick", "RasterLayer"))){
      mins <- minValue(env)
      maxes <- maxValue(env)
    } else if (inherits(env, "list")){
      mins <- unlist(lapply(env, min))
      maxes <- unlist(lapply(env, max))
    }

    predict.table <- t(t(this.lhs) * (maxes  - mins) + mins)

    # Use that sample space to get a starting overlap value
    colnames(predict.table) <- names(env)
    if(inherits(model.1, "DistModel")){
      pred1 <- as.numeric(predict(model.1, x = data.frame(predict.table), type = "response"))
    } else {
      pred1 <- as.numeric(predict(model.1, newdata = data.frame(predict.table), type = "response"))
    }

    if(inherits(model.2, "DistModel")){
      pred2 <- as.numeric(predict(model.2, x = data.frame(predict.table), type = "response"))
    } else {
      pred2 <- as.numeric(predict(model.2, newdata = data.frame(predict.table), type = "response"))
    }


    if(sd(pred1) == 0 | sd(pred2) == 0){
      output <- list(env.D = NA,
                     env.I = NA,
                     env.cor = NA)

      return(output)
    }

    this.d <- 1 - sum(abs(pred1/sum(pred1) - pred2/(sum(pred2))))/2
    this.i <- 1 - sum((sqrt(pred1/sum(pred1)) - sqrt(pred2/sum(pred2)))**2)/2
    this.cor <- cor(pred1, pred2, method = cor.method)

    # Check to see if the value is usable, roll again if not
    if(!is.nan(this.d) & !is.nan(this.i)){
      continue <- TRUE
    } else {
      n.reps <- n.reps + 1
    }
  }

  # If we fail to find useful starting conditions we'll just barf an NA and give up
  if(n.reps == max.reps){
    cat("\n\nCould not find suitable starting conditions for environmental overlap, returning NA\n\n")
    this.d <- NA
    this.i <- NA
    this.cor <- NA
  } else {

    # So here we've got good starting conditions and we're going to keep going
    # with the LHS design until we get a minimum difference between subsequent
    # samples (delta < tolerance)
    delta <- 1

    # print(paste(this.diff, delta))

    while(delta > tolerance){

      # Keep track of our last value
      # old.diff <- this.diff

      # Add 1000 rows to the LHS and build a new predict table
      this.lhs <- randomLHS(10000, length(names(env)))
      predict.table <- t(t(this.lhs) * (maxes  - mins) + mins)
      colnames(predict.table) <- names(env)

      if(inherits(model.1, "DistModel")){
        pred1 <- as.numeric(predict(model.1, x = data.frame(predict.table), type = "response"))
      } else {
        pred1 <- as.numeric(predict(model.1, newdata = data.frame(predict.table), type = "response"))
      }

      if(inherits(model.2, "DistModel")){
        pred2 <- as.numeric(predict(model.2, x = data.frame(predict.table), type = "response"))
      } else {
        pred2 <- as.numeric(predict(model.2, newdata = data.frame(predict.table), type = "response"))
      }

      if(sd(pred1) == 0 | sd(pred2) == 0){
        output <- list(env.D = NA,
                       env.I = NA,
                       env.cor = NA)

        return(output)
      }

      this.d <- c(this.d, 1 - sum(abs(pred1/sum(pred1) - pred2/(sum(pred2))))/2)
      this.i <- c(this.i, 1 - sum((sqrt(pred1/sum(pred1)) - sqrt(pred2/sum(pred2)))**2)/2)
      if(sd(pred1) == 0 | sd(pred2) == 0){
        this.cor <- c(this.cor, 0)
      } else {
        this.cor <- c(this.cor, cor(pred1, pred2, method = cor.method))
      }


      # Calculate delta for this iteration
      delta <- max(c(abs(mean(this.d) - mean(this.d[-length(this.d)])),
                     abs(mean(this.i) - mean(this.i[-length(this.i)])),
                     abs(mean(this.cor) - mean(this.cor[-length(this.cor)]))), na.rm=TRUE)
      #print(delta)
    }
  }

  output <- list(env.D = mean(this.d),
                 env.I = mean(this.i),
                 env.cor = mean(this.cor))

  return(output)
}
