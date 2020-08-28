#' Calculates overlap between models in environment space using latin hypercube sampling
#'
#' @param model.1 An enmtools.model object model object that can be projected using the predict() function
#' @param model.2 Another enmtools.model object or other model object that can be projected using the predict() function
#' @param env A raster, raster stack of environmental data, or a list of minima and maxima for the environmental space to evaluate models over
#' @param tolerance How close do successive overlap metrics have to be before we decide we're close enough to the final answer
#' @param max.reps Maximum number of attempts that will be made to find suitable starting conditions
#' @param cor.method Which method to use for calculating correlations between models
#' @param chunk.size How many combinations of environmental variables to try at a time.  If your niche breadth in environment space is small, increasing this value may help you get a result.
#' @param recal.model.1 Optional.  The output of enmtools.recalibrate for model 1, which needs to have been run with "recalibrate = TRUE".
#' @param recal.model.2 Optional.  The output of enmtools.recalibrate for model 2, which needs to have been run with "recalibrate = TRUE".
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#'
#' @return A list of values measuring the overlap between models in environment space, as well as some plots depicting change of the estimates as a function of how many samples were used, which are included as a sort of convergence diagnostic.
#'
#' @examples
#' \donttest{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni <- iberolacerta.clade$species$cyreni
#' monticola <- iberolacerta.clade$species$monticola
#' cyreni.glm <- enmtools.glm(cyreni, euro.worldclim, f = pres ~ bio1 + bio12, nback = 500)
#' monticola.glm <- enmtools.glm(monticola, euro.worldclim, f = pres ~ bio1 + bio12, nback = 500)
#' env.overlap(cyreni.glm, monticola.glm, euro.worldclim)
#' }

env.overlap <- function(model.1, model.2, env, tolerance = .001, max.reps = 10, cor.method = "spearman", chunk.size = 100000, recal.model.1 = NA, recal.model.2 = NA, verbose = FALSE){

  if(inherits(model.1, "enmtools.model")){
    model.1 <- model.1$model
  }

  if(inherits(model.2, "enmtools.model")){
    model.2 <- model.2$model
  }

  # Check if recal models exist, and if they're the right class
  # Have to use 'all' for these because recal models are lists
  if(!all(is.na(recal.model.1))){
    if(!inherits(recal.model.1, "enmtools.recalibrated.model")){
      stop("recal.model.1 is not an enmtools.recalibrated.model object!")
    }
  }

  if(!all(is.na(recal.model.2))){
    if(!inherits(recal.model.2, "enmtools.recalibrated.model")){
      stop("recal.model.2 is not an enmtools.recalibrated.model object!")
    }
  }

  # These two are tracking whether we have good enough starting conditions
  # and how many times we've tried
  continue <- FALSE
  n.reps <- 1

  # Some of the DM and BC models were barfing on certain starting conditions
  # so I've put this bit in here to make it try a few times.
  while(continue == FALSE & n.reps < max.reps){

    gens <- chunk.size

    pred1 <- NA
    pred2 <- NA
    pred1.recal <- NA
    pred2.recal <- NA

    # Draw a starting latin hypercube scheme
    this.lhs <- randomLHS(chunk.size, length(names(env)))

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
      if(inherits(model.1, "ranger")) {
        pred1 <- as.numeric(predict(model.1, data = data.frame(predict.table), type = "response")$predictions[ , 2, drop = TRUE])
      } else {
        pred1 <- as.numeric(predict(model.1, newdata = data.frame(predict.table), type = "response"))
      }
    }

    if(inherits(model.2, "DistModel")){
      pred2 <- as.numeric(predict(model.2, x = data.frame(predict.table), type = "response"))
    } else {
      if(inherits(model.2, "ranger")) {
        pred2 <- as.numeric(predict(model.2, data = data.frame(predict.table), type = "response")$predictions[ , 2, drop = TRUE])
      } else {
        pred2 <- as.numeric(predict(model.2, newdata = data.frame(predict.table), type = "response"))
      }
    }

    # Have to do this because rf is producing tiny negative suitabilities for some
    # jackass reason
    pred1[pred1 < 0] <- 0
    pred2[pred2 < 0] <- 0

    if(verbose == TRUE){message(paste("Trying to find starting conditions, attempt", n.reps))}

    if(sd(pred1) == 0 | sd(pred2) == 0){
      n.reps <- n.reps + 1
      next
    }

    # RECALIBRATED MODELS: The trick here is to set this up so that it works whether
    # one or both models are recalibrated

    recal.this.d <- list()
    recal.this.i <- list()
    recal.this.cor <- list()

    # First we'll do it if both are recalibrated
    if(!all(is.na(recal.model.1)) & !all(is.na(recal.model.2))){

      check.packages("CalibratR")

      recal.names <- intersect(names(recal.model.1$recalibrated.model$predictions),
                               names(recal.model.2$recalibrated.model$predictions))

      recal.pred1 <- CalibratR::predict_calibratR(recal.model.1$recalibrated.model$calibration_models, pred1)
      recal.pred2 <- CalibratR::predict_calibratR(recal.model.2$recalibrated.model$calibration_models, pred2)

      for(i in recal.names){
        recal.pred1[[i]][recal.pred1[[i]] < 0] <- 0
        recal.pred2[[i]][recal.pred2[[i]] < 0] <- 0

        recal.this.d[[i]] <- 1 - sum(abs(recal.pred1[[i]]/sum(recal.pred1[[i]]) - recal.pred2[[i]]/(sum(recal.pred2[[i]]))))/2
        recal.this.i[[i]]<- 1 - sum((sqrt(recal.pred1[[i]]/sum(recal.pred1[[i]])) - sqrt(recal.pred2[[i]]/sum(recal.pred2[[i]])))**2)/2
        recal.this.cor[[i]] <- cor(recal.pred1[[i]], recal.pred2[[i]], method = cor.method)
      }
    }

    # Now if just model 1 is recalibrated
    if(!all(is.na(recal.model.1)) & all(is.na(recal.model.2))){

      check.packages("CalibratR")

      recal.names <- names(recal.model.1$recalibrated.model$predictions)

      recal.pred1 <- CalibratR::predict_calibratR(recal.model.1$recalibrated.model$calibration_models, pred1)
      recal.pred2 <- pred2

      for(i in recal.names){
        recal.pred1[[i]][recal.pred1[[i]] < 0] <- 0

        recal.this.d[[i]] <- 1 - sum(abs(recal.pred1[[i]]/sum(recal.pred1[[i]]) - recal.pred2/(sum(recal.pred2))))/2
        recal.this.i[[i]]<- 1 - sum((sqrt(recal.pred1[[i]]/sum(recal.pred1[[i]])) - sqrt(recal.pred2/sum(recal.pred2)))**2)/2
        recal.this.cor[[i]] <- cor(recal.pred1[[i]], recal.pred2, method = cor.method)
      }
    }


    # Now if just model 2 is recalibrated
    if(all(is.na(recal.model.1)) & !all(is.na(recal.model.2))){

      check.packages("CalibratR")

      recal.names <- names(recal.model.2$recalibrated.model$predictions)

      recal.pred1 <- pred1
      recal.pred2 <- CalibratR::predict_calibratR(recal.model.2$recalibrated.model$calibration_models, pred2)

      for(i in recal.names){
        recal.pred2[[i]][recal.pred2[[i]] < 0] <- 0

        recal.this.d[[i]] <- 1 - sum(abs(recal.pred1/sum(recal.pred1) - recal.pred2[[i]]/(sum(recal.pred2[[i]]))))/2
        recal.this.i[[i]]<- 1 - sum((sqrt(recal.pred1/sum(recal.pred1)) - sqrt(recal.pred2[[i]]/sum(recal.pred2[[i]])))**2)/2
        recal.this.cor[[i]] <- cor(recal.pred1, recal.pred2[[i]], method = cor.method)
      }
    }


    # Now for the main models (i.e., not recalibrated)
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
    warning("\n\nCould not find suitable starting conditions for environmental overlap, returning NA\n\n")
    return(list(env.D = NA,
                env.I = NA,
                env.cor = NA,
                env.D.plot = NA,
                env.I.plot = NA,
                env.cor.plot = NA))
  } else {

    # So here we've got good starting conditions and we're going to keep going
    # with the LHS design until we get a minimum difference between subsequent
    # samples (delta < tolerance).  We're going to diagnose convergence just based
    # on the main models.

    if(verbose == TRUE){message("Building replicates...")}

    delta <- 1

    # print(paste(this.diff, delta))

    while(delta > tolerance){

      # print(max(gens))

      # Add chunk.size rows to the LHS and build a new predict table
      this.lhs <- randomLHS(chunk.size, length(names(env)))
      predict.table <- t(t(this.lhs) * (maxes  - mins) + mins)
      colnames(predict.table) <- names(env)

      if(inherits(model.1, "DistModel")){
        pred1 <- as.numeric(predict(model.1, x = data.frame(predict.table), type = "response"))
      } else {
        if(inherits(model.1, "ranger")) {
          pred1 <- as.numeric(predict(model.1, data = data.frame(predict.table), type = "response")$predictions[ , 2, drop = TRUE])
        } else {
          pred1 <- as.numeric(predict(model.1, newdata = data.frame(predict.table), type = "response"))
        }
      }

      if(inherits(model.2, "DistModel")){
        pred2 <- as.numeric(predict(model.2, x = data.frame(predict.table), type = "response"))
      } else {
        if(inherits(model.2, "ranger")) {
          pred2 <- as.numeric(predict(model.2, data = data.frame(predict.table), type = "response")$predictions[ , 2, drop = TRUE])
        } else {
          pred2 <- as.numeric(predict(model.2, newdata = data.frame(predict.table), type = "response"))
        }
      }

      pred1[pred1 < 0] <- 0
      pred2[pred2 < 0] <- 0

      if(sd(pred1) == 0 | sd(pred2) == 0){
        next
      } else {
        gens <- c(gens, max(gens) + chunk.size)
      }

      # RECALIBRATED MODELS: The trick here is to set this up so that it works whether
      # one or both models are recalibrated

      # First we'll do it if both are recalibrated
      if(!all(is.na(recal.model.1)) & !all(is.na(recal.model.2))){

        check.packages("CalibratR")

        recal.names <- intersect(names(recal.model.1$recalibrated.model$predictions),
                                 names(recal.model.2$recalibrated.model$predictions))

        recal.pred1 <- CalibratR::predict_calibratR(recal.model.1$recalibrated.model$calibration_models, pred1)
        recal.pred2 <- CalibratR::predict_calibratR(recal.model.2$recalibrated.model$calibration_models, pred2)

        for(i in recal.names){
          recal.pred1[[i]][recal.pred1[[i]] < 0] <- 0
          recal.pred2[[i]][recal.pred2[[i]] < 0] <- 0

          recal.this.d[[i]] <- c(recal.this.d[[i]], 1 - sum(abs(recal.pred1[[i]]/sum(recal.pred1[[i]]) - recal.pred2[[i]]/(sum(recal.pred2[[i]]))))/2)
          recal.this.i[[i]]<- c(recal.this.i[[i]], 1 - sum((sqrt(recal.pred1[[i]]/sum(recal.pred1[[i]])) - sqrt(recal.pred2[[i]]/sum(recal.pred2[[i]])))**2)/2)
          recal.this.cor[[i]] <- c(recal.this.cor[[i]], cor(recal.pred1[[i]], recal.pred2[[i]], method = cor.method))
        }
      }

      # Now if just model 1 is recalibrated
      if(!all(is.na(recal.model.1)) & all(is.na(recal.model.2))){

        check.packages("CalibratR")

        recal.names <- names(recal.model.1$recalibrated.model$predictions)

        recal.pred1 <- CalibratR::predict_calibratR(recal.model.1$recalibrated.model$calibration_models, pred1)
        recal.pred2 <- pred2

        for(i in recal.names){
          recal.pred1[[i]][recal.pred1[[i]] < 0] <- 0

          recal.this.d[[i]] <- c(recal.this.d[[i]], 1 - sum(abs(recal.pred1[[i]]/sum(recal.pred1[[i]]) - recal.pred2/(sum(recal.pred2))))/2)
          recal.this.i[[i]]<- c(recal.this.i[[i]], 1 - sum((sqrt(recal.pred1[[i]]/sum(recal.pred1[[i]])) - sqrt(recal.pred2/sum(recal.pred2)))**2)/2)
          recal.this.cor[[i]] <- c(recal.this.cor[[i]], cor(recal.pred1[[i]], recal.pred2, method = cor.method))
        }
      }


      # Now if just model 2 is recalibrated
      if(all(is.na(recal.model.1)) & !all(is.na(recal.model.2))){

        check.packages("CalibratR")

        recal.names <- names(recal.model.2$recalibrated.model$predictions)

        recal.pred1 <- pred1
        recal.pred2 <- CalibratR::predict_calibratR(recal.model.2$recalibrated.model$calibration_models, pred2)

        for(i in recal.names){
          recal.pred2[[i]][recal.pred2[[i]] < 0] <- 0

          recal.this.d[[i]] <- c(recal.this.d[[i]], 1 - sum(abs(recal.pred1/sum(recal.pred1) - recal.pred2[[i]]/(sum(recal.pred2[[i]]))))/2)
          recal.this.i[[i]]<- c(recal.this.i[[i]], 1 - sum((sqrt(recal.pred1/sum(recal.pred1)) - sqrt(recal.pred2[[i]]/sum(recal.pred2[[i]])))**2)/2)
          recal.this.cor[[i]] <- c(recal.this.cor[[i]], cor(recal.pred1, recal.pred2[[i]], method = cor.method))
        }
      }

      # We're going to use this n so we can just do a weighted average of our D/I/cor
      # instead of concatenating pred1 and pred2
      n <- length(this.d)

      old.d <- this.d[n]
      new.d <- 1 - sum(abs(pred1/sum(pred1) - pred2/(sum(pred2))))/2
      old.i <- this.i[n]
      new.i <- 1 - sum((sqrt(pred1/sum(pred1)) - sqrt(pred2/sum(pred2)))**2)/2

      this.d <- c(this.d, old.d * (n/(n+1)) + new.d * 1/(n+1))
      this.i <- c(this.i, old.i * (n/(n+1)) + new.i * 1/(n+1))
      if(sd(pred1) == 0 | sd(pred2) == 0){
        this.cor <- c(this.cor, NA)
      } else {
        old.cor <- this.cor[n]
        n <- n - length(which(is.na(this.cor)))
        new.cor <- cor(pred1, pred2, method = cor.method)
        this.cor <- c(this.cor, old.cor * (n/(n+1)) + new.cor * 1/(n+1))
      }

      # We're not bothering with the above for recalibrated models because
      # we're not using them to diagnose convergence - we'll take a moving
      # average at the end.


      # Calculate delta for this iteration
      delta <- max(c(abs(mean(this.d) - mean(this.d[-length(this.d)])),
                     abs(mean(this.i) - mean(this.i[-length(this.i)])),
                     abs(mean(this.cor) - mean(this.cor[-length(this.cor)]))), na.rm=TRUE)
      #print(delta)
    }
  }

  output <- NA

  # Packing list for non-recalibrated models
  if(all(is.na(recal.model.1)) & all(is.na(recal.model.2))){
    output <- list(env.D = mean(this.d),
                   env.I = mean(this.i),
                   env.cor = mean(this.cor),
                   env.D.plot = qplot(gens, this.d, ylab = "D", xlab = "Samples", ylim = c(0,1)),
                   env.I.plot = qplot(gens, this.i, ylab = "I", xlab = "Samples", ylim = c(0,1)),
                   env.cor.plot = qplot(gens, this.cor, ylab = "Correlation", xlab = "Samples", ylim = c(-1,1)))
  } else {
    # At least one of the models was recalibrated
    output <- list(env.D = mean(this.d),
                   env.I = mean(this.i),
                   env.cor = mean(this.cor),
                   env.D.plot = qplot(gens, this.d, ylab = "D", xlab = "Samples", ylim = c(0,1)),
                   env.I.plot = qplot(gens, this.i, ylab = "I", xlab = "Samples", ylim = c(0,1)),
                   env.cor.plot = qplot(gens, this.cor, ylab = "Correlation", xlab = "Samples", ylim = c(-1,1)),
                   recal.env.D = lapply(recal.this.d, function(x) mean(x)),
                   recal.env.i = lapply(recal.this.i, function(x) mean(x)),
                   recal.env.cor = lapply(recal.this.cor, function(x) mean(x)))
  }


  return(output)
}


