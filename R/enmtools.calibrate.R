#' Takes an emtools.model object, and reformats it to run through the CalibratR package, calculates Continuous Boyce Index, and runs a Hosmer-Lemeshow goodness-of-fit test.  Can either do a full CalibratR run or just return ECE/MCE statistics and plots.
#'
#' @param model An enmtools.model object
#' @param recalibrate When TRUE, does a full CalibratR "calibrate" run to recalibrate the model.  When FALSE, just returns metrics and plots measuring calibration of the model as is.
#' @param cuts The number of bins to split suitability scores into for calculating calibration.
#' @param ... Further arguments to be passed to CalibratR's "calibrate" function.
#'
#' @examples
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' monticola.glm <- enmtools.glm(iberolacerta.clade$species$monticola,
#' env = euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.3)
#' enmtools.calibrate(monticola.glm)

enmtools.calibrate <- function(model, recalibrate = FALSE, cuts = 11, ...){

  if(suppressWarnings(is.na(model$test.evaluation))){
    stop("No test evaluation data available, cannot measure calibration.")
  }

  pred.df <- data.frame()

  # The following bits of code basically just turn each model type's
  # output into probabilities.

  if(inherits(model, "enmtools.glm") | inherits(model, "enmtools.gam")){

    # For models that need to be transformed from logit to probabilities
    p <- exp(model$test.evaluation@presence)/(1 + exp(model$test.evaluation@presence))
    a <- exp(model$test.evaluation@absence)/(1 + exp(model$test.evaluation@absence))

    # Pack it all up
    pred.df <- data.frame(prob = c(p, a),
                          obs = c(rep("presence", length(p)), rep("absence", length(a))))
  } else if(inherits(model, "enmtools.bc") | inherits(model, "enmtools.dm") | inherits(model, "enmtools.maxent") | inherits(model, "enmtools.rf")){

    # For models that already have probabilities
    p <- model$test.evaluation@presence
    a <- model$test.evaluation@absence

    pred.df <- data.frame(prob = c(p, a),
                          obs = c(rep("presence", length(p)), rep("absence", length(a))))
  } else {
    stop("Calibration not implemented yet for this model type.")
  }

  # Get a calibration data frame from caret for plots etc.
  calib <- caret::calibration(obs ~ prob, data = pred.df, class = "presence", cuts = cuts)

  calib.plot <- qplot(calib$data$midpoint, calib$data$Percent,
                      geom = c("line", "point"), xlim = c(0, 100), ylim = c(0, 100),
                      xlab = "Predicted", ylab = "Observed") +
    geom_abline(intercept = 0, slope = 1, linetype = 3)

  class.plot <- qplot(pred.df$prob, facets = obs ~ ., data = pred.df,
                      alpha = 0.5, ylab = "Count", xlab = "Predicted")

  # Need to convert obs to 1/0 for hoslem test and calibrate function
  this.pa <- rep(NA, length(pred.df$obs))
  this.pa[which(pred.df$obs == "presence")] <- 1
  this.pa[which(pred.df$obs == "absence")] <- 0
  hoslem <- hoslem.test(this.pa, pred.df$prob, g = cuts)

  ECE <- getECE(this.pa, pred.df$prob, n_bins = cuts)
  ECE.equal.width <- get_ECE_equal_width(this.pa, pred.df$prob)
  MCE <- getMCE(this.pa, pred.df$prob, n_bins = cuts)
  MCE.equal.width <- get_MCE_equal_width(this.pa, pred.df$prob)

  # Testing to see whether models are presence only or presence/background
  continuous.boyce <- NA
  if("presence" %in% colnames(model$analysis.df)){
    continuous.boyce <- ecospat.boyce(model$suitability,
                                      model$analysis.df[model$analysis.df$presence == 1,1:2])
  } else {
    continuous.boyce <- ecospat.boyce(model$suitability,
                                      model$analysis.df[,1:2])
  }

  # Recalibrating as needed
  recalibrated.model <- NA
  calibrated.suitabilities <- NA
  recalibrated.metrics <- list()
  recalibrated.plots <- list()

  if(recalibrate == TRUE){
    recalibrated.model <- CalibratR::calibrate(this.pa, pred.df$prob, evaluate_no_CV_error = FALSE)
    preds <- raster::rasterToPoints(model$suitability)
    cal.preds <- CalibratR::predict_calibratR(recalibrated.model$calibration_models, preds[,"layer"])
    calibrated.suitabilities <- lapply(cal.preds, function(x) rasterize(preds[,1:2], model$suitability, field = x))


    for(i in names(recalibrated.model$summary_CV$models$uncalibrated)){

      recalibrated.metrics[[i]][["ECE"]] <- mean(sapply(recalibrated.model$summary_CV$models$uncalibrated[[i]], function(x) x$error$calibration_error$ECE_equal_freq))
      recalibrated.metrics[[i]][["ECE.equal.width"]] <- mean(sapply(recalibrated.model$summary_CV$models$uncalibrated[[i]], function(x) x$error$calibration_error$ECE_equal_width))
      recalibrated.metrics[[i]][["MCE"]] <- mean(sapply(recalibrated.model$summary_CV$models$uncalibrated[[i]], function(x) x$error$calibration_error$MCE_equal_freq))
      recalibrated.metrics[[i]][["MCE.equal.width"]] <- mean(sapply(recalibrated.model$summary_CV$models$uncalibrated[[i]], function(x) x$error$calibration_error$MCE_equal_width))

      temp.df <- data.frame(prob = recalibrated.model$predictions[[i]],
                            obs = pred.df$obs)

      recalibrated.plots[[i]][["class.plot"]] <-   class.plot <- qplot(temp.df$prob, facets = obs ~ ., data = temp.df,
                                                                       alpha = 0.5, ylab = "Count", xlab = "Predicted")
    }

    for(i in names(recalibrated.model$summary_CV$models$calibrated)){

      recalibrated.metrics[[i]][["ECE"]] <- mean(sapply(recalibrated.model$summary_CV$models$calibrated[[i]], function(x) x$error$calibration_error$ECE_equal_freq))
      recalibrated.metrics[[i]][["ECE.equal.width"]] <- mean(sapply(recalibrated.model$summary_CV$models$calibrated[[i]], function(x) x$error$calibration_error$ECE_equal_width))
      recalibrated.metrics[[i]][["MCE"]] <- mean(sapply(recalibrated.model$summary_CV$models$calibrated[[i]], function(x) x$error$calibration_error$MCE_equal_freq))
      recalibrated.metrics[[i]][["MCE.equal.width"]] <- mean(sapply(recalibrated.model$summary_CV$models$calibrated[[i]], function(x) x$error$calibration_error$MCE_equal_width))

      temp.df <- data.frame(prob = recalibrated.model$predictions[[i]],
                            obs = pred.df$obs)

      recalibrated.plots[[i]][["class.plot"]] <-   class.plot <- qplot(temp.df$prob, facets = obs ~ ., data = temp.df,
                                                                      alpha = 0.5, ylab = "Count", xlab = "Predicted")
    }
  }

  output <- list(calibration.plot = calib.plot,
                 classification.plot = class.plot,
                 ECE = ECE,
                 ECE.equal.width = ECE.equal.width,
                 MCE = MCE,
                 MCE.equal.width = MCE.equal.width,
                 continuous.boyce = continuous.boyce,
                 hoslem = hoslem,
                 recalibrated.model = recalibrated.model,
                 calibrated.suitabilities = calibrated.suitabilities,
                 recalibrated.metrics = recalibrated.metrics,
                 recalibrated.plots = recalibrated.plots)

  return(output)
}



# This code comes from the CalibratR package, and is being copy/pasted because it's not exported by the original package.
getECE <- function(actual, predicted, n_bins=10){ #equal frequency bins

  predicted <- predicted
  labels <- actual
  idx <- order(predicted)
  pred_actual <- (cbind(predicted[idx], labels[idx]))

  N <- nrow(pred_actual)
  rest <- N%%n_bins
  S <- 0
  W <- c()
  B <- min(N,n_bins) #if less then n_bins elements in data set, then use that number of bins
  groups <- list()

  for (i in 1:B){ #i von 1 bis B
    if (i <= rest){ #put rest elements into each bin
      group_pred <- (pred_actual[(((i-1)*ceiling(N/n_bins)+1) : (i*ceiling(N/n_bins))),1])
      group_actual <- (pred_actual[(((i-1)*ceiling(N/n_bins)+1) : (i*ceiling(N/n_bins))),2])
    }
    else {
      group_pred <- (pred_actual[((rest+(i-1)*floor(N/n_bins)+1) : (rest+i*floor(N/n_bins))),1])#group size=N/B
      group_actual <- (pred_actual[((rest+(i-1)*floor(N/n_bins)+1) : (rest+i*floor(N/n_bins))),2])
    }

    n_ <- length(group_pred)
    expected <- mean(group_pred) #mean of predictions in bin b
    observed <- mean(group_actual) #true fraction of pos.instances = prevalence in bin b

    S[i] <- abs(observed-expected) #absolut difference of observed value-predicted value in bin
    W[i] <- n_/N #empirical frequence of all instances that fall into bin i, should be equal when using equal freq binning approach
    groups[[i]] <- group_pred

  }

  mean_prediction <- lapply(groups, mean)
  min_group <- lapply(groups, min)
  max_group <- lapply(groups, max)

  res <- t(S)%*%W
  return(as.numeric(res))
}


# This code comes from the CalibratR package, and is being copy/pasted because it's not exported by the original package.
get_ECE_equal_width <- function(actual, predicted, bins=10){ #equal width bins

  pred_actual <- cbind(predicted, actual)

  if(all(predicted<=1) && all(predicted>=0)){
    hist_x <- graphics::hist(pred_actual[,1], breaks=seq(0,1,1/bins), plot=F)
  }
  else{
    hist_x <- graphics::hist(pred_actual[,1], breaks=bins, plot=F)
  }

  breaks_y <- hist_x$breaks
  y_true <- graphics::hist(subset(pred_actual[,1], pred_actual[,2]=="1"), breaks=breaks_y, plot=F)
  divided <- cut(pred_actual[,1], breaks=c(hist_x$breaks), label = seq(1,length(y_true$mids)), include.lowest = T)
  prediction_in_bin <- list()
  expected <- c()

  for (i in as.numeric(levels(divided))){
    prediction_in_bin[[i]] <- pred_actual[which(divided==i),1]
    expected[i] <- mean(prediction_in_bin[[i]]) #mean prediction in that bin
    #expected[i] <- hist_x$mids[i] #hist mids as mean prediction in that bin
  }


  counts_all <- hist_x$counts
  counts_true <- y_true$counts
  zeros <- which(counts_all==0)

  prevalence <- counts_true/counts_all
  prevalence[zeros] <- 0 #set prevalence to 0 when no observations are in the bin
  expected[zeros] <- hist_x$mids[zeros] #set expectation to the mid bin point, when no elements are in bin

  S_2 <- abs(prevalence-expected)
  W_2 <- counts_all/(length(predicted))


  return(as.numeric(t(S_2)%*%W_2))
}

# This code comes from the CalibratR package, and is being copy/pasted because it's not exported by the original package.
getMCE <- function(actual, predicted, n_bins=10){

  predicted <- predicted
  labels <- actual
  idx <- order(predicted)
  pred_actual <- (cbind(predicted[idx], actual[idx]))
  N <- nrow(pred_actual)
  rest <- N%%n_bins
  B <- min(N,n_bins)

  S <- 0
  W <- c()
  for (i in 1:B){ #i von 1 bis B
    if (i <= rest){ #put rest elements into each bin
      group_pred <- (pred_actual[(((i-1)*ceiling(N/n_bins)+1) : (i*ceiling(N/n_bins))),1])
      group_actual <- (pred_actual[(((i-1)*ceiling(N/n_bins)+1) : (i*ceiling(N/n_bins))),2])
    }
    else {
      group_pred <- (pred_actual[((rest+(i-1)*floor(N/n_bins)+1) : (rest+i*floor(N/n_bins))),1])#group size=N/B
      group_actual <- (pred_actual[((rest+(i-1)*floor(N/n_bins)+1) : (rest+i*floor(N/n_bins))),2])
    }

    n <- length(group_pred)
    expected <- mean(group_pred) #mean of predictions in bin b
    observed <- mean(group_actual) #true fraction of pos.instances = prevalence in bin b

    S[i] <- abs(observed-expected) #absolut difference of observed value-predicted value in bin
    W[i] <- n/N #empirical frequence of all instances that fall into bin i, should be pretty much the same among all bins
  }

  res <- max(S*W)
  return(res)
}

# This code comes from the CalibratR package, and is being copy/pasted because it's not exported from that package.
get_MCE_equal_width <- function(actual, predicted, bins=10){ #equal width bins

  predicted <- predicted
  labels <- actual
  idx <- order(predicted)
  pred_actual <- (cbind(predicted[idx], labels[idx]))

  hist_x <- graphics::hist(pred_actual[,1],breaks=bins, plot=F)
  breaks_y <- hist_x$breaks
  y_true <- graphics::hist(subset(pred_actual[,1], pred_actual[,2]=="1"),breaks=breaks_y, plot=F)
  divided <- cut(pred_actual[,1], breaks=c(hist_x$breaks),label = seq(1,length(y_true$mids)),include.lowest = T)
  prediction_in_bin <- list()
  expected <- c()

  for (i in as.numeric(levels(divided))){
    prediction_in_bin[[i]] <- pred_actual[which(divided==i),1]
    #expected[i] <- hist_x$mids[i] #mean prediction in that bin
    expected[i] <- mean(pred_actual[which(divided==i),1]) #mean prediction in that bin
  }

  counts_all <- hist_x$counts
  counts_true <- y_true$counts
  zeros <- which(counts_all==0)

  prevalence <- counts_true/counts_all
  prevalence[zeros] <- 0 #set prevalence to 0 when no observations are in the bin
  expected[zeros] <- hist_x$mids[zeros] #set expectation to the mid bin point, when no elements are in bin

  S_2 <- abs(prevalence-expected)
  W_2 <- counts_all/(length(predicted))
  return(max(S_2*W_2))
}
