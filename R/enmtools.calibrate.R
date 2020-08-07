#' Takes an emtools.model object, and reformats it to run through the CalibratR package, calculates Continuous Boyce Index, and runs a Hosmer-Lemeshow goodness-of-fit test.  Can either do a full CalibratR run or just return ECE/MCE statistics and plots.
#'
#' @param model An enmtools.model object
#' @param recalibrate When TRUE, does a full CalibratR "calibrate" run to recalibrate the model.  When FALSE, just returns metrics and plots measuring calibration of the model as is.
#' @param cuts The number of bins to split suitability scores into for calculating calibration.
#' @param env A set of environment layers to be used for optional env space metrics
#' @param n.background Number of background points to be used for env space metrics
#' @param ... Further arguments to be passed to CalibratR's "calibrate" function.
#'
#' @return An enmtools.calibrate object containing calibration and classificaction plots, metrics of model calibration, and (optionally) versions of the model that were recalibrated using various methods.
#'
#' @examples
#' \donttest{
#' install.extras(repos='http://cran.us.r-project.org')
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' monticola.glm <- enmtools.glm(iberolacerta.clade$species$monticola,
#'                               env = euro.worldclim,
#'                               f = pres ~ bio1 + bio9,
#'                               test.prop = 0.3)
#' enmtools.calibrate(monticola.glm)
#' }

enmtools.calibrate <- function(model, recalibrate = FALSE, cuts = 11, env = NA, n.background = 10000, ...){

  check.packages(c("ecospat", "CalibratR", "caret", "ResourceSelection"))

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
    train.p <- exp(model$training.evaluation@presence)/(1 + exp(model$training.evaluation@presence))

    # Subsampling background test data to get same prevalence as we had with training data
    a <- sample(a, size = model$test.prop * length(a), replace = FALSE)

    # Pack it all up
    pred.df <- data.frame(prob = c(p, a),
                          obs = c(rep("presence", length(p)), rep("absence", length(a))))
  } else if(inherits(model, "enmtools.bc") | inherits(model, "enmtools.dm") | inherits(model, "enmtools.maxent") | inherits(model, "enmtools.rf")){

    # For models that already have probabilities
    p <- model$test.evaluation@presence
    a <- model$test.evaluation@absence

    # Subsampling background test data to get same prevalence as we had with training data
    a <- sample(a, size = model$test.prop * length(a), replace = FALSE)

    train.p <- model$training.evaluation@presence

    pred.df <- data.frame(prob = c(p, a),
                          obs = c(rep("presence", length(p)), rep("absence", length(a))))
  } else {
    stop("Calibration not implemented yet for this model type.")
  }

  if(!is.factor(pred.df$obs)){
    pred.df$obs <- as.factor(pred.df$obs)
  }

  # Get a calibration data frame from caret for plots etc.
  calib <- caret::calibration(obs ~ prob, data = pred.df, class = "presence", cuts = cuts)

  this.calib.plot <- calib.plot(pred.df$prob, pred.df$obs, "Uncalibrated", cuts = cuts)

  this.class.plot <- class.plot(pred.df$prob, pred.df$obs, "Uncalibrated", cuts = cuts)

  # Need to convert obs to 1/0 for hoslem test and calibrate function
  this.pa <- rep(NA, length(pred.df$obs))
  this.pa[which(pred.df$obs == "presence")] <- 1
  this.pa[which(pred.df$obs == "absence")] <- 0
  hoslem <- ResourceSelection::hoslem.test(this.pa, pred.df$prob, g = cuts)

  ECE <- CalibratR::getECE(this.pa, pred.df$prob, n_bins = cuts)
  ECE.equal.width <- get_ECE_equal_width(this.pa, pred.df$prob)
  MCE <- CalibratR::getMCE(this.pa, pred.df$prob, n_bins = cuts)
  MCE.equal.width <- get_MCE_equal_width(this.pa, pred.df$prob)

  # Testing to see whether models are presence only or presence/background
  continuous.boyce <- NA
  if("presence" %in% colnames(model$analysis.df)){
    continuous.boyce <- ecospat::ecospat.boyce(model$suitability,
                                      model$test.data, PEplot = FALSE)
  } else {
    continuous.boyce <- ecospat::ecospat.boyce(model$suitability,
                                      model$test.data, PEplot = FALSE)
  }

  # Recalibrating as needed
  recalibrated.model <- NA
  calibrated.suitabilities <- NA
  recalibrated.metrics <- list()
  recalibrated.plots <- list()
  env.training.evaluation <- list()
  env.test.evaluation <- list()

  if(recalibrate == TRUE){
    recalibrated.model <- CalibratR::calibrate(this.pa, pred.df$prob, evaluate_no_CV_error = FALSE, ...)
    preds <- raster::rasterToPoints(model$suitability)
    cal.preds <- CalibratR::predict_calibratR(recalibrated.model$calibration_models, preds[,"layer"])

    # The Phillips and Elith recalibration should be done here for presence only models
    # Should make it optional with a flag that defaults to TRUE
    # transformation is (mean untransformed suitability * suitability in grid cell)/(prevalence * (1 - suitability in grid cell))
    calibrated.suitabilities <- lapply(cal.preds, function(x) rasterize(preds[,1:2], model$suitability, field = x))

    class.plots.1 <- lapply(names(recalibrated.model$summary_CV$models$uncalibrated), function(x) class.plot(recalibrated.model$predictions[[x]], pred.df$obs, x, cuts = cuts))
    class.plots.2 <- lapply(names(recalibrated.model$summary_CV$models$calibrated), function(x) class.plot(recalibrated.model$predictions[[x]], pred.df$obs, x, cuts = cuts))
    names(class.plots.1) <- names(recalibrated.model$summary_CV$models$uncalibrated)
    names(class.plots.2) <- names(recalibrated.model$summary_CV$models$calibrated)
    recalibrated.plots[["classification.plots"]] <- c(class.plots.1, class.plots.2)

    calib.plots.1 <- lapply(names(recalibrated.model$summary_CV$models$uncalibrated), function(x) calib.plot(recalibrated.model$predictions[[x]], pred.df$obs, x, cuts = cuts))
    calib.plots.2 <- lapply(names(recalibrated.model$summary_CV$models$calibrated), function(x) calib.plot(recalibrated.model$predictions[[x]], pred.df$obs, x, cuts = cuts))
    names(calib.plots.1) <- names(recalibrated.model$summary_CV$models$uncalibrated)
    names(calib.plots.2) <- names(recalibrated.model$summary_CV$models$calibrated)
    recalibrated.plots[["calibration.plots"]] <- c(calib.plots.1, calib.plots.2)

    # Do env space discrim metrics
    if(inherits(env, c("raster", "RasterBrick", "RasterStack"))){

      allpoints <- rbind(model$analysis.df[,1:2], model$test.data)
      values <- raster::extract(env, allpoints)
      maxes <- apply(values, 2, function(x) max(x, na.rm = TRUE))
      mins <- apply(values, 2, function(x) min(x, na.rm = TRUE))

      this.lhs <- lhs::randomLHS(n.background, length(names(env)))
      bg.table <- t(t(this.lhs) * (maxes  - mins) + mins)
      colnames(bg.table) <- names(env)

      p.table <- raster::extract(env, model$analysis.df[model$analysis.df$presence == 1,1:2])
      test.table <- raster::extract(env, model$test.data)

      # Having to do this for now because the dismo models don't like "newdata"
      # Unfortunately I think we finally have to use an if statement because ranger predict is really different
      if(inherits(model, "ranger")) {
        pred.p <- as.numeric(predict(model$model, data = data.frame(p.table), type = "response")$predictions[ , 2, drop = TRUE])
        pred.bg <- as.numeric(predict(model$model, data = data.frame(bg.table), type = "response")$predictions[ , 2, drop = TRUE])
        pred.test <- as.numeric(predict(model$model, data = data.frame(test.table), type = "response")$predictions[ , 2, drop = TRUE])
      } else {
        pred.p <- as.numeric(predict(model$model, newdata = data.frame(p.table), x = data.frame(p.table), type = "response"))
        pred.bg <- as.numeric(predict(model$model, newdata = data.frame(bg.table), x = data.frame(bg.table), type = "response"))
        pred.test <- as.numeric(predict(model$model, newdata = data.frame(test.table), x = data.frame(test.table), type = "response"))
      }

      pred.p <- CalibratR::predict_calibratR(recalibrated.model$calibration_models, pred.p)
      pred.bg <- CalibratR::predict_calibratR(recalibrated.model$calibration_models, pred.bg)
      pred.test <- CalibratR::predict_calibratR(recalibrated.model$calibration_models, pred.test)

      for(j in names(pred.p)){
        env.training.evaluation[[j]] <- dismo::evaluate(pred.p[[j]], pred.bg[[j]])
      }

      for(j in names(pred.test)){
        env.test.evaluation[[j]] <- dismo::evaluate(pred.test[[j]], pred.bg[[j]])
      }

    }

    # Doing recalibration on data from model object (as opposed to whole env)
    recal.test.p <- CalibratR::predict_calibratR(recalibrated.model$calibration_models, as.numeric(p))
    recal.train.p <- CalibratR::predict_calibratR(recalibrated.model$calibration_models, as.numeric(train.p))
    recal.train.a <- CalibratR::predict_calibratR(recalibrated.model$calibration_models, as.numeric(a))



    # We have to go through this twice because calibrated and uncalibrated models are stored separately
    for(i in names(recalibrated.model$summary_CV$models$uncalibrated)){

      recalibrated.metrics[[i]] <- list()

      # ECE and MCE
      recalibrated.metrics[[i]][["ECE"]] <- mean(sapply(recalibrated.model$summary_CV$models$uncalibrated[[i]], function(x) x$error$calibration_error$ECE_equal_freq))
      recalibrated.metrics[[i]][["ECE.equal.width"]] <- mean(sapply(recalibrated.model$summary_CV$models$uncalibrated[[i]], function(x) x$error$calibration_error$ECE_equal_width))
      recalibrated.metrics[[i]][["MCE"]] <- mean(sapply(recalibrated.model$summary_CV$models$uncalibrated[[i]], function(x) x$error$calibration_error$MCE_equal_freq))
      recalibrated.metrics[[i]][["MCE.equal.width"]] <- mean(sapply(recalibrated.model$summary_CV$models$uncalibrated[[i]], function(x) x$error$calibration_error$MCE_equal_width))

      # Geo space discrim metrics
      recalibrated.metrics[[i]][["geo.training.evaluation"]] <- dismo::evaluate(recal.train.p[[i]], recal.train.a[[i]])
      recalibrated.metrics[[i]][["geo.test.evaluation"]] <- dismo::evaluate(recal.test.p[[i]], recal.train.a[[i]])

      recalibrated.metrics[[i]][["env.training.evaluation"]] <- env.training.evaluation[[i]]
      recalibrated.metrics[[i]][["env.test.evaluation"]] <- env.test.evaluation[[i]]

      # Boyce is noyce
      # Testing to see whether models are presence only or presence/background
      recalibrated.metrics[[i]][["continuous.boyce"]] <- NA
      if("presence" %in% colnames(model$analysis.df)){
        recalibrated.metrics[[i]][["continuous.boyce"]]  <- ecospat::ecospat.boyce(calibrated.suitabilities[[i]],
                                          model$test.data, PEplot = FALSE)
      } else {
        recalibrated.metrics[[i]][["continuous.boyce"]]  <- ecospat::ecospat.boyce(calibrated.suitabilities[[i]],
                                          model$test.data, PEplot = FALSE)
      }

    }

    # The code so nice I used it twice
    for(i in names(recalibrated.model$summary_CV$models$calibrated)){

      recalibrated.metrics[[i]] <- list()

      # ECE and MCE
      recalibrated.metrics[[i]][["ECE"]] <- mean(sapply(recalibrated.model$summary_CV$models$calibrated[[i]], function(x) x$error$calibration_error$ECE_equal_freq))
      recalibrated.metrics[[i]][["ECE.equal.width"]] <- mean(sapply(recalibrated.model$summary_CV$models$calibrated[[i]], function(x) x$error$calibration_error$ECE_equal_width))
      recalibrated.metrics[[i]][["MCE"]] <- mean(sapply(recalibrated.model$summary_CV$models$calibrated[[i]], function(x) x$error$calibration_error$MCE_equal_freq))
      recalibrated.metrics[[i]][["MCE.equal.width"]] <- mean(sapply(recalibrated.model$summary_CV$models$calibrated[[i]], function(x) x$error$calibration_error$MCE_equal_width))

      # Geo space discrim metrics
      recalibrated.metrics[[i]][["geo.training.evaluation"]] <- dismo::evaluate(recal.train.p[[i]], recal.train.a[[i]])
      recalibrated.metrics[[i]][["geo.test.evaluation"]] <- dismo::evaluate(recal.test.p[[i]], recal.train.a[[i]])

      recalibrated.metrics[[i]][["env.training.evaluation"]] <- env.training.evaluation[[i]]
      recalibrated.metrics[[i]][["env.test.evaluation"]] <- env.test.evaluation[[i]]

      # Boyce is noyce
      # Testing to see whether models are presence only or presence/background
      recalibrated.metrics[[i]][["continuous.boyce"]] <- NA
      if("presence" %in% colnames(model$analysis.df)){
        recalibrated.metrics[[i]][["continuous.boyce"]]  <- ecospat::ecospat.boyce(calibrated.suitabilities[[i]],
                                                                          model$test.data, PEplot = FALSE)
      } else {
        recalibrated.metrics[[i]][["continuous.boyce"]]  <- ecospat::ecospat.boyce(calibrated.suitabilities[[i]],
                                                                          model$test.data, PEplot = FALSE)
      }
    }
  }

  output <- list(calibration.plot = this.calib.plot,
                 classification.plot = this.class.plot,
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

  class(output) <- c("enmtools.calibrate")

  if(recalibrate == TRUE){
    class(output) <- c(class(output), "enmtools.recalibrated.model")
  }

  return(output)
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

# Summary for objects of class enmtools.calibrate
summary.enmtools.calibrate <- function(object, ...){

  print(plot(object))

  cat("Calibration metrics for uncalibrated model: ")
  stats.df <- data.frame(ECE = object$ECE,
                         MCE = object$MCE,
                         ECE.equal.width = object$ECE.equal.width,
                         MCE.equal.width = object$MCE.equal.width,
                         boyce.index = object$continuous.boyce$Spearman.cor)
  print(knitr::kable(stats.df))

  print(object$hoslem)

  if(inherits(object, "enmtools.recalibrated.model")){
    metrics.df <- data.frame(Recalibration = names(object$recalibrated.metrics),
                             ECE = unlist(lapply(object$recalibrated.metrics, function(x) x$ECE)),
                             MCE = unlist(lapply(object$recalibrated.metrics, function(x) x$MCE)),
                             ECE.equal.width = unlist(lapply(object$recalibrated.metrics, function(x) x$ECE.equal.width)),
                             MCE.equal.width = unlist(lapply(object$recalibrated.metrics, function(x) x$MCE.equal.width)),
                             boyce.index = unlist(lapply(object$recalibrated.metrics, function(x) x$continuous.boyce$Spearman.cor)))
    print(knitr::kable(metrics.df))


  }

}

# Print method for objects of class enmtools.calibrate
print.enmtools.calibrate <- function(x, ...){

  print(summary(x))

}


# Plot method for objects of class enmtools.calibrate
plot.enmtools.calibrate <- function(x, ...){

  if(inherits(x, "enmtools.recalibrated.model")){

    plotmodel <- function(object, modname){
      # Pack up summary stats
      stats.df <- data.frame(Metric = c("ECE", "MCE", "ECE.equal.width", "MCE.equal.width", "boyce.index"),
                             Value = c(x$recalibrated.metrics[[modname]]$ECE,
                                       x$recalibrated.metrics[[modname]]$MCE,
                                       x$recalibrated.metrics[[modname]]$ECE.equal.width,
                                       x$recalibrated.metrics[[modname]]$MCE.equal.width,
                                       x$recalibrated.metrics[[modname]]$continuous.boyce$Spearman.cor))

      # Format data table to make it pretty
      stats.df <- ggpubr::ggtexttable(stats.df, rows = NULL)

      # We want the classification plot to be the same height as the calib plot and table combined
      # and the easiest way to do this is just to pack them separately
      p1 <- ggarrange(x$recalibrated.plots$calibration.plot[[modname]], stats.df,
                      ncol = 1, nrow = 2)

      summary.plot <- ggarrange(p1, x$recalibrated.plots$classification.plot[[modname]] + theme(legend.position="none"),
                                ncol = 2, nrow = 1)
      summary.plot <- annotate_figure(summary.plot,
                                      top = text_grob(modname))
    }

    recal.plots <- list()
    for(i in names(x$recalibrated.metrics)){
      recal.plots[[i]] <- plotmodel(x, i)
    }

    return(recal.plots)
  } else {

    # Code to plot the uncalibrated model with metrics

    # Pack up summary stats
    stats.df <- data.frame(Metric = c("ECE", "MCE", "ECE.equal.width", "MCE.equal.width", "boyce.index"),
                           Value = c(x$ECE, x$MCE, x$ECE.equal.width, x$MCE.equal.width, x$continuous.boyce$Spearman.cor))

    # Format data table to make it pretty
    stats.df <- ggpubr::ggtexttable(stats.df, rows = NULL)

    # We want the classification plot to be the same height as the calib plot and table combined
    # and the easiest way to do this is just to pack them separately
    p1 <- ggarrange(x$calibration.plot, stats.df,
                    ncol = 1, nrow = 2)

    summary.plot <- ggarrange(p1, x$classification.plot + theme(legend.position="none"),
                              ncol = 2, nrow = 1)
    summary.plot <- annotate_figure(summary.plot,
                                    top = text_grob("Uncalibrated Model"))
    return(summary.plot)
  }


}

# Function to make classification plots
class.plot <- function(pred, obs, name, cuts){
  temp.df <- data.frame(pred = pred,
                        obs = obs)
  return(qplot(pred, facets = obs ~ ., data = temp.df,
               alpha = 0.5, ylab = "Count", xlab = "Predicted",
               bins = cuts, fill = obs, color = obs, main = name) +
           theme_minimal() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
           scale_x_continuous(limits = c(0, 1), oob = function(x, limits) x))
}

# Function to make calibration plots
calib.plot <- function(pred, obs, name, cuts){

  temp.df <- data.frame(pred = pred,
                        obs = obs)

  this.calib <- caret::calibration(obs ~ pred, data = temp.df, class = "presence", cuts = cuts)
  this.calib$data <- this.calib$data[complete.cases(this.calib$data),]

  return(qplot(this.calib$data$midpoint, this.calib$data$Percent,
               geom = c("line", "point"), xlim = c(0, 100), ylim = c(0, 100),
               xlab = "Predicted", ylab = "Observed", main = name) +
           geom_abline(intercept = 0, slope = 1, linetype = 3) +
           theme_minimal() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)))
}
