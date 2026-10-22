#' Takes an enmtools.model object, and performs variable importance analyses on it using permutation importance and/or SHAP values
#'
#' @param model An enmtools.model object
#' @param metric The model fit metric used to measure the effect of permuting each variable.  Any single-value metric stored in a dismo ModelEvaluation object can be used, default is "auc".
#' @param nsim The number of permutations to be run for each variable for method "permute"
#' @param method A character string or vector containing "permute", "shap", or both.  "permute" measures the decline in model fit when each variable is permuted; "shap" calculates SHAP values for presence points.  For GLMs and GAMs, SHAP values are calculated exactly from the model terms, on the scale of the linear predictor.  For random forests (randomForest or ranger), they are calculated with TreeSHAP, which requires the treeshap package.  These exact methods are only used when the formula or recipe passes the predictors to the model unchanged; for other models, including those fitted with a recipe that transforms the predictors, SHAP values are estimated with the kernelshap package.
#' @param shap.method For models whose SHAP values are estimated with kernelshap (see `method`), either "permshap" (default, faster) or "kernelshap" (slower but more accurate for correlated features).
#' @param shap.npres The maximum number of presence points for which SHAP values are calculated.  Presence points are subsampled at random if there are more than this.
#' @param shap.nback The maximum number of background points used as the reference sample for SHAP values, so that each value describes how a variable makes a presence more or less suitable than the study area as a whole.  Argument not used for enmtools.glm, enmtools.gam, enmtools.rf, and enmtools.rf.ranger models.
#' @param verbose Controls printing of messages
#' @param ... Further arguments to be passed to kernelshap/permshap.
#'
#' @return An enmtools.vip object.  If method includes "shap", its "shap" element is a list containing the SHAP values (S), the predictor values for the presence points they were calculated for (X), and the baseline prediction (baseline).  These can be passed to the shapviz package for further plots, e.g. shapviz::shapviz(result$shap$S, X = result$shap$X, baseline = result$shap$baseline).
#'
#' @examples
#' \donttest{
#' monticola.glm <- enmtools.glm(iberolacerta.clade$species$monticola,
#'                               env = euro.worldclim,
#'                               test.prop = 0.3)
#' if(check.extras("enmtools.vip")) {
#'   enmtools.vip(monticola.glm)
#' }
#' }

enmtools.vip <- function(model, metric = "auc", nsim = 10, method = "permute", shap.method = "permshap", shap.npres = 100, shap.nback = 100, verbose = TRUE, ...){

  assert.extras.this.fun()

  output <- list()

  if(any(!method %in% c("permute", "shap"))){
    stop("method must be \"permute\", \"shap\", or both.")
  }

  if(!inherits(model, c("enmtools.tidy", "enmtools.maxent"))){
    stop("Variable importance tests not available for models of this type.")
  }

  if(inherits(model, "enmtools.tidy")){
    thismodel <- model$model
    # Original predictor columns, before any formula or recipe processing
    feature.names <- colnames(workflows::extract_mold(thismodel)$blueprint$ptypes$predictors)
    train <- model$analysis.df[,-c(1,2)]
    pred.wrapper <- function(object, newdata) predict(object, new_data = newdata, type = "prob")$.pred_1
  }

  if(inherits(model, "enmtools.maxent")){
    thismodel <- model$model
    train <- rbind(attr(thismodel, "presence"), attr(thismodel, "absence"))
    feature.names <- colnames(train)
    train$presence <- c(rep(1, nrow(attr(thismodel, "presence"))),
                        rep(0, nrow(attr(thismodel, "absence"))))
    pred.wrapper <- function(object, newdata) predict(object, newdata)
  }

  if("permute" %in% method){

    if(inherits(model, c("enmtools.maxent")) & verbose == FALSE){
      invisible(capture.output(output[["permute"]] <- permutation.importance(thismodel,
                                                                             train = train,
                                                                             feature.names = feature.names,
                                                                             pred.fun = pred.wrapper,
                                                                             metric = metric,
                                                                             nsim = nsim)))
    } else {
      output[["permute"]] <- permutation.importance(thismodel,
                                                    train = train,
                                                    feature.names = feature.names,
                                                    pred.fun = pred.wrapper,
                                                    metric = metric,
                                                    nsim = nsim)
    }
    plotdf <- reshape2::melt(attr(output[["permute"]], "raw_scores"))
    colnames(plotdf) <- c("Variable", "Permutation", "Importance")

    output[["permute.plot"]] <- ggplot(plotdf,
                                       aes(x = Importance,
                                           fill = after_stat(x))) +
      geom_histogram(bins = 20) +
      theme_bw() +
      geom_hline(yintercept = 0, color = "grey") +
      viridis::scale_fill_viridis(name = "Variable", option = "D") +
      facet_grid(rows = vars(fct_reorder(.data$Variable, .data$Importance, .desc = TRUE)), switch = "y") +
      ylab("Variable") +
      ggtitle("Variable importance, permutation method") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.spacing = unit(0, "lines"),
            axis.title.x = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5),
            legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank(),
            strip.text.y.left = element_text(angle = 0),
            plot.margin = margin(7, 14, 7, 7))
  }

  # To access the raw scores from reps you use attr(results$permute, "raw_scores")

  if("shap" %in% method){

    # Explain at most shap.npres presences, using at most shap.nback
    # background points as the reference sample
    pres.idx <- which(train$presence == 1)
    abs.idx  <- which(train$presence == 0)
    pres.idx <- pres.idx[sample.int(length(pres.idx), min(shap.npres, length(pres.idx)))]
    abs.idx  <- abs.idx[sample.int(length(abs.idx), min(shap.nback, length(abs.idx)))]
    X.shap <- train[pres.idx, feature.names, drop = FALSE]
    bg.X   <- train[abs.idx, feature.names, drop = FALSE]

    # Exact methods (additive SHAP for GLMs and GAMs, TreeSHAP for random
    # forests) work on the fitted model itself, so they can only be used when
    # the formula or recipe passes the predictors to it unchanged
    if(inherits(model, "enmtools.tidy")){
      engine.fit <- workflows::extract_fit_engine(thismodel)
      mold <- workflows::extract_mold(thismodel)
      passthrough <- identical(colnames(mold$predictors), colnames(mold$blueprint$ptypes$predictors))
    } else {
      engine.fit <- thismodel
      passthrough <- FALSE
    }

    if(passthrough && inherits(engine.fit, c("glm", "gam"))){
      shap <- additive.shap(engine.fit, X.shap, feature.names)
    } else if(passthrough && inherits(engine.fit, c("randomForest", "ranger"))){
      if(!requireNamespace("treeshap", quietly = TRUE)){
        stop("SHAP values for random forest models require the treeshap package.  Install it with install.extras(\"enmtools.vip.treeshap\") or install.packages(\"treeshap\").  If no binary is available for your system, use install.packages(\"treeshap\", type = \"source\").", call. = FALSE)
      }

      # TreeSHAP needs a reference sample that reaches every node the explained
      # points do, so it uses all of the training data
      if(inherits(engine.fit, "randomForest")){
        unified <- treeshap::randomForest.unify(engine.fit, train[, feature.names, drop = FALSE])
      } else {
        unified <- treeshap::ranger.unify(engine.fit, train[, feature.names, drop = FALSE])
      }
      shap <- list(S = as.matrix(treeshap::treeshap(unified, X.shap, verbose = FALSE)$shaps),
                   X = X.shap,
                   baseline = mean(pred.wrapper(thismodel, train[, feature.names, drop = FALSE])))
    } else {
      if(!requireNamespace("kernelshap", quietly = TRUE)){
        stop("SHAP values for this type of model require the kernelshap package.  Install it with install.extras(\"enmtools.vip.kernelshap\") or install.packages(\"kernelshap\").", call. = FALSE)
      }

      shap.fun <- if(shap.method == "kernelshap") kernelshap::kernelshap else kernelshap::permshap

      ks <- shap.fun(thismodel,
                     X = X.shap,
                     bg_X = bg.X,
                     pred_fun = pred.wrapper,
                     verbose = verbose,
                     ...)
      shap <- list(S = ks$S, X = X.shap, baseline = ks$baseline)
    }

    rownames(shap$S) <- NULL
    output[["shap"]] <- shap

    shap.long <- data.frame(Variable = colnames(shap$S),
                            Importance = colMeans(abs(shap$S)))

    output[["shap.plot"]] <- ggplot(shap.long,
                                    aes(x = Importance, fill = after_stat(x))) +
      geom_histogram(bins = 20) +
      theme_bw() +
      geom_hline(yintercept = 0, color = "grey") +
      viridis::scale_fill_viridis(name = "Variable", option = "D") +
      facet_grid(rows = vars(fct_reorder(.data$Variable, .data$Importance, .desc = TRUE)), switch = "y") +
      ylab("Variable") +
      ggtitle("Variable importance, SHAP method") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.spacing = unit(0, "lines"),
            axis.title.x = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5),
            legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank(),
            strip.text.y.left = element_text(angle = 0),
            plot.margin = margin(7, 14, 7, 7))

    # Beeswarm-style plot of every SHAP value, colored by the value of the
    # predictor scaled to 0-1 within each variable
    shap.points <- data.frame(Variable = rep(colnames(shap$S), each = nrow(shap$S)),
                              SHAP = as.vector(shap$S),
                              Value = as.vector(apply(as.matrix(shap$X[, colnames(shap$S), drop = FALSE]), 2, function(x)
                                (x - min(x)) / max(diff(range(x)), .Machine$double.eps))))
    shap.points$Variable <- factor(shap.points$Variable,
                                   levels = shap.long$Variable[order(shap.long$Importance)])

    output[["per.observation.plot"]] <- ggplot(shap.points, aes(x = .data$SHAP, y = .data$Variable, color = .data$Value)) +
      geom_vline(xintercept = 0, color = "grey") +
      geom_jitter(width = 0, height = 0.2, size = 1) +
      viridis::scale_color_viridis(name = "Predictor value", option = "D", breaks = c(0, 1), labels = c("Low", "High")) +
      theme_bw() +
      xlab("SHAP value") +
      ylab(NULL) +
      ggtitle("SHAP values per observation") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(hjust = 0.5),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            plot.margin = margin(7, 14, 7, 7))
  }

  class(output) <- c("enmtools.vip")

  return(output)

}


# Permutation importance: the decline in a dismo model fit metric when each
# variable is permuted in turn.  Returns a data frame of mean importance and
# standard deviation for each variable, with the importance from every
# permutation stored in the "raw_scores" attribute.
permutation.importance <- function(object, train, feature.names, pred.fun, metric = "auc", nsim = 10){

  if(metric == "roc_auc"){
    metric <- "auc"
  }

  higher.better <- c("auc", "cor", "kappa", "CCR", "TPR", "TNR", "PPP", "NPP", "OR")
  lower.better <- c("FPR", "FNR", "MCR")
  if(!metric %in% c(higher.better, lower.better)){
    stop(paste0("metric must be one of: ", paste(c(higher.better, lower.better), collapse = ", ")))
  }

  pres <- train$presence == 1

  fit.metric <- function(pred, tr){
    evaluation <- dismo::evaluate(pred[pres], pred[!pres], tr = tr)
    value <- methods::slot(evaluation, metric)
    if(metric %in% lower.better){
      value <- -value
    }
    value
  }

  # Threshold-dependent metrics are calculated at the threshold that maximizes
  # sensitivity plus specificity for the unpermuted model
  baseline.pred <- as.numeric(pred.fun(object, train[, feature.names, drop = FALSE]))
  threshold <- dismo::threshold(dismo::evaluate(baseline.pred[pres], baseline.pred[!pres]), "spec_sens")
  baseline <- fit.metric(baseline.pred, threshold)

  raw.scores <- matrix(NA, nrow = length(feature.names), ncol = nsim,
                       dimnames = list(feature.names, paste0("permutation.", 1:nsim)))

  for(i in feature.names){
    for(j in 1:nsim){
      permuted <- train[, feature.names, drop = FALSE]
      permuted[[i]] <- sample(permuted[[i]])
      raw.scores[i, j] <- baseline - fit.metric(as.numeric(pred.fun(object, permuted)), threshold)
    }
  }

  output <- data.frame(Variable = feature.names,
                       Importance = rowMeans(raw.scores),
                       StDev = apply(raw.scores, 1, sd))
  rownames(output) <- NULL
  attr(output, "raw_scores") <- raw.scores
  attr(output, "metric") <- metric
  output
}


# Exact SHAP values for additive models (GLMs and GAMs), taken from the
# contribution of each model term on the scale of the linear predictor.  Each
# term must involve a single predictor; terms for the same predictor are summed.
additive.shap <- function(object, X, feature.names){
  terms.pred <- predict(object, newdata = X, type = "terms")
  term.vars <- lapply(colnames(terms.pred), function(x) all.vars(parse(text = x)[[1]]))

  if(any(lengths(term.vars) != 1)){
    stop("SHAP values can only be calculated exactly for models in which each term involves a single predictor.")
  }

  term.vars <- unlist(term.vars)
  S <- sapply(feature.names, function(x) rowSums(terms.pred[ , term.vars == x, drop = FALSE]))
  S <- matrix(S, nrow = nrow(X), dimnames = list(NULL, feature.names))

  list(S = S, X = X, baseline = as.numeric(attr(terms.pred, "constant")))
}
