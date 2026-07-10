#' Takes an enmtools.model object, and performs variable importance analyses on it using methods from the vip package
#'
#' @param model An enmtools.model object
#' @param metric The metric to use for measuring how variables affect model predictions
#' @param nsim The number of simulations to be run for method "permute"
#' @param method A character string or vector containing any combination of "model", "permute", "shap", or "firm".  "model", "permute", and "firm" use functions from the vip package; "shap" computes SHAP values via kernelshap and returns a shapviz object.
#' @param shap_method Either "permshap" (default, faster) or "kernelshap" (slower but more accurate for correlated features).  Ignored for enmtools.glm and enmtools.gam models, which always use additive_shap.
#' @param verbose Controls printing of messages
#' @param ... Further arguments to be passed to vip's "vi" functions or kernelshap/permshap, depending on which method is chosen.
#'
#' @return An enmtools.vip object
#'
#' @examples
#' \donttest{
#' #install.extras(repos='http://cran.us.r-project.org')
#' requireNamespace("vip", quietly = TRUE)
#' monticola.glm <- enmtools.glm(iberolacerta.clade$species$monticola,
#'                               env = euro.worldclim,
#'                               test.prop = 0.3)
#' if(check.extras("enmtools.vip")) {
#'   enmtools.vip(monticola.glm)
#' }
#' }

enmtools.vip <- function(model, metric = "roc_auc", nsim = 10, method = "permute", shap_method = "permshap", verbose = TRUE, ...){

  assert.extras.this.fun()

  output <- list()

  if(inherits(model, "enmtools.bc") | inherits(model, "enmtools.dm")){
    stop("Variable importance tests not available for models of this type.")
  }

  if(inherits(model, "enmtools.glm")){
    thismodel <- model$model
    feature_names <- labels(terms(thismodel))
    feature_names <- gsub("poly\\(", "", feature_names)
    feature_names <- gsub(",.*", "", feature_names)
    train <- model$analysis.df[,-c(1,2)]
    target <- "presence"
    pred_wrapper <- predict
    train$presence <- as.factor(train$presence)
  }

  if(inherits(model, "enmtools.gam")){
    thismodel <- model$model
    feature_names <- labels(terms(thismodel))
    train <- model$analysis.df[,-c(1,2)]
    target <- "presence"
    pred_wrapper <- predict
    train$presence <- as.factor(train$presence)
  }

  if(inherits(model, "enmtools.rf")){
    thismodel <- model$model
    feature_names <- labels(terms(thismodel))
    train <- model$analysis.df[,-c(1,2)]
    target <- "presence"
    pred_wrapper <- predict
    train$presence <- as.factor(train$presence)
  }

  if(inherits(model, "enmtools.rf.ranger")){
    thismodel <- model$model
    feature_names <- colnames(model$analysis.df)
    feature_names <- feature_names[!feature_names %in% c("x", "y", "presence")]
    train <- model$analysis.df[,-c(1,2)]
    target <- "presence"
    pred_wrapper <- function(object, newdata) predict(object, data = newdata, type = "response")$predictions
    train$presence <- as.factor(train$presence)
  }

  if(inherits(model, "enmtools.maxent")){
    thismodel <- model$model
    train <- rbind(attr(thismodel, "presence"), attr(thismodel, "absence"))
    feature_names <- colnames(train)
    train$presence <- c(rep(1, nrow(attr(thismodel, "presence"))),
                        rep(0, nrow(attr(thismodel, "absence"))))
    target <- "presence"
    pred_wrapper <- function(object, newdata) predict(object, newdata)
    train$presence <- as.factor(train$presence)
  }

  if(inherits(model, "enmtools.ppmlasso")){
    thismodel <- model$model
    feature_names <- colnames(model$analysis.df)
    feature_names <- feature_names[!feature_names %in% c("x", "y", "presence", "wt")]
    train <- model$analysis.df[,c(feature_names, "presence")]
    target <- "presence"
    pred_wrapper <- function(object, newdata) predict(object, newdata = newdata, type = "response")
    train$presence <- as.factor(train$presence)
  }

  if("model" %in% method){

    if(inherits(model, c("enmtools.gam")) | inherits(model, c("enmtools.maxent")) | inherits(model, c("enmtools.ppmlasso"))){
      output[["model"]] <- "Variable importance using this method has not been implemented for models of this type."
    } else {
      output[["model"]] <- vip::vi_model(thismodel)

      output[["model.plot"]] <- ggplot(output[["model"]],
                                       aes(x = Importance,
                                           fill = fct_reorder(Variable, Importance, .desc = TRUE))) +
        geom_histogram(bins = 20) +
        theme_bw() +
        geom_hline(yintercept = 0, color = "grey") +
        viridis::scale_fill_viridis(name = "Variable", option = "D", discrete = TRUE, direction = -1) +
        facet_grid(rows = vars(fct_reorder(.data$Variable, .data$Importance, .desc = TRUE)), switch = "y") +
        ylab("Variable") +
        ggtitle("Variable importance, model-specific method") +
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

  }

  if("permute" %in% method){

    if(inherits(model, c("enmtools.maxent")) & verbose == FALSE){
      invisible(capture.output(output[["permute"]] <- vip::vi_permute(thismodel,
                                                                      feature_names = feature_names,
                                                                      train = train,
                                                                      target = target,
                                                                      metric = metric,
                                                                      pred_wrapper = pred_wrapper,
                                                                      event_level = "second",
                                                                      nsim = nsim,
                                                                      keep = TRUE)))
    } else {
      output[["permute"]] <- vip::vi_permute(thismodel,
                                             feature_names = feature_names,
                                             train = train,
                                             target = target,
                                             metric = metric,
                                             pred_wrapper = pred_wrapper,
                                             event_level = "second",
                                             nsim = nsim,
                                             keep = TRUE)
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
    X_shap <- train[, feature_names, drop = FALSE]

    shap_pred <- function(object, X) pred_wrapper(object, X)

    if(inherits(model, c("enmtools.glm", "enmtools.gam"))){
      sv <- shapviz::shapviz(kernelshap::additive_shap(thismodel,
                                                       X = X_shap,
                                                       pred_fun = shap_pred))
    } else {
      pres_idx <- which(train$presence == 1)
      abs_idx  <- which(train$presence == 0)
      bg_idx   <- c(pres_idx, sample(abs_idx, min(length(pres_idx), length(abs_idx))))
      bg_X     <- X_shap[bg_idx, , drop = FALSE]

      shap_fun <- if(shap_method == "kernelshap") kernelshap::kernelshap else kernelshap::permshap

      sv <- shapviz::shapviz(shap_fun(thismodel,
                                      X = X_shap,
                                      bg_X = bg_X,
                                      pred_fun = shap_pred,
                                      verbose = verbose,
                                      ...))
    }

    output[["shap"]] <- sv

    shap_long <- data.frame(Variable = colnames(sv$S),
                            Importance = colMeans(abs(sv$S)))

    output[["shap.plot"]] <- ggplot(shap_long,
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

    output[["per.observation.plot"]] <- shapviz::sv_importance(sv, kind = "beeswarm",
                                                               viridis_args = list(option = "D")) +
      theme_bw() +
      ggtitle("SHAP values per observation") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5),
            legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            plot.margin = margin(7, 14, 7, 7))
  }


  if("firm" %in% method){

    # This method is using pred.fun instead of pred_wrapper,
    # I think because it's being passed to pdp
    if(inherits(model, c("enmtools.maxent"))){
      output[["firm"]] <- vip::vi_firm(thismodel,
                                       feature_names = feature_names,
                                       train = train,
                                       target = target,
                                       metric = metric,
                                       pred.fun = pred_wrapper,
                                       reference_class = "1",
                                       nsim = nsim)
    } else {
      output[["firm"]] <- vip::vi_firm(thismodel,
                                       feature_names = feature_names,
                                       train = train,
                                       target = target,
                                       metric = metric,
                                       reference_class = "1",
                                       nsim = nsim)
    }

    output[["firm.plot"]] <- ggplot(output[["firm"]],
                                    aes(x = Importance,
                                        fill = fct_reorder(Variable, Importance, .desc = TRUE))) +
      geom_histogram(bins = 20) +
      theme_bw() +
      geom_hline(yintercept = 0, color = "grey") +
      viridis::scale_fill_viridis(name = "Variable", option = "D", discrete = TRUE, direction = -1) +
      facet_grid(rows = vars(fct_reorder(.data$Variable, .data$Importance, .desc = TRUE)), switch = "y") +
      ylab("Variable") +
      ggtitle("Variable importance, FIRM method") +
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

  class(output) <- c("enmtools.vip")

  return(output)

}

