#' Takes an enmtools.model object, and performs variable importance analyses on it using methods from the vip package
#'
#' @param model An enmtools.model object
#' @param metric The metric to use for measuring how variables affect model predictions
#' @param nsim The number of simulations to be run for method "permute"
#' @param method A character string or vector containing any combination of "model", "permute", "shap", or "firm".  For details on what these mean, see the vip package help.
#' @param ... Further arguments to be passed to vip's "vi" functions.
#'
#' @return An enmtools.vip object
#'
#' @examples
#' \dontrun{
#' install.extras(repos='http://cran.us.r-project.org')
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' monticola.glm <- enmtools.glm(iberolacerta.clade$species$monticola,
#'                               env = euro.worldclim,
#'                               test.prop = 0.3)
#' enmtools.vip(monticola.glm)
#' }

enmtools.vip <- function(model, metric = "auc", nsim = 10, method = "permute", ...){

  check.packages(c("vip", "pdp", "fastshap", "reshape2", "viridis"))

  output <- list()

  if(inherits(model, "enmtools.bc") | inherits(model, "enmtools.dm")){
    stop("Variable importance tests not available for models of this type.")
  }

  if(inherits(model, "enmtools.glm")){
    thismodel <- model$model
    feature_names <- labels(terms(thismodel))
    train <- model$analysis.df[,-c(1,2)]
    target <- "presence"
    pred_wrapper <- predict
    reference_class <- "1"
  }

  if(inherits(model, "enmtools.gam")){
    thismodel <- model$model
    feature_names <- labels(terms(thismodel))
    train <- model$analysis.df[,-c(1,2)]
    target <- "presence"
    pred_wrapper <- predict
    reference_class <- "1"
  }

  if(inherits(model, "enmtools.rf")){
    thismodel <- model$model
    feature_names <- labels(terms(thismodel))
    train <- model$analysis.df[,-c(1,2)]
    target <- "presence"
    pred_wrapper <- predict
    reference_class <- "1"
  }

  if(inherits(model, "enmtools.rf.ranger")){
    thismodel <- model$model
    feature_names <- colnames(model$analysis.df)
    feature_names <- feature_names[!feature_names %in% c("Longitude", "Latitude", "presence")]
    train <- model$analysis.df[,-c(1,2)]
    target <- "presence"
    pred_wrapper <- function(object, newdata) predict(object, data = newdata, type = "response")$predictions
    reference_class <- "1"
  }

  if(inherits(model, "enmtools.maxent")){
    thismodel <- model$model
    train <- rbind(attr(thismodel, "presence"), attr(thismodel, "absence"))
    feature_names <- colnames(train)
    train$presence <- c(rep(1, nrow(attr(thismodel, "presence"))),
                        rep(0, nrow(attr(thismodel, "absence"))))
    target <- "presence"
    pred_wrapper <- function(object, newdata) predict(object, newdata)
    reference_class <- "1"
  }

  if(inherits(model, "enmtools.ppmlasso")){
    thismodel <- model$model
    feature_names <- colnames(model$analysis.df)
    feature_names <- feature_names[!feature_names %in% c("Longitude", "Latitude", "presence", "wt")]
    train <- model$analysis.df[,c(feature_names, "presence")]
    target <- "presence"
    pred_wrapper <- function(object, newdata) predict(object, newdata = newdata, type = "response")
    reference_class <- "1"
  }

  if("model" %in% method){

    if(inherits(model, c("enmtools.gam")) | inherits(model, c("enmtools.maxent")) | inherits(model, c("enmtools.ppmlasso"))){
      output[["model"]] <- "Variable importance using this method has not been implemented for models of this type."
    } else {
      output[["model"]] <- vip::vi_model(thismodel)

      output[["model.plot"]] <- ggplot(output[["model"]],
                                       aes_string(x = "Importance",
                                           fill = fct_reorder("Variable", "Importance", .desc = TRUE))) +
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
    output[["permute"]] <- vip::vi_permute(thismodel,
                                           feature_names = feature_names,
                                           train = train,
                                           target = target,
                                           metric = metric,
                                           pred_wrapper = pred_wrapper,
                                           reference_class = "1",
                                           nsim = nsim,
                                           keep = TRUE)

    plotdf <- reshape2::melt(attr(output[["permute"]], "raw_scores"))
    colnames(plotdf) <- c("Variable", "Permutation", "Importance")

    output[["permute.plot"]] <- ggplot(plotdf,
                                       aes_string(x = "Importance",
                                           fill = "..x..")) +
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
    output[["shap"]] <- vip::vi_shap(thismodel,
                                     feature_names = feature_names,
                                     train = train,
                                     pred_wrapper = pred_wrapper,
                                     nsim = nsim)

    output[["shap.plot"]] <- ggplot(output[["shap"]],
                                    aes_string(x = "Importance",
                                        fill = fct_reorder("Variable", "Importance", .desc = TRUE))) +
      geom_histogram(bins = 20) +
      theme_bw() +
      geom_hline(yintercept = 0, color = "grey") +
      viridis::scale_fill_viridis(name = "Variable", option = "D", discrete = TRUE, direction = -1) +
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
                                    aes_string(x = "Importance",
                                        fill = fct_reorder("Variable", "Importance", .desc = TRUE))) +
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

