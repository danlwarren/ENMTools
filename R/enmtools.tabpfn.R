#' Takes an enmtools.species object and builds a TabPFN model
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data.
#' @param f Standard R formula or tidymodels recipe (optional). If NULL, all environmental variables will be used.
#' @param test.prop Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.
#' @param eval Determines whether model evaluation should be done. Turned on by default.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param env.nback Number of points to draw from environment space for environment space discrimination metrics.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param rts.reps The number of replicates to do for a Raes and ter Steege-style test of significance
#' @param weights If this is set to "equal", presences and background data will be assigned weights so that the sum of all presence points weights equals the sum of all background point weights. Otherwise, weights are not provided to the model.
#' @param bg.source Source for drawing background points. If "points", it just uses the background points that are already in the species object. If "range", it uses the range raster. If "env", it draws points at random from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports. Defaults to FALSE.
#' @param clamp When set to TRUE, clamps the environmental layers so that predictions made outside the min/max of the training data for each predictor are set to the value for the min/max for that predictor.
#' @param corner An integer from 1 to 4. Selects which corner to use for "block" test data. By default the corner is selected randomly.
#' @param bias An optional raster estimating relative sampling effort per grid cell. Will be used for drawing background data.
#' @param backend Character. Either "local" (default) for local Python TabPFN, or "api" for cloud API backend.
#' @param model_path Character. Controls which model to use:
#'   - "tabpfn-v2-classifier-v2_default.ckpt" (default): TabPFN v2 pretrained model (ungated)
#'   - "auto": latest TabPFN model (may require HuggingFace authentication)
#'   - "real": pretrained model trained on real data
#'   - A finetuned model name (e.g. "sdm-finetuned-nonspatial"): downloads and uses a finetuned checkpoint
#'   - A file path ending in .pt: uses a local finetuned checkpoint
#' @param device Character. Device for computation: "auto" (default), "cuda", or "cpu".
#' @param n_estimators Integer. Number of ensemble estimators (default 16).
#' @param softmax_temperature Numeric. Softmax temperature for predictions (default 0.9).
#' @param balance_probabilities Logical. Whether to balance class probabilities (default TRUE).
#' @param average_before_softmax Logical. Whether to average logits before softmax (default TRUE).
#' @param ensemble_subsamples Integer or NULL. For API backend, number of subsamples for manual ensembling.
#'   If NULL (default), no ensembling is done.
#' @param ... Additional arguments passed to the model.
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @details TabPFN (Tabular Prior-Fitted Network) is a foundation model for tabular
#' classification that achieves strong performance without hyperparameter tuning.
#' It works by performing in-context learning at inference time, using the training
#' data as context for predictions.
#'
#' For SDM, a subsampling ensemble approach is used: each of the \code{n_estimators}
#' ensemble members sees all presence points plus a balanced random sample of
#' background points (equal to the number of presences). This handles the class
#' imbalance inherent in SDM data and is passed to TabPFN via its native
#' \code{SUBSAMPLE_SAMPLES} inference config parameter.
#'
#' Two backends are available:
#' \itemize{
#'   \item \strong{Local} (\code{backend = "local"}): Uses the TabPFN Python package
#'     locally. Supports both pretrained models (default) and finetuned SDM models.
#'     Requires the Python \code{tabpfn} package (see \code{\link{install.tabpfn}}).
#'   \item \strong{Cloud API} (\code{backend = "api"}): Uses the TabPFN cloud service.
#'     Requires a \code{TABPFN_ACCESS_TOKEN} environment variable.
#' }
#'
#' @seealso \code{\link{install.tabpfn}} for installing the Python dependencies,
#'   \code{\link{tabpfn_list_models}} for available finetuned models.
#'
#' @examples
#' \dontrun{
#' install.tabpfn()
#' enmtools.tabpfn(iberolacerta.clade$species$monticola, env = euro.worldclim)
#' }
#'
#' @export
enmtools.tabpfn <- function(species, env, f = NULL, test.prop = 0, eval = TRUE,
                             nback = 1000, env.nback = 10000, report = NULL,
                             overwrite = FALSE, rts.reps = 0, weights = "equal",
                             bg.source = "default", verbose = FALSE, clamp = TRUE,
                             corner = NA, bias = NA,
                             backend = "local",
                             model_path = "tabpfn-v2-classifier-v2_default.ckpt",
                             device = "auto",
                             n_estimators = 16L,
                             softmax_temperature = 0.9,
                             balance_probabilities = TRUE,
                             average_before_softmax = TRUE,
                             ensemble_subsamples = NULL,
                             ...) {

  assert.extras.this.fun()

  # Resolve finetuned model names to paths before passing to bridge
  if (model_path %in% names(.tabpfn_models)) {
    model_path <- tabpfn_model_path(model_path)
  }

  model_args <- list(
    backend = backend,
    model_path = model_path,
    device = device,
    n_estimators = as.integer(n_estimators),
    softmax_temperature = softmax_temperature,
    balance_probabilities = balance_probabilities,
    average_before_softmax = average_before_softmax,
    ensemble_subsamples = ensemble_subsamples,
    ...
  )

  enmtools.tidy(
    species = species,
    env = env,
    f = f,
    model = "tabpfn",
    test.prop = test.prop,
    eval = eval,
    nback = nback,
    env.nback = env.nback,
    report = report,
    overwrite = overwrite,
    rts.reps = rts.reps,
    weights = weights,
    bg.source = bg.source,
    verbose = verbose,
    clamp = clamp,
    corner = corner,
    bias = bias,
    model_args = model_args
  )
}


# Summary for objects of class enmtools.tabpfn
#' @exportS3Method
summary.enmtools.tabpfn <- function(object, plot = TRUE, ...) {

  cat("\n\nTabPFN model\n\n")

  cat("\n\nFormula:  ")
  form <- workflows::extract_preprocessor(object$model)
  if (!inherits(form, "formula")) {
    form <- formula(recipes::prep(form))
  }
  cat(deparse(form))

  cat("\n\nData table (top ten lines): ")
  print(knitr::kable(head(object$analysis.df, 10)))

  cat("\n\nModel fit (training data):  ")
  print(object$training.evaluation)

  cat("\n\nEnvironment space model fit (training data):  ")
  print(object$env.training.evaluation)

  cat("\n\nProportion of data withheld for model fitting:  ")
  cat(object$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(object$test.evaluation)

  cat("\n\nEnvironment space model fit (test data):  ")
  print(object$env.test.evaluation)

  cat("\n\nSuitability:  \n")
  print(object$suitability)

  cat("\n\nNotes:  \n")
  object$notes

  if (plot) {
    plot(object)
  }
}

# Print method for objects of class enmtools.tabpfn
#' @exportS3Method
print.enmtools.tabpfn <- function(x, ...) {
  print(summary(x, ...))
}


# Plot method for objects of class enmtools.tabpfn
#' @exportS3Method
plot.enmtools.tabpfn <- function(x, ...) {

  suit.points <- data.frame(rasterToPoints2(x$suitability))
  colnames(suit.points) <- c("x", "y", "Suitability")
  test <- terra::as.data.frame(x$test.data, geom = "XY")

  suit.plot <- ggplot(data = suit.points, aes(y = .data$y, x = .data$x)) +
    geom_raster(aes(fill = .data$Suitability)) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = x$analysis.df[x$analysis.df$presence == 1, ], aes(y = .data$y, x = .data$x),
               pch = 21, fill = "white", color = "black", size = 2)

  if (inherits(x$test.data, "SpatVector")) {
    suit.plot <- suit.plot + geom_point(data = test, aes(y = .data$y, x = .data$x),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }

  if (!is.na(x$species.name)) {
    title <- paste("TabPFN model for", x$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }

  return(suit.plot)
}


# Predict method for models of class enmtools.tabpfn
#' @export
predict.enmtools.tabpfn <- function(object, env, ...) {

  # Check for stale Python objects
  fit_obj <- workflows::extract_fit_parsnip(object$model)$fit
  if (inherits(fit_obj, "tabpfn_fit") && !is.null(fit_obj$clf)) {
    if (reticulate::py_is_null_xptr(fit_obj$clf)) {
      stop("TabPFN model contains a stale Python object. ",
           "Python objects do not persist across R sessions. ",
           "Please refit the model.", call. = FALSE)
    }
  }

  # Use the standard enmtools.tidy predict method
  NextMethod("predict", object, env = env, ...)
}
