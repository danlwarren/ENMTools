#' Takes an enmtools.species object and builds a maxnet model
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
#' @param ... Additional arguments to be passed to maxnet::maxnet()
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @details maxnet provides a pure R implementation of the Maxent algorithm using glmnet for regularization.
#' Unlike the original Java-based Maxent, maxnet does not require Java installation.
#' The model uses the same feature types (linear, quadratic, product, threshold, hinge) and
#' regularization approach as Maxent.
#'
#' @seealso \code{\link[maxnet]{maxnet}} for the underlying modeling function.
#'
#' @examples
#' \dontrun{
#' enmtools.maxnet(iberolacerta.clade$species$monticola, env = euro.worldclim)
#' }
#'
#' @export
enmtools.maxnet <- function(species, env, f = NULL, test.prop = 0, eval = TRUE,
                            nback = 1000, env.nback = 10000, report = NULL,
                            overwrite = FALSE, rts.reps = 0, weights = "equal",
                            bg.source = "default", verbose = FALSE, clamp = TRUE,
                            corner = NA, bias = NA, ...) {

  assert.extras.this.fun()

  model_args <- list(...)

  enmtools.tidy(
    species = species,
    env = env,
    f = f,
    model = "maxnet",
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


# Summary for objects of class enmtools.maxnet
#' @exportS3Method
summary.enmtools.maxnet <- function(object, plot = TRUE, ...) {

  cat("\n\nmaxnet model\n\n")

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

# Print method for objects of class enmtools.maxnet
#' @exportS3Method
print.enmtools.maxnet <- function(x, ...) {
  print(summary(x, ...))
}


# Plot method for objects of class enmtools.maxnet
#' @exportS3Method
plot.enmtools.maxnet <- function(x, ...) {

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
    title <- paste("maxnet model for", x$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }

  return(suit.plot)
}


# Predict method for models of class enmtools.maxnet
#' @export
predict.enmtools.maxnet <- function(object, env, ...) {

  # Use the standard enmtools.tidy predict method
  NextMethod("predict", object, env = env, ...)
}
