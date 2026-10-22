#' Takes an enmtools.species object and builds a maxnet model
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data.
#' @param f Optional.  A formula or [recipes::recipe()] object specifying the predictors.  If NULL (the default), every layer in `env` is used.  Maxnet builds its own features (linear, quadratic, hinge, etc.) from the predictors, so a formula only needs to list them.
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

  model.args <- list(...)

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
    model.args = model.args
  )
}

