#' Takes an enmtools.species object and builds a hypervolume model
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data. Note: hypervolume works best with a small number of
#'   uncorrelated environmental variables (typically 2-6 dimensions).
#' @param f Optional.  A formula or [recipes::recipe()] object, used only to select which environmental variables are used (those on the right hand side of a formula, or with the predictor role in a recipe).  If NULL (the default), every layer in `env` is used.  Transformations, interactions, and recipe steps are ignored, with a warning; to fit the model to transformed variables such as principal components, transform the environmental layers first.  This argument is new as of version 2.0
#' @param test.prop Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.
#' @param eval Determines whether model evaluation should be done. Turned on by default.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param env.nback Number of points to draw from environment space for environment space discrimination metrics.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param rts.reps The number of replicates to do for a Raes and ter Steege-style test of significance
#' @param bg.source Source for drawing background points. If "points", it just uses the background points that are already in the species object. If "range", it uses the range raster. If "env", it draws points at random from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports. Defaults to FALSE.
#' @param clamp Defaults to FALSE.  Suitability from this model already declines for conditions outside the range of the presence points, and clamping the environmental layers would raise suitability for those conditions.  If TRUE, the environmental layers are clamped to the minimum and maximum of each predictor at the presence points.
#' @param corner An integer from 1 to 4. Selects which corner to use for "block" test data. By default the corner is selected randomly.
#' @param bias An optional raster estimating relative sampling effort per grid cell. Will be used for drawing background data.
#' @param method Method for constructing hypervolumes: "gaussian" (default) or "svm".
#' @param samples.per.point Number of random points generated around each presence point when building the hypervolume.  More points describe the hypervolume more precisely but make predictions slower.  If NULL (the default), the hypervolume package's default is used, which scales with the number of variables and presence points so that the total number of random points stays similar across sample sizes.
#' @param reduction.factor Proportion of the hypervolume's random points used when predicting suitability, between 0 and 1.  Defaults to 1, which uses all of them.  Lower values make predictions faster, but a different random subset is used each time, so suitability near the edge of the hypervolume changes from one prediction to the next.
#' @param ... Additional arguments to be passed to hypervolume construction functions
#'
#' @return An enmtools model object containing species name, model object, suitability raster, and any evaluation objects that were created.
#'
#' @details Hypervolume models estimate the environmental niche as an n-dimensional hypervolume
#' using kernel density estimation. This is a presence-only method that does not use background points
#' for model fitting (though background points are still used for model evaluation).
#'
#' Note that hypervolume models work best with a small number of uncorrelated environmental
#' variables (typically 2-6). Using too many variables can lead to poor performance due to the
#' curse of dimensionality.
#'
#' The function internally standardizes environmental data before fitting the hypervolume,
#' and stores the scaling parameters for use in prediction.
#'
#' @seealso \code{\link[hypervolume]{hypervolume_gaussian}} for the underlying modeling function.
#'
#' @examples
#' \dontrun{
#' # Use a small number of environmental variables
#' env_subset <- euro.worldclim[[c("bio1", "bio12")]]
#' monticola.hv <- enmtools.hypervolume(iberolacerta.clade$species$monticola, env = env_subset)
#' }
#'
#' @export
enmtools.hypervolume <- function(species, env, f = NULL, test.prop = 0, eval = TRUE,
                                  nback = 1000, env.nback = 10000, report = NULL,
                                  overwrite = FALSE, rts.reps = 0,
                                  bg.source = "default", verbose = FALSE, clamp = FALSE,
                                  corner = NA, bias = NA,
                                  method = "gaussian", samples.per.point = NULL,
                                  reduction.factor = 1, ...) {

  assert.extras.this.fun()

  model.args <- list(method = method, samples.per.point = samples.per.point,
                     reduction.factor = reduction.factor, ...)

  enmtools.tidy(
    species = species,
    env = env,
    f = f,
    model = "hypervolume",
    test.prop = test.prop,
    eval = eval,
    nback = nback,
    env.nback = env.nback,
    report = report,
    overwrite = overwrite,
    rts.reps = rts.reps,
    weights = "none",  # hypervolume is presence-only, no weights needed
    bg.source = bg.source,
    verbose = verbose,
    clamp = clamp,
    corner = corner,
    bias = bias,
    model.args = model.args
  )
}


# Summary for objects of class enmtools.hypervolume
#' @exportS3Method
summary.enmtools.hypervolume <- function(object, plot = TRUE, ...){

  output <- NextMethod()

  cat("\n\nHypervolume:  \n")
  print(object$hv)

  output

}


# Plot method for objects of class enmtools.hypervolume
#' @exportS3Method
plot.enmtools.hypervolume <- function(x, ...){

  suit.plot <- NextMethod()
  plot(x$hv)
  return(suit.plot)

}


# Function for checking data prior to running enmtools.hypervolume
hypervolume.precheck <- function(species, env){

  ### Check to make sure the data we need is there
  if(!inherits(species, "enmtools.species")){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  check.species(species)

  if(!inherits(species$presence.points, "SpatVector")){
    stop("Species presence.points do not appear to be an object of class SpatVector")
  }

  if(!inherits(env, c("SpatRaster"))){
    stop("No environmental rasters were supplied!")
  }

}
