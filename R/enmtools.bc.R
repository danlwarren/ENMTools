#' Takes an emtools.species object with presence and background points, and builds a Bioclim model
#'
#' @param species An enmtools.species object
#' @param f Optional.  A formula or [recipes::recipe()] object, used only to select which environmental variables are used (those on the right hand side of a formula, or with the predictor role in a recipe).  If NULL (the default), every layer in `env` is used.  Transformations, interactions, and recipe steps are ignored, with a warning; to fit the model to transformed variables such as principal components, transform the environmental layers first.  This argument is new as of version 2.0
#' @param env A SpatRaster of environmental data.
#' @param test.prop Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param nback Number of background points for models. In the case of bioclim models these are only used for evaluation.
#' @param env.nback Number of points to draw from environment space for environment space discrimination metrics.
#' @param rts.reps The number of replicates to do for a Raes and ter Steege-style test of significance
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param clamp Defaults to FALSE.  Suitability from this model already declines for conditions outside the range of the presence points, and clamping the environmental layers would raise suitability for those conditions.  If TRUE, the environmental layers are clamped to the minimum and maximum of each predictor at the presence points.
#' @param corner An integer from 1 to 4.  Selects which corner to use for "block" test data.  By default the corner is selected randomly.
#' @param bias An optional raster estimating relative sampling effort per grid cell.  Will be used for drawing background data.
#' @param ... Arguments to be passed to dismo::bioclim()
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @examples
#' \donttest{
#' enmtools.bc(iberolacerta.clade$species$monticola, env = euro.worldclim)
#' }

enmtools.bc <- function(species, env = NA, f = NULL, test.prop = 0, report = NULL, overwrite = FALSE, nback = 1000, env.nback = 10000, rts.reps = 0, bg.source = "default",  verbose = FALSE, clamp = FALSE, corner = NA, bias = NA, ...){

  return(
    enmtools.tidy(species, env, f = f, model = "bc", test.prop = test.prop, eval = TRUE, nback = nback, env.nback = env.nback, report = report, overwrite = overwrite, rts.reps = rts.reps, bg.source = bg.source,  verbose = verbose, clamp = clamp, corner = corner, bias = bias, ...)
  )
}
