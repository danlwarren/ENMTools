#' Takes an enmtools.species object with presence and background points, and builds
#' a fitted `tidymodels` workflow.
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data.
#' @param f A formula or tidymodels recipe
#' @param model A character string specifying the desired model. Default is "glm"
#' @param test.prop Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.
#' @param eval Determines whether model evaluation should be done.  Turned on by default, but moses turns it off to speed things up.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param env.nback Number of points to draw from environment space for environment space discrimination metrics.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param rts.reps The number of replicates to do for a Raes and ter Steege-style test of significance
#' @param weights If this is set to "equal", presences and background data will be assigned weights so that the sum of all presence points weights equals the sum of all background point weights.  Otherwise, weights are not provided to the model.
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param clamp When set to TRUE, clamps the environmental layers so that predictions made outside the min/max of the training data for each predictor are set to the value for the min/max for that predictor. Prevents the model from extrapolating beyond the min/max bounds of the predictor space the model was trained in, although there could still be projections outside the multivariate training space if predictors are strongly correlated.
#' @param corner An integer from 1 to 4.  Selects which corner to use for "block" test data.  By default the corner is selected randomly.
#' @param bias An optional raster estimating relative sampling effort per grid cell.  Will be used for drawing background data.
#' @param step Logical determining whether to do stepwise model selection or not
#' @param model_args An optional named list with additional arguments to the
#' modelling function (as specified in `model` argument).
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @examples
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' enmtools.glm(iberolacerta.clade$species$monticola, env = euro.worldclim, f = pres ~ bio1 + bio9)



enmtools.tidy <- function(species, env, f = NULL, model = "glm", test.prop = 0, eval = TRUE, nback = 1000, env.nback = 10000, report = NULL, overwrite = FALSE, rts.reps = 0, weights = "equal", bg.source = "default",  verbose = FALSE, clamp = TRUE, corner = NA, bias = NA, step = FALSE, model_args = list()){

  if(!is.null(f)) {
    if(inherits(f, "recipe")) {
      rec <- f
    } else {
      rec <- recipe(species, f, env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias)
    }
  }
  analysis.df <- enmtools.prep(species, env = env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias)

  mod <- parsnip::logistic_reg()

}

recipe.enmtools.species <- function (x, formula = NULL, env = NA, nback = 1000, bg.source = "default", verbose = FALSE, bias = NA, ..., vars = NULL, roles = NULL) {
  x <- enmtools.prep(x, env = env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias)
  if(is.null(formula)) {
    vars <- colnames(x)
    roles <- rep("predictor", length(vars))
    roles[vars == "presence"] <- "outcome"
    roles[vars %in% c("x", "y")] <- "coords"
  } else {
    vars <- NULL
    roles <- NULL
  }
  recipes::recipe(x, formula = formula, vars = vars, roles = roles)
}

enmtools.prep <- function(x, env = NA, nback = 1000, bg.source = "default", verbose = FALSE, bias = NA) {
  species <- check.bg(x, env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias)
  species <- add.env(species, env = env, verbose = verbose)
  x <- make_analysis.df(species)
  x
}

choose_model <- function(model, ...) {
  switch(model,
         glm = parsnip::logistic_reg(...),
         gam = parsnip::gen_additive_mod(...),
         rf = parsnip::rand_forest(engine = "randomForest", ...),
         ranger = parsnip::rand_forest(...))
}
