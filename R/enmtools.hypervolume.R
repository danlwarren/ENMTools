#' Takes an enmtools.species object and builds a hypervolume model
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data. Note: hypervolume works best with a small number of
#'   uncorrelated environmental variables (typically 2-6 dimensions).
#' @param f Standard R formula or tidymodels recipe (optional). If NULL, all environmental variables will be used.
#' @param test.prop Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.
#' @param eval Determines whether model evaluation should be done. Turned on by default.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param env.nback Number of points to draw from environment space for environment space discrimination metrics.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param rts.reps The number of replicates to do for a Raes and ter Steege-style test of significance
#' @param bg.source Source for drawing background points. If "points", it just uses the background points that are already in the species object. If "range", it uses the range raster. If "env", it draws points at random from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports. Defaults to FALSE.
#' @param clamp When set to TRUE, clamps the environmental layers so that predictions made outside the min/max of the training data for each predictor are set to the value for the min/max for that predictor.
#' @param corner An integer from 1 to 4. Selects which corner to use for "block" test data. By default the corner is selected randomly.
#' @param bias An optional raster estimating relative sampling effort per grid cell. Will be used for drawing background data.
#' @param method Method for constructing hypervolumes: "gaussian" (default) or "svm".
#' @param samples.per.point Number of random samples per point for hypervolume estimation. Default 1000.
#' @param reduction.factor Value between 0 and 1 for prediction speed. Lower values are faster but less accurate. Default 0.5.
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
                                  bg.source = "default", verbose = FALSE, clamp = TRUE,
                                  corner = NA, bias = NA,
                                  method = "gaussian", samples.per.point = 1000,
                                  reduction.factor = 0.5, ...) {

  assert.extras.this.fun()

  model_args <- list(method = method, samples.per.point = samples.per.point,
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
    model_args = model_args
  )
}


# Summary for objects of class enmtools.hypervolume
#' @exportS3Method
summary.enmtools.hypervolume <- function(object, plot = TRUE, ...){

  print(object$hv)

  if(plot) {
    plot(object)
  }

}

# Print method for objects of class enmtools.hypervolume
#' @exportS3Method
print.enmtools.hypervolume <- function(x, ...){

  print(summary(x, ...))

}


# Plot method for objects of class enmtools.hypervolume
#' @exportS3Method
plot.enmtools.hypervolume <- function(x, ...){

  suit.points <- data.frame(rasterToPoints2(x$suitability))
  colnames(suit.points) <- c("x", "y", "Suitability")
  test <- terra::as.data.frame(x$test.data, geom = "XY")

  suit.plot <- ggplot(data = suit.points,  aes(y = .data$y, x = .data$x)) +
    geom_raster(aes(fill = .data$Suitability)) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = x$analysis.df,  aes(y = .data$y, x = .data$x),
               pch = 21, fill = "white", color = "black", size = 2)

  if(inherits(x$test.data, "SpatVector")){
    suit.plot <- suit.plot + geom_point(data = test,  aes(y = .data$y, x = .data$x),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }

  if(!is.na(x$species.name)){
    title <- paste("Hypervolume model for", x$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }

  plot(x$hv)
  return(suit.plot)

}


# Predict method for models of class enmtools.hypervolume
predict.enmtools.hypervolume <- function(object, env, reduction.factor = 0.1, ...){

  # Make a plot of habitat suitability in the new region
  suitability <- hypervolume::hypervolume_project(object$hv, env, reduction.factor = reduction.factor)
  suit.points <- data.frame(rasterToPoints2(suitability))
  colnames(suit.points) <- c("x", "y", "Suitability")

  suit.plot <- ggplot(data = suit.points,  aes(y = .data$y, x = .data$x)) +
    geom_raster(aes(fill = .data$Suitability)) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic()

  if(!is.na(object$species.name)){
    title <- paste("Hypervolume model projection for", object$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }
  output <- list(suitability.plot = suit.plot,
                 suitability = suitability)

  return(output)
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
