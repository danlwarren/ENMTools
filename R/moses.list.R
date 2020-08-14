#' Takes a list of enmtools.species objects and uses model selection to ask whether they're better treated jointly or separately
#'
#' @param species.list A list of enmtools.species objects, or an enmtools.clade object.
#' @param env A raster or raster stack of environmental data.
#' @param nback Number of background points to generate, if any
#' @param f A GLM-style function for model fitting
#' @param eval Boolean indicating whether or not GLMs should be evaluated using AUC/TSS/etc.
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param ... further arguments to be passed to enmtools.glm
#'
#' @return A list containing GLMs for the groups separately and together, as well as AIC values for those models.

moses.list <- function(species.list, env, f = NULL, eval = FALSE, nback = 1000, bg.source = "default", verbose = FALSE, ...){

  if(inherits(species.list, "enmtools.clade")){
    species.list <- species.list$species
  }

  # Builds a default formula using all env
  if(is.null(f)){
    f <- as.formula(paste("presence", paste(c(names(env)), collapse = " + "), sep = " ~ "))
  }

  check.moses(species.list, env, f)

  for(i in 1:length(species.list)){
    species.list[[i]] <- check.bg(species.list[[i]], env, nback = nback, bg.source = bg.source, verbose = verbose)
  }

  # We're just going to do all separate vs. all together.  Work on a separate function for phylo tests.
  separate.glms <- lapply(species.list, function(x) enmtools.glm(x, env, f, eval, verbose = verbose, ...))
  names(separate.glms) <- lapply(species.list, function(x) x$species.name)
  separate.aic <- sum(unlist(lapply(separate.glms, function(x) x$model$aic)))

  combined.species <- enmtools.species(presence.points = as.data.frame(do.call("rbind", lapply(species.list, function(x) rbind(x$presence.points)))),
                                       background.points = as.data.frame(do.call("rbind", lapply(species.list, function(x) rbind(x$background.points)))),
                                       species.name = "combined")
  combined.glm <- enmtools.glm(combined.species, env, f, eval, verbose = verbose, ...)
  combined.aic <- combined.glm$model$aic

  output <- list(separate.glms = separate.glms,
                 combined.glm = combined.glm,
                 separate.aic = separate.aic,
                 combined.aic = combined.aic)

  return(output)
}

check.moses <- function(species.list, env, f){

  # Check to see if the function is the right class
  if(!inherits(f, "formula")){
    stop("Argument \'formula\' must contain an R formula object!")
  }

  lapply(species.list, function(x) check.species(x))

  if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
    stop("No environmental rasters were supplied!")
  }

}
