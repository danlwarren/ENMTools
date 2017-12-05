#' Takes a list of enmtools.species objects and uses model selection to ask whether they're better treated jointly or separately
#'
#' @param species.list A list of enmtools.species objects, or an enmtools.clade object.
#' @param env A raster or raster stack of environmental data.
#' @param nback Number of background points to generate, if any
#' @param f A GLM-style function for model fitting
#' @param eval Boolean indicating whether or not GLMs should be evaluated using AUC/TSS/etc.
#' @param ... further arguments to be passed to enmtools.glm
#'
#' @export moses.list

moses.list <- function(species.list, env, f = NULL, eval = FALSE, nback = 1000, ...){

  if(inherits(species.list, "enmtools.clade")){
    species.list <- species.list$species
  }

  # Builds a default formula using all env
  if(is.null(f)){
    f <- as.formula(paste("presence", paste(c(names(env)), collapse = " + "), sep = " ~ "))
  }

  check.moses(species.list, env, f)

  for(i in 1:length(species.list)){
    species.list[[i]] <- check.bg(species.list[[i]], env, nback = nback, ...)
  }

  # We're just going to do all separate vs. all together.  Work on a separate function for phylo tests.
  separate.glms <- lapply(species.list, function(x) enmtools.glm(x, env, f, eval, ...))
  names(separate.glms) <- lapply(species.list, function(x) x$species.name)
  separate.aic <- sum(unlist(lapply(separate.glms, function(x) x$model$aic)))

  combined.species <- enmtools.species(presence.points = as.data.frame(do.call("rbind", lapply(species.list, function(x) rbind(x$presence.points)))),
                                       background.points = as.data.frame(do.call("rbind", lapply(species.list, function(x) rbind(x$background.points)))),
                                       species.name = "combined")
  combined.glm <- enmtools.glm(combined.species, env, f, eval, ...)
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
