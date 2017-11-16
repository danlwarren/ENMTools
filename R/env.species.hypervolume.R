#' Calculates an environmental hypervolume of presence points using Blonder et al.'s \code{\link[hypervolue]{hypervolume-package}}
#'
#' @param species An enmtools.species object containing presence points
#' @param env A raster or raster stack of environmental data.
#' @param method Method to use in calculating hypervolume, See \code{\link[hypervolume]{hypervolume}} for details
#' @param use.background Should the hypervolume be calulated on background points, instead of presence points?
#' @param standardise Should the environmental variables be standardised before calculating the hypervolume?
#' @param reduce_dim Should environmental variables be dimension reduced. Currently only option is "auto", which does dimension reduction using principle components analysis, only if the number of environmental variables exceeds a threshold of \code{log(# of observations)}.
#' @param ... Additional parameters to pass on to the \code{\link[hypervolume]{hypervolume}} function
#' @export env.species.hypervolume
env.species.hypervolume <- function(species, env, method = "gaussian", use.background = FALSE, standardise = TRUE, reduce_dim = "auto", ...) {

  check.species(species)

  species <- add.env(species, env)

  if(use.background) {
    if(is.null(species$background.points)) {
      stop("Error: use.background is TRUE but input enmtools.species object contains no background points.")
    }
    pnts <- species$background.points[ , -c(1, 2)]
  } else {
    pnts <- species$presence.points[ , -c(1, 2)]
  }
  hyp_dim <- ncol(pnts)
  log_obs <- log(nrow(pnts))

  if(hyp_dim <= 1L) {
    stop("Error: Hypervolume calculation requires at least two dimensions!")
  }

  if(reduce_dim == "auto") {
    if(log_obs < hyp_dim) {
      message("Warning: Too few points for number of environmental variables. Performing principle components dimension reduction...")
      pcs <- prcomp(pnts)
      pnts <- pcs$x[ , c(1:floor(log_obs))]
    }
  }

  if(standardise) {
    message("Standardising environmental variables to mean of 0 and standard deviation of 1...")
    pnts <- scale(pnts)
  }
  #hyp_vol <- hypervolume(species$presence.points[ , -c(1, 2)], method = method, ...)
  hyp_vol <- hypervolume(pnts, method = method)

  plot(hyp_vol)

  return(hyp_vol)
}
