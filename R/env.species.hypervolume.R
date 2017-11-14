env.species.hypervolume <- function(species, env, method = "gaussian", standardise = TRUE, reduce_dim = "auto", ...) {
  species <- add.env(species, env)

  pnts <- species$presence.points[ , -c(1, 2)]
  hyp_dim <- ncol(pnts)
  log_obs <- log(nrow(pnts))

  if(hyp_dim <= 1L) {
    stop("Error: Hypervolume calculation requires at least two dimensions!")
  }

  if(reduce_dim == "auto") {
    if(log_obs < hyp_dim) {
      message("Warning: Too few prsence points for number of environmental variables. Performing principle components dimension reduction...")
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
