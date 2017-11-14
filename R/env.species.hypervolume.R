env.species.hypervolume <- function(species, env, method = "gaussian", ...) {
  species <- add.env(species, env)
  #hyp_vol <- hypervolume(species$presence.points[ , -c(1, 2)], method = method, ...)
  hyp_vol <- hypervolume(species$presence.points[ , -c(1, 2)], method = method)
}
