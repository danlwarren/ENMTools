#' raster.pca, PCA on a set of environmental rasters
#'
#' @param env A set of environmental layers
#' @param n The number of PCA layers to return
#'
#' @return A list containing a stack of rasters representing the top n pca axes of the initial environmental variables, as well as the pca object from the analysis that produced them.
#'
#' @keywords raster pca environment
#'
#' @export raster.pca
#'
#' @examples
#' env.pca <- raster.pca(euro.worldclim, 2)

raster.pca <- function(env, n){

  # Get all values
  env.val <- getValues(env)

  # Figure out which cells have complete cases and which have at least one NA
  keepers <- which(complete.cases(env.val))
  nas <- which(!complete.cases(env.val))

  # Do PCA
  pca <- princomp(env.val[keepers,], cor=T)

  # Build dummy layers
  env.pca <- env[[1:n]]

  # Add scores and NAs where appropriate
  env.pca[nas] <- NA
  for(i in 1:n){
    env.pca[[n]][keepers] <- pca$scores[,n]
  }

  # Rename layers and ship it out
  names(env.pca) <- paste0("PC", 1:n)

  env.pca <- setMinMax(env.pca)

  output <- list(rasters = env.pca,
                 pca.object = pca)

  return(output)
}
