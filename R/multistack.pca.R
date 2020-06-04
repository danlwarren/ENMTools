#' multistack.pca, simultaneous PCA on more than one stack of environmental rasters
#'
#' @param n The number of PCA layers to return
#' @param ... Any number of environmental raster stacks or bricks
#'
#' @return A list containing a stack or brick of rasters for each input set representing the top n pca axes of the initial environmental variables, as well as the pca object from the analysis that produced them.
#'
#' @keywords raster pca environment
#'
#' @examples
#' \dontrun{
#' world <- raster::getData(name = "worldclim", download = TRUE, var = "bio", res = 5)
#' test1 <- crop(world, extent(10, 15, 10, 15))
#' test2 <- crop(world, extent(17, 22, 17, 22))
#' test3 <- crop(world, extent(24, 26, 24, 26))
#' multistack.pca(test1, test2, test3)
#' }

multistack.pca <- function(..., n = 2){

  # Get raster stacks into a list, make sure all have the same layers
  stacks <- list(...)
  if(length(stacks) <= 1){
    stop("One or fewer raster stacks passed!")
  }

  # This is a lot of crap just to get the names and get n out if supplied,
  # but it works
  stacknames <- sapply(match.call(expand.dots=TRUE)[-1], deparse)
  if(any(names(stacknames) == "n")){
    stacknames <- stacknames[-which(names(stacknames) == "n")]
  }
  names(stacks) <- stacknames

  # This will be set to true if there are any name mismatches
  mismatch <- FALSE

  for(i in 1:length(stacks)){
    for(j in 1:length(stacks)){
      if(length(setdiff(names(stacks[[i]]), names(stacks[[j]]))) > 0){
        print(paste("Layers in", names(stacks)[[i]],
                    "missing from", names(stacks)[[j]], ":"))

        print(setdiff(names(stacks[[i]]), names(stacks[[j]])))
        mismatch <- TRUE
      }
    }
  }
  if(mismatch){stop("Layer mismatch!")}

  # Get all values
  env.val <- lapply(stacks, function(x) getValues(x))

  # Figure out which cells have complete cases and which have at least one NA
  keepers <- lapply(env.val, function(x) which(complete.cases(x)))
  nas <- lapply(env.val, function(x) which(!complete.cases(x)))

  # Do PCA
  pca <- prcomp(do.call("rbind", env.val), retx = TRUE, scale = TRUE)

  # Build dummy layers
  env.pca <- stacks
  for(i in 1:length(env.pca)){
    env.pca[[i]] <- env.pca[[i]][[1:n]]
  }

  # Add scores and NAs where appropriate
  # the offset is going to be used so that we start
  # on the right row of the PCA values for each set of layers
  offset <- 0
  for(i in 1:length(env.pca)){
    for(j in 1:n){
      thislayer <- env.pca[[i]][[j]]
      thislayer[nas[[i]]] <- NA
      thislayer[keepers[[i]]] <- pca$x[(offset + 1):(offset + length(keepers[[i]])),j]
      env.pca[[i]][[j]] <- thislayer
    }
    offset <- offset + length(keepers[[i]])
    # Rename layers and ship it out
    names(env.pca[[i]]) <- paste0("PC", 1:n)
    env.pca[[i]] <- setMinMax(env.pca[[i]])
  }

  output <- list(raster.stacks = env.pca,
                 pca.object = pca)

  return(output)
}
