#' Automating some basic tasks for using a raster stack for modeling.  Checks rasters for same extent and resolution, and sets values in each layer to NA if there is an NA in any other layer.
#'
#' @param env A stack of environmental rasters
#' @param verbose Controls printing of progress messages
#'
#' @return A raster stack.
#'
#' @examples
#' data(euro.worldclim)
#' check.env(euro.worldclim)


check.env <- function(env, verbose = FALSE){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  if(!inherits(env, c("raster", "RasterLayer", "RasterBrick", "RasterStack"))){
    stop("Argument env requires an object of class raster or RasterLayer")
  }

  if(verbose == TRUE){
    cat("Checking to make sure rasters have the same extent... \n")
  }

  extent.check <- data.frame(matrix(ncol = length(names(env))))
  colnames(extent.check) <- names(env)
  for(i in 1:length(names(env))){
    for(j in i:length(names(env))){
      extent.check[i,j] <- extent(env[[i]]) == extent(env[[j]])
    }
  }
  rownames(extent.check) <- names(env)
  if(any(isFALSE(extent.check))){
    print(extent.check)
    stop("Some environmental rasters have different extents, indicated by FALSE in the table above")
  }

  if(verbose == TRUE){
    cat("Making NAs consistent across layers... \n")
  }

  namask = sum(env)

  env = raster::mask(env, namask)

  # Return the formatted raster stack
  return(env)
}


