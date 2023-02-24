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

  # Checking classes of input args

  if(!inherits(env, c("raster", "RasterLayer", "RasterBrick", "RasterStack"))){
    stop("Argument env requires an object of class raster or RasterLayer")
  }

  # Checking to make sure all rasters in the stack have the same extent.
  # Actually terra::rast shouldn't allow that to be false but it doesn't hurt to double check.
  if(verbose == TRUE){
    cat("Checking to make sure rasters have the same extent... \n")
  }

  extent.check <- data.frame(matrix(ncol = length(names(env))))
  colnames(extent.check) <- names(env)
  for(i in 1:length(names(env))){
    for(j in i:length(names(env))){
      extent.check[i,j] <- terra::compareGeom(env[[i]], env[[j]], crs = FALSE, rowcol = FALSE)
    }
  }
  rownames(extent.check) <- names(env)
  if(any(isFALSE(extent.check))){
    print(extent.check)
    stop("Some environmental rasters have different extents, indicated by FALSE in the table above")
  }

  # As above, but for resolution
  if(verbose == TRUE){
    cat("Checking to make sure rasters have the same resolution... \n")
  }

  resolution.check <- data.frame(matrix(ncol = length(names(env))))
  colnames(resolution.check) <- names(env)
  for(i in 1:length(names(env))){
    for(j in i:length(names(env))){
      resolution.check[i,j] <- terra::compareGeom(env[[i]], env[[j]], res = TRUE, crs = FALSE, ext = FALSE, rowcol = FALSE)
    }
  }
  rownames(resolution.check) <- names(env)
  if(any(isFALSE(resolution.check))){
    print(resolution.check)
    stop("Some environmental rasters have different resolutions, indicated by FALSE in the table above")
  }

  if(verbose == TRUE){
    cat("Making NAs consistent across layers... \n")
  }

  # Here we're just exploiting the fact that sum will by default return NA when any layer has an NA
  env = terra::mask(env, sum(env))

  # Return the formatted raster stack
  return(env)
}


