#' Takes a set of points, a buffer radius, a buffer type, and a mask and returns
#' either a raster, a polygon, or background points representing the species background.
#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' @param points A two column data frame with X and Y coordinates
#' @param buffer.width Radius for circular buffers to draw around points, for buffer.type = circular.  If buffer.type = "convhull", denotes the amount to which the initial polygon should be buffered.  It's worth noting that this argument may in some cases be treated as map units (e.g., lat and long), but in other caess may be treated as meters.  If you're getting weird behavior, you might try changing the units you're using to specify buffer.width.
#' @param buffer.type Which method to use for buffering species occurrence points.  Currently accepts "circles" and "convhull".
#' @param mask A raster to use as a mask for trimming the buffer if the return type is "raster" or "points"
#' @param return.type What type of object to return.  Can be "raster", "polygon", or "points".
#' @param n Sample size for number of background points to return, for return.type = "points".
#'
#' @return Either a raster, a polygon, or a data frame of points representing the species background.

background.buffer <- function(points, buffer.width, buffer.type = "circles", mask = NA, return.type = "raster", n = 1000){

  buffer.type <- match.arg(buffer.type, c("circles", "convhull"))
  return.type <- match.arg(return.type, c("points", "raster", "polygon"))

  # build buffer of specified type from points
  pol <- NA

  if(buffer.type == "circles"){
    x <- circles(points, d=buffer.width, lonlat=TRUE)
    pol <- rgeos::gUnaryUnion(x@polygons)
  }

  if(buffer.type == "convhull"){
    x <- dismo::convHull(points)
    pol <- rgeos::gUnaryUnion(x@polygons)
    pol <- raster::buffer(pol, width = buffer.width)
  }

  # if return.type = shape, just return from here
  if(return.type == "polygon"){return(pol)}

  # From here we're either returning points or a raster

  # Trim to raster
  if(!inherits(mask, c("raster", "RasterLayer", "RasterBrick", "RasterStack"))){
    stop("Mask needs to be a raster object!")
  }

  if(length(names(mask)) > 1){
    mask <- mask[[1]]
  }

  buffer.raster <- mask(mask, pol)

  # if return.type = "raster", just return from here
  if(return.type == "raster"){
    buffer.raster[!is.na(buffer.raster)] <- 1
    return(buffer.raster)
  }

  # sample points
  if(return.type == "points"){
    xy <- sampleRandom(buffer.raster, size=n, xy=TRUE)

    colnames(xy)[1:2] <- c(colnames(points))

    # If we didn't get as many points as we wanted
    if(nrow(xy) < n){
      xy <- xy[sample(1:nrow(xy), n, replace = TRUE),]
    }

    return(as.data.frame(xy[,1:2]))
  }

}
