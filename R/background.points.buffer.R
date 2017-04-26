#' Takes a set of points, a buffer radius, a sample size, and a mask and returns
#' randomly sampled points from within that buffer radius.

#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' @param points A two column data frame with X and Y coordinates
#' @param radius Radius for circular buffers to draw around points, in meters. This 
#'               creates (internally) a polygon layer called 'pol'. 
#' @param n Sample size for number of background points to return
#' @param mask A raster to use as a mask for drawing points
#' @param cropfirst Would you like to crop the raster to the extent of the points+buffers? 
#'        Typically a big speedup for large rasters, but still not super-fast. Default=TRUE.
#' @param use_spsample Randomly sample points from the points+buffer polygon 'pol'. 
#'                     MUCH faster, slight risk of sampling the same pixel twice, but 
#'                     this shouldn't matter for most uses.
#' @param type The 'type' option for the sp::spsample function. Text from spsample 
#'             documentation: character; "random" for completely spatial random; "regular" for regular (systematically aligned) sampling; "stratified" for stratified random (one single random location in each "cell"); "nonaligned" for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates); "hexagonal" for sampling on a hexagonal lattice; "clustered" for clustered sampling; "Fibonacci" for Fibonacci sampling on the sphere (see references).
#' 
#' @export background.points.buffer
#' 

background.points.buffer <- function(points, radius, n, mask, cropfirst=TRUE, use_spsample=TRUE, type="random"){
  
  # 2017-04-26_NJM:
  # Check 'points' for lon/lat, make sure 
  # 1st column = x = longitude
  # 2nd column = y = latitude
  tmpnames = colnames(points)
  
  # First column with "lon" in it is x, longitude
  xTF = grepl(pattern="lon", x=tmpnames)
  if (any(xTF) == TRUE)
  	{
    xcolnum = (1:length(tmpnames))[xTF][1]
    } else {
    xcolnum = 1
    }
  # First column with "lat" in it is y, latitude
  yTF = grepl(pattern="lat", x=tmpnames)
  if (any(yTF) == TRUE)
  	{
    ycolnum = (1:length(tmpnames))[yTF][1]
    } else {
    ycolnum = 2
    }
  xyvals = points[,c(xcolnum, ycolnum)]
  
  x <- dismo::circles(xyvals, d=radius, lonlat=TRUE)
  pol <-  rgeos::gUnaryUnion(x@polygons)


  # Crop raster to extent of buffered points
  # editorial: "mask" should be changed to "env"
  if (use_spsample == FALSE)
    {
    # Traditional method, with or without cropping the raster
		if (cropfirst == TRUE)
			{
			cropped = raster::crop(x=mask, y=extent(pol))
			} else {
			cropped = mask
			}
		buffer.raster <- raster::mask(x=cropped, mask=pol)
		xy <- raster::sampleRandom(buffer.raster, size=n, xy=TRUE)
		} else {
		# New method: sample points directly
		xypoints = sp::spsample(x=pol, n=n, type=type)
		xy <- raster::extract(x=mask, y=xypoints, method="simple")
		# Add the xy coordinates on the front
		xy = cbind(as.data.frame(xypoints), xy)
		}


  # OLD:
  #colnames(xy)[1:2] <- c(colnames(points))
  # NEW:
  colnames(xy)[1:2] <- tmpnames[c(xcolnum, ycolnum)]

  return(as.data.frame(xy[,1:2]))
}





