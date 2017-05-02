#' Takes a set of points, a buffer radius, a sample size, and a mask and returns
#' randomly sampled points from within that buffer radius.

#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' @param points A two column data frame with X and Y coordinates
#' @param radius Radius for circular buffers to draw around points, in meters. This 
#'               creates (internally) a polygon layer called '\code{pol}'. 
#' @param n Sample size for number of background points to return
#' @param mask A raster to use as a mask for drawing points. Not needed if 
#'             \code{use_spsample=TRUE} and \code{extract_values=FALSE}.
#' @param cropfirst Would you like to crop the raster to the extent of the points+buffers? 
#'        Typically a big speedup for large rasters, but still not super-fast. Default=\code{TRUE}.
#' @param use_spsample Randomly sample points from the points+buffer polygon 'pol'. 
#'                     MUCH faster, slight risk of sampling the same pixel twice, but 
#'                     this shouldn't matter for most uses.
#' @param type The 'type' option for the \code{sp::spsample} function. Text from spsample 
#'             documentation: character; "\code{random}" for completely spatial random; "\code{regular}" for regular (systematically aligned) sampling; "\code{stratified}" for stratified random (one single random location in each "cell"); "\code{nonaligned}" for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates); "\code{hexagonal}" for sampling on a hexagonal lattice; "\code{clustered}" for clustered sampling; "\code{Fibonacci}" for Fibonacci sampling on the sphere (see references).
#' @extract_values Do you want the values extracted, or just the coordinates?
#' @examples
#' # Example speed test below (don't run)
#' test=1
#' 
#' \dontrun{
#' library(devtools)	# install_github
#' #install_github("danlwarren/ENMTools")
#' library(ENMTools)	# hierarchical species distribution modeling
#' # enmtools.aoc
#' 
#' library(BioGeoBEARS)
#' sourceall("/GitHub/ENMTools/R/")
#' 
#' wd = "/GitHub/ENMTools/inst/extdata/"
#' setwd(wd)
#' 
#' # Download only first time you run, then change to FALSE
#' download_data_files = FALSE
#' # Species occurrences from a CSV file (from ENMTools GitHub page)
#' remote_dir = "https://raw.githubusercontent.com/danlwarren/ENMTools/master/test/testdata/"
#' # Species occurrences from a LOCAL file (on your computer)
#' local_dir = "tmp"
#' 
#' 
#' 
#' # Create an empty species object
#' ahli = enmtools.species()
#' ahli
#' 
#' # Load a species object with occurrence data
#' species_occs_fn = "ahli.csv"
#' remote_fn = paste0(remote_dir, species_occs_fn)
#' local_fn = paste0(local_dir, "/", species_occs_fn)
#' #local_fn = "hi/this/is/my/folder/and_file_name.csv" # for a file you have on disk
#' if (file.exists(local_dir) == FALSE)
#' 	{
#' 	dir.create(local_dir)
#' 	}
#' 
#' # Downloading the file. You may need to change the 'method' argument
#' # on some operating systems
#' if (download_data_files == TRUE)
#' 	{
#' 	download.file(url=remote_fn, destfile=local_fn, method="curl")
#' 	}
#' 
#' # Read in the file
#' occs = read.csv(file=local_fn)
#' head(occs)
#' 
#' # Make an ENMTools species object
#' ahli = enmtools.species(species.name="ahli", presence.points=occs[,3:4])
#' check.species(ahli)
#' 
#' 
#' #######################################################
#' # Download and load environmental data (ASCII rasters)
#' # (from ENMTools GitHub page)
#' #######################################################
#' env_fns = c("pc1.asc", "pc2.asc", "pc3.asc", "pc4.asc")
#' cat("\nDownloading ", length(env_fns), "...", sep="")
#' for (i in 1:length(env_fns))
#' 	{
#' 	env_fn = env_fns[i]
#' 	cat("\nFile #", i, "/", length(env_fns), ": ", env_fn, "...\n", sep="")
#' 	remote_fn = paste0(remote_dir, env_fn)
#' 	local_fn = paste0(local_dir, "/", env_fn)
#' 	if (download_data_files == TRUE)
#' 		{
#' 		download.file(url=remote_fn, destfile=local_fn, method="curl", quiet=FALSE)
#' 		}
#' 	#cat("done.")
#' 	} # END for (i in 1:length(env_fns))
#' 
#' local_env_fns <- list.files(path=local_dir, pattern="pc", full.names=TRUE)
#' local_env_fns
#' env <- raster::stack(local_env_fns)
#' names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4")
#' env <- raster::setMinMax(env)
#' 
#' 
#' #######################################################
#' # Experiment 1: Speed of assigning background points
#' #######################################################
#' 
#' # Assign background points
#' # Original version: 
#' # 1. create a circular buffer around the occurrences
#' # 2. create a shapefile from the buffer
#' # 3. Cut the env raster down (mask it) to the polygon in the buffer shapefile
#' # 4. Randomly extract pixels from the masked raster
#' # The settings below produce the original behavior:
#' ahli$background.points_old = background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=FALSE, use_spsample=FALSE, type="random")
#' 
#' # How long does this take (with a small raster?)
#' orig_time1 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=FALSE, use_spsample=FALSE, type="random"))
#' orig_time1
#' #    user  system elapsed 
#' #   0.925   0.046   0.963 
#' 
#' # Even worse if we use all 4 layers
#' orig_time2 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=FALSE, use_spsample=FALSE, type="random"))
#' orig_time2
#' #    user  system elapsed 
#' #   3.040   0.112   3.128 
#' 
#' # I found that with a larger raster, e.g. 6 tiles of bioclim data (Australasia), this 
#' # operation could take several minutes, and would have to be repeated for each species
#' # The first attempt, "cropfirst=TRUE", uses a crop() function before step 3. This can help
#' # speed somewhat, but is not needed below.
#' 
#' # I revised the function to rely more on vector methods
#' # New version: 
#' # 1. create a circular buffer around the occurrences
#' # 2. create a shapefile from the buffer
#' # 3. Use sp::spsample to sample random long/lat coordinates inside the shapefile
#' # 4. Extract the pixels at these points 
#' 
#' 
#' # Let's use the new defaults.
#' ahli$background.points = background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=FALSE)
#' # Seems faster! How much faster?
#' 
#' new_time1 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random"), extract_values=FALSE)
#' new_time1
#' # user  system elapsed 
#' #   0.052   0.001   0.052 
#' 
#' new_time2 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=TRUE, use_spsample=TRUE, type="random"), extract_values=FALSE)
#' new_time2
#' #    user  system elapsed 
#' #   0.052   0.002   0.052 
#' 
#' # It gets slower if you do extract the values (although still much faster
#' # for large rasters)
#' new_time3 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=TRUE))
#' new_time3
#' #    user  system elapsed 
#' #   0.350   0.005   0.349 
#' 
#' # Even slower for a multi-layer raster
#' new_time4 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=TRUE))
#' new_time4
#' #    user  system elapsed 
#' #   1.256   0.019   1.259 
#' 
#' #######################################################
#' # Experiment 2: Speed of making background raster
#' #######################################################
#' 
#' ahli$range = background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=FALSE)
#' 
#' # The only thing that might be helpful here is cropping first
#' # (but, this might cause problems downstream)
#' raster_time1 = system.time(background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=FALSE))
#' raster_time1
#' #    user  system elapsed 
#' #   1.641   0.033   1.661 
#' 
#' raster_time2 = system.time(background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=TRUE))
#' raster_time2
#' #    user  system elapsed 
#' #   1.421   0.018   1.430 
#' 
#' # The raster overlap stuff, though, could perhaps be done in other ways...
#' }
#' 
#' @export background.points.buffer
#' 

background.points.buffer <- function(points, radius, n, mask=NULL, cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=FALSE) {
  
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
    # Mask (env raster) is needed here
    if (is.null(mask))
    	{
    	txt = paste0("STOP ERROR in background.points.buffer(); a raster needs to be input into 'mask', because use_spsample==FALSE")
    	cat("\n\n")
    	cat(txt)
    	cat("\n\n")
    	stop(txt)
    	}
    
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
		if (extract_values == TRUE)
			{
			# Extract the actual values
			xy <- raster::extract(x=mask, y=xypoints, method="simple")
			# Mask (env raster) is needed here
			if (is.null(mask))
				{
				txt = paste0("STOP ERROR in background.points.buffer(); a raster needs to be input into 'mask', because extract_values==TRUE")
				cat("\n\n")
				cat(txt)
				cat("\n\n")
				stop(txt)
				}
			# Add the xy coordinates on the front
			xy = cbind(as.data.frame(xypoints), xy)
			} else {
			xy = as.data.frame(xypoints)
			}
		} # END if (use_spsample == FALSE)


  # OLD:
  #colnames(xy)[1:2] <- c(colnames(points))
  # NEW:
  colnames(xy)[1:2] <- tmpnames[c(xcolnum, ycolnum)]
	
	if (extract_values == TRUE)
		{
		# Return the raster values as well as coordinates
		return(as.data.frame(xy))
		} else {
		# Return just the coordinates
		return(as.data.frame(xy[,1:2]))
		}
	
  
}





