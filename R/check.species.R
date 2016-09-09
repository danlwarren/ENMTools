#' Checking compliance for an object of class enmtools.species.
#'
#' Checks for existence and proper class of:
#' @param range A raster or SpatialPolygon with the actual range they occur in
#' @param presence.points A data frame with sampled localities
#' @param background.points A data frame with absence/pseudoabsence/background localities
#' @param species.name A character vector with the species name
#' @param models A list of models that are made for the species, which will be stuffed in there as we go along
#' to pass the check.  This is used by internal enmtools functions to make sure the necessary data is present
#' before processing anything.
#'
#' @export check.species


check.species <- function(this.species){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  if(!isTRUE(is.na(this.species$range))){
    if(!inherits(this.species$range, c("raster", "RasterLayer", "RasterBrick"))){
      stop("Argument range requires an object of class raster or RasterLayer")
    }
  }

  if(!isTRUE(is.na(this.species$presence.points))){
    if(!inherits(this.species$presence.points, "data.frame")){
      stop("Argument presence.points requires an object of class data.frame")
    }

    # Presence points exist, and are a data frame
    this.species$presence.points <- format.latlon(this.species$presence.points)
  }

  if(!isTRUE(is.na(this.species$background.points))){
    if(!inherits(this.species$background.points, "data.frame")){
      stop("Argument background.points requires an object of class data.frame")
    }

    # Background points exist, and are a data frame
    this.species$background.points <- format.latlon(this.species$background.points)
  }

  if(!isTRUE(is.na(this.species$background.points)) & !isTRUE(is.na(this.species$presence.points))){
    if(any(!colnames(this.species$presence.points) %in% colnames(this.species$background.points))){
      stop("Column names for presence and background points do not match")
    }
  }

  if(!isTRUE(is.na(this.species$species.name))){
    if(!inherits(this.species$species.name, "character")){
      stop("Argument species.name requires an object of class character")
    }
  }

  # Return the formatted species object
  return(this.species)
}


format.latlon <- function(latlon){

  # Basically this bit just tries to auto-identify the lat and lon columns, then returns a
  # reformatted data frame with col names "Longitude" and "Latitude"

  # Try to figure out which columns contain "lon" or "x"
  loncols <- c(which(grepl("^lon", colnames(latlon), ignore.case = TRUE)), match("x", tolower(colnames(latlon))))
  if(!is.na(loncols)){
    loncols <- loncols[which(!is.na(loncols))]
  }

  # Ditto for "lat" and "y"
  latcols <- c(which(grepl("^lat", colnames(latlon), ignore.case = TRUE)), match("y", tolower(colnames(latlon))))
  if(!is.na(latcols)){
    latcols <- latcols[which(!is.na(latcols))]
  }


  # Check whether we've got one column for each, and make sure they're not the same column
  if(is.na(latcols) | is.na(loncols)){
    stop("Unable to auotmatically determine Longitude and Latitude columns.  Please rename to Longitude and Latitude.")
  }
  if(length(latcols == 1) & length(loncols == 1) & latcols != loncols){
    output <- data.frame(cbind(latlon[,loncols], latlon[,latcols]))
    colnames(output) <- c("Longitude", "Latitude")
  } else {
    stop("Unable to auotmatically determine Longitude and Latitude columns.  Please rename to Longitude and Latitude.")
  }
  return(output)
}
