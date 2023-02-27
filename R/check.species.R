#' Checking compliance for an object of class enmtools.species.
#'
#' @param this.species An enmtools.species object to be checked.
#' @param env Environmental rasters that will be used for modeling.  If provided to check.species, ENMTools will remove occurrence points that have NA values for any layer in env.
#' @param trim.dupes Controls whether to trim duplicate occurrence points from the presence data.  Defaults to FALSE, which leaves duplicates in place.  Alternatives are "exact", which will remove points with the same lat/long as another point, or "grid", which will trim data so that there is at most one point per grid cell for the rasters in env, and centers those points in the cells.
#'
#' @return An enmtools.species object with appropriate formatting.
#'
#' @examples
#' check.species(iberolacerta.clade$species$monticola)


check.species <- function(this.species, env = NA, trim.dupes = FALSE){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  # This bit replaces NULL values with NA values
  expect <- c("presence.points", "background.points",
              "models", "species.name", "range")
  nulls <- names(which(sapply(expect, function(x) is.null(this.species[[x]]))))

  # Have to do this in a loop because sapply won't assign NAs for some reason
  for(i in nulls){
    this.species[[i]] <- NA
  }

  if(!isTRUE(is.na(this.species$range))){
    if(!inherits(this.species$range, c("SpatRaster"))){
      stop("Argument range requires an object of class SpatRaster")
    }
    if(is.na(terra::crs(this.species$range))){
      warning("Species range raster does not have a CRS set")
    }
  }

  if(!isTRUE(is.na(this.species$presence.points))){
    if(!inherits(this.species$presence.points, "SpatVector")){
      "Species presence points require an object of class SpatVector"
    }
  }

  if(!isTRUE(is.na(this.species$background.points))){
    if(!inherits(this.species$background.points, "SpatVector")){
      "Species background points require an object of class SpatVector"
    }
  }

  if(!isTRUE(is.na(this.species$species.name))){
    if(!inherits(this.species$species.name, "character")){
      stop("Argument species.name requires an object of class character")
    }
  }

  # Extracts data from env at presence points, uses that to remove points that have NA in any layer
  if(inherits(env, "SpatRaster")){
    temp.df <- terra::extract(env, terra::crds(this.species$presence.points))
    this.species$presence.points <- terra::vect(this.species$presence.points[complete.cases(temp.df),])
  }

  # Removing duplicates
  if(trim.dupes == "exact"){
    this.species$presence.points <- unique(this.species$presence.points)
  }

  if(trim.dupes == "grid"){
    if(inherits(env, "SpatRaster")){
      this.species$presence.points <- trimdupes.by.raster(terra::crds(this.species$presence.points, env))
    } else {
      stop("Trim dupes by grid specified but env was either not supplied or was not a SpatRaster object!")
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
  if(any(!is.na(loncols))){
    loncols <- loncols[which(!is.na(loncols))]
  }

  # Ditto for "lat" and "y"
  latcols <- c(which(grepl("^lat", colnames(latlon), ignore.case = TRUE)), match("y", tolower(colnames(latlon))))
  if(any(!is.na(latcols))){
    latcols <- latcols[which(!is.na(latcols))]
  }


  # Check whether we've got one column for each, and make sure they're not the same column
  if(is.na(latcols)){
    stop("Unable to auotmatically determine Longitude and Latitude columns.  Please rename to Longitude and Latitude.")
  }

  if(is.na(loncols)){
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
