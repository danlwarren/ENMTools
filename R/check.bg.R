#' Takes an emtools.species object and adds background points if they're missing. Looks for range raster first, then goes for environmental layers.
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param nback Number of background points to generate, if any
#'
#' @export check.bg

check.bg <- function(species, env = NA, nback = 1000){

  cat(paste("\nChecking background for ", species$species.name, "...\n"))

  check.species(species)

  if(!any(c("data.frame") %in% class(species$presence.points))){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }


  if(!any(c("data.frame") %in% class(species$background.points))){
    # Now we know we need to add data points

    if(any(c("raster", "RasterLayer", "RasterStack", "RasterBrick") %in% class(species$range))){

      # Drawing background points from range raster
      cat("\n\nNo background points provided, drawing background from range raster.\n\n")
      species$background.points <- as.data.frame(randomPoints(species$range, nback, species$presence.points))
      colnames(species$background.points) <- colnames(species$presence.points)
      return(species)

    }

    if(any(c("raster", "RasterLayer", "RasterStack", "RasterBrick") %in% class(env))) {

      cat("\nNo background points or range raster, drawing background from environmental layers.\n\n")
      species$background.points <- as.data.frame(randomPoints(env[[1]], nback, species$presence.points))
      colnames(species$background.points) <- colnames(species$presence.points)
      return(species)

    } else {

      # No range or env
      stop("No background points, range raster, or environmental data of appropriate type!")

    }
  }

  # Background points already exist
  return(species)
}


