#' Takes an emtools.species object and adds background points if they're missing. Looks for range raster first, then goes for environmental layers.
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param nback Number of background points to generate, if any
#'
#' @export check.bg

check.bg <- function(species, env = NA, nback = 1000){

  species <- check.species(species)

  if(!inherits(species$presence.points, "data.frame")){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }


  if(!inherits(species$background.points,"data.frame")){
    # Now we know we need to add data points

    if(inherits(species$range, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){

      # Drawing background points from range raster
      cat("\n\nNo background points provided, drawing background from range raster.\n\n")
      if(nback > sum(getValues(species$range) > 0, na.rm=TRUE)){
        species$background.points <- as.data.frame(rasterToPoints(species$range)[,1:2])
      } else {
        species$background.points <- as.data.frame(randomPoints(species$range, nback, species$presence.points))  
      }
      
      colnames(species$background.points) <- colnames(species$presence.points)
      return(species)

    }

    if(inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){

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


