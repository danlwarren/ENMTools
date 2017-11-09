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

#' Takes an emtools.species object and adds background points if they're missing. Looks for range raster first, then goes for environmental layers. This version is for ppmlasso models.
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param nback Approximate number of background points to generate, if any. Actual number of points generated can be fairly far off due to optimization for speed.
#'
#' @export check.bg.ppmlasso

check.bg.ppmlasso <- function(species, env = NA, nback = 1000){

  species <- check.species(species)

  if(!inherits(species$presence.points, "data.frame")){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }


  if(!inherits(species$background.points,"data.frame")){
    # Now we know we need to add data points

    if(inherits(species$range, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){

      # Drawing background points from range raster
      cat("\n\nNo background points provided, drawing background from range raster.\n\n")
      ncells <- sum(getValues(species$range) > 0, na.rm=TRUE)
      if(nback > ncells){
        agg_fact <- round(sqrt(nback / ncells))
        if(agg_fact > 1) {
          agg_rast <- disaggregate(species$range[[1]], fact = agg_fact,
                                fun = max)
          species$background.points <- as.data.frame(rasterToPoints(agg_rast)[,1:2])
        } else {
          species$background.points <- as.data.frame(rasterToPoints(species$range)[,1:2])
        }
      } else {
        agg_fact <- round(sqrt(ncells / nback))
        if(agg_fact > 1) {
          agg_rast <- aggregate(species$range[[1]], fact = agg_fact,
                                fun = max)
          species$background.points <- as.data.frame(rasterToPoints(agg_rast)[,1:2])
        } else {
          species$background.points <- as.data.frame(rasterToPoints(species$range)[,1:2])
        }
      }

      colnames(species$background.points) <- colnames(species$presence.points)
      return(species)

      # Accurate but very slow method:
      # bg_rast <- species$range[[1]]
      # values(bg_rast)[!is.na(values(bg_rast))] <- 1
      # rast_poly <- rasterToPolygons(bg_rast, dissolve = TRUE)
      # species$background.points <- spsample(rast_poly, n = nback, type = "regular")@coords

      colnames(species$background.points) <- colnames(species$presence.points)
      return(species)

    }

    if(inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){

      cat("\nNo background points or range raster, drawing background from environmental layers.\n\n")

      ncells <- sum(getValues(env) > 0, na.rm=TRUE)
      if(nback > ncells){
        agg_fact <- round(sqrt(nback / ncells))
        agg_rast <- disaggregate(env[[1]], fact = agg_fact,
                                 fun = max)
        species$background.points <- as.data.frame(rasterToPoints(agg_rast)[,1:2])
      } else {
        agg_fact <- round(sqrt(ncells / nback))
        agg_rast <- aggregate(env[[1]], fact = agg_fact,
                              fun = max)
        species$background.points <- as.data.frame(rasterToPoints(agg_rast)[,1:2])
      }


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


