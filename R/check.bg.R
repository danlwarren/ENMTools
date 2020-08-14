#' Takes an emtools.species object and adds background points if they're missing. Looks for range raster first, then goes for environmental layers.
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param nback Number of background points to generate, if any
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#'
#' @return An enmtools.species object with background points.

check.bg <- function(species, env = NA, nback = 1000, bg.source = "default", verbose = FALSE){

  species <- check.species(species)

  if(!inherits(species$presence.points, "data.frame")){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }



  if(bg.source %in% c("points", "range", "env")){

    # Background source manually supplied
    if(verbose == TRUE){message(paste("Pulling background points from", bg.source))}

  } else {

    # Attempting to automatically determine background source
    if(inherits(species$background.points, "data.frame")){

      bg.source = "points"
      if(verbose == TRUE){message("\n\nDrawing background from species background points.\n\n")}

    } else if(inherits(species$range, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){

      # Drawing points from range raster
      bg.source = "range"
      if(verbose == TRUE){message("\n\nNo background points provided, drawing background from range raster.\n\n")}

    } else if(inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))) {

      # Drawing from env
      if(verbose == TRUE){message("\nNo background points or range raster, drawing background from environmental layers.\n\n")}
      bg.source = "env"

    } else {
      # No usable env
      stop("No background points, range raster, or environmental data of appropriate type!")
    }
  }



  # Now we're adding the points
  if(bg.source == "points"){
    # Using points from the enmtools.species object
    if(!inherits(species$background.points, "data.frame")){
      stop("bg.source set to points, but species background.points does not contain a data frame!")
    }
    return(species)
  }

  if(bg.source == "range"){
    # Drawing background points from range raster
    if(!inherits(species$range, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
      stop("bg.source set to range, but species does not have a recognizable range raster!")
    }

    if(!raster::compareCRS(crs(env), crs(species$range))){
      stop("CRS mismatch between species range raster and environmental rasters!")
    }

    if(nback > sum(as.numeric(!is.na(values(species$range))))){
      species$background.points <- as.data.frame(rasterToPoints(species$range)[,1:2])
      inds <- sample(1:nrow(species$background.points), size = nback, replace = TRUE)
      species$background.points <- species$background.points[inds,]
    } else {
      species$background.points <- as.data.frame(randomPoints(species$range, nback, species$presence.points))
    }

    colnames(species$background.points) <- colnames(species$presence.points)
    return(species)
  }

  if(bg.source == "env"){
    if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
      stop("bg.source set to env, but env layers were not recognized!")
    }

    if(nback > sum(as.numeric(!is.na(values(env[[1]]))))){
      species$background.points <- as.data.frame(rasterToPoints(env[[1]])[,1:2])
      inds <- sample(1:nrow(species$background.points), size = nback, replace = TRUE)
      species$background.points <- species$background.points[inds,]
    } else {
      species$background.points <- as.data.frame(randomPoints(env[[1]], nback, species$presence.points))
    }

    colnames(species$background.points) <- colnames(species$presence.points)
    return(species)
  }

}
