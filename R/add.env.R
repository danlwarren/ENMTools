#' Takes a set of points, a buffer radius, a sample size, and a mask and returns
#' randomly sampled points from within that buffer radius.

#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' @param species An enmtools.species or enmtools.clade object
#' @param env A raster stack of environmental variables
#'
#' @export add.env


add.env <- function(species, env){

  if(!any(c("RasterLayer", "RasterStack", "raster") %in% class(env))){
    stop("Argument env must be a raster, RasterLayer, or RasterStack object!")
  }

  if("enmtools.species" %in% class(species)){

    # Adding env data to a single species
    print(paste("Adding environmental data to species", species$species.name))
    if(class(species$presence.points) %in% c("data.frame", "matrix")){
      cat("\tProcessing presence points...\n")
      species$presence.points <- cbind(species$presence.points, extract(env, species$presence.points[,1:2]))
    } else {
      print("No presence points, skipping...\n")
    }

    if(class(species$background.points) %in% c("data.frame", "matrix")){
      cat("\tProcessing background points...\n")
      species$background.points <- cbind(species$background.points, extract(env, species$background.points[,1:2]))
    } else {
      print("No background points, skipping...\n")
    }

  } else if("enmtools.clade" %in% class(species)){

    # Adding env data to an entire clade
    # All we need to do for this is call add.env for each species individually
    for(i in 1:length(species$species)){
      species$species[[i]] <- add.env(species$species[[i]], env)
    }

  } else {

    # Not a species or clade
    stop("Argument species is not an enmtools.species or enmtools.clade object!")

  }

  return(species)

}
