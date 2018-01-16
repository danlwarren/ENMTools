#' Takes a set of points, a buffer radius, a sample size, and a mask and returns
#' randomly sampled points from within that buffer radius.

#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' @importFrom raster extract
#'
#' @param species An enmtools.species or enmtools.clade object
#' @param env A raster stack of environmental variables
#' @param verbose Controls printing of progress messages
#'
#' @export add.env


add.env <- function(species, env, verbose = TRUE){

  if(!any(c("RasterLayer", "RasterStack", "raster", "RasterBrick") %in% class(env))){
    stop("Argument env must be a raster, RasterLayer, or RasterStack object!")
  }

  if("enmtools.species" %in% class(species)){

    # Adding env data to a single species
    if(verbose == TRUE){
      cat(paste("Adding environmental data to species", species$species.name, "\n"))
    }

    if(class(species$presence.points) %in% c("data.frame", "matrix")){
      if(verbose == TRUE){
        cat("\tProcessing presence points...\n")
      }

      # Have to assign names manually because otherwise it fails when there's only one env layer
      names <- c(colnames(species$presence.points), names(env))
      species$presence.points <- cbind(species$presence.points, extract(env, species$presence.points[,1:2]))
      colnames(species$presence.points) <- names
      species$presence.points <- species$presence.points[complete.cases(species$presence.points),]
    } else {
      print("No presence points, skipping...\n")
    }

    if(class(species$background.points) %in% c("data.frame", "matrix")){
      if(verbose == TRUE){
        cat("\tProcessing background points...\n")
      }

      names <- c(colnames(species$background.points), names(env))
      species$background.points <- cbind(species$background.points, extract(env, species$background.points[,1:2]))
      colnames(species$background.points) <- names
      species$background.points <- species$background.points[complete.cases(species$background.points),]
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
