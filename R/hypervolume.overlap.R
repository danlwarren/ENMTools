#' Takes an emtools.species object and environmental layers, and constructs a hypervolume using the R package hypervolume
#'
#' @param species.1 An enmtools.species or enmtools.hypervolume object
#' @param species.2 An enmtools.species or enmtools.hypervolume object
#' @param env A stack of environmental rasters, required when enmtools.species objects are passed
#' @param reduction.factor Passed to hypervolume functions
#' @param ... Extra parameters to be passed to enmtools.hypervolume
#'
#' @return A list containing a set of hypervolume objects and the overlap that was measured between them.
#'
#' @examples
#' \dontrun{
#' install.extras(repos='http://cran.us.r-project.org')
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' mont <- iberolacerta.clade$species$monticola
#' cyr <- iberolacerta.clade$species$cyreni
#' env <- euro.worldclim[[c(1,8,12,17)]]
#' if(requireNamespace("hypervolume", quietly = TRUE)) {
#'     hypervolume.overlap(mont, cyr, env = env)
#' }
#' }

hypervolume.overlap <- function(species.1, species.2, env = NA, reduction.factor = 0.1, ...){

  check.packages("hypervolume")

  # Turn species into hypervolumes, if they're not already
  if(inherits(species.1, "enmtools.species")){
    if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
      stop("Environmental rasters must be supplied when enmtools.species objects are passed to hypervolume.overlap.")
    }
    species.1 <- enmtools.hypervolume(species.1, env, ...)
  }

  if(inherits(species.2, "enmtools.species")){
    if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
      stop("Environmental rasters must be supplied when enmtools.species objects are passed to hypervolume.overlap.")
    }
    species.2 <- enmtools.hypervolume(species.2, env, ...)
  }

  # Checking that we now have two enmtools.hypervolume objects
  if(!inherits(species.1, "enmtools.hypervolume")){
    stop("Species 1 is not an enmtools.species or enmtools.hypervolume object!")
  }

  if(!inherits(species.2, "enmtools.hypervolume")){
    stop("Species 2 is not an enmtools.species or enmtools.hypervolume object!")
  }

  hv.set <- hypervolume::hypervolume_set(species.1$hv, species.2$hv, check.memory = FALSE)

  volumes <- hypervolume::get_volume(hv.set)

  # Storing and resetting par on exit as required by CRAN
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))

  op=par(mar=c(3,10,1,1))
  barplot(volumes,horiz=TRUE,las=2,main="Hypervolume",cex.names=0.5,col='lightblue')

  par(op)
  plot(hv.set[[c(3,5,6)]])

  output <- list(overlap = hypervolume::hypervolume_overlap_statistics(hv.set),
                 hv.set = hv.set)

  return(output)
}
