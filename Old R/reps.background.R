#' reps.background, creates Monte Carlo replicates for ENM background test
#'
#' This function generates replicates for the background test, as described in
#' Warren et al. 2008
#'
#'
#' @param x A raster file, shape file, or data frame to use as a mask for drawing points
#' @param outfile An output .csv file to write replicates to.  If not provided, ENMTools
#'        will return a data frame instead.
#' @param reps The number of replicates to create
#' @param npoints The number of points in each replicate
#' @param verbose Controls printing of diagnostic messages
#' @param ... arguments to be passed to the randomPoints function of dismo
#'
#' @return output A data frame containing Monte Carlo replicates to be used in constructing
#'         models for the background test.
#'
#' @keywords keywords
#'
#' @export reps.background
#'
#' @examples
#' # Points from a data frame
#' reps.background(ahli, outfile = "myreps.csv", npoints = 10, reps=100)
#'
#' # Points from a raster
#' reps.background(ahli.raster, outfile = "myreps.csv", npoints = 10, reps=100)
#'
#' #Points from a polygon
#' ahli.shape <- circles(ahli[,2:3], d = 5000, lonlat=TRUE)
#' reps.background(ahli.shape, outfile = "myreps.csv", npoints = 10, reps=100)

reps.background <- function(x, outfile = FALSE, npoints, reps = 10, species= "species", verbose=FALSE, ...){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps

  if(verbose){print(paste("Starting background reps at", Sys.time()))}
  if(verbose){print(paste("Generating a total of", reps*npoints, "points in", reps, "replicates"))}  #Prints number of points per species

  # Background points from raster layer
  if(class(x) == "RasterLayer"){
    output <- as.data.frame(cbind(paste(species, "rep", rep(seq(1, reps), each=npoints), sep="_"),
                                  randomPoints(x, npoints*reps, ...)))
  }

  # Background points from data frame
  if(class(x) == "data.frame"){
    if(npoints > length(x[,1])){
      stop(paste("Number of points per rep (", npoints ,") exceeds number of points provided (", length(x[,1]) ,")!"), sep = "")
    }

    if(verbose){
      print(paste("Starting background reps at", Sys.time()))
    }

    inds <- c()
    for(i in 1:reps){
      inds <- c(inds, sample(1:length(x[,1]), npoints))
    }
    output <- as.data.frame(cbind(paste(species, "rep", rep(seq(1, reps), each=npoints), sep="_"), x[inds,]))
  }

  if(class(x) == "SpatialPolygons"){
    output <- as.data.frame(cbind(paste(species, "rep", rep(seq(1, reps), each=npoints), sep="_"),
                                  as.data.frame(spsample(x, npoints*reps, type = "random", ...))))
    rownames(output) <- seq(1:nrow(output))
  }

  colnames(output) <- c("species", "lon", "lat")

  if(outfile != FALSE){write.csv(output, file=outfile, quote=FALSE, row.names=FALSE)}
  return(output)
}
