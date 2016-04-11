#' reps.background.asc, creates Monte Carlo replicates for ENM background test using .asc
#' input files
#'
#' This function generates replicates for the background test, as described in
#' Warren et al. 2008
#'
#'
#' @param x A raster file to use as a mask for drawing points
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
#' @export reps.background.raster
#'
#' @examples
#' reps.background.raster(env, outfile = "~/myreps.csv", npoints = 20, reps=100)

reps.background.raster <- function(x, outfile = FALSE, npoints, reps = 10, species= "species", verbose=FALSE, ...){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps
  proceed <- TRUE

  if(proceed){
    if(verbose){print(paste("Starting background reps at", Sys.time()))}
    if(verbose){print(paste("Generating a total of", reps*npoints, "points in", reps, "replicates"))}  #Prints number of points per species
    output <- as.data.frame(cbind(paste(species, "rep", rep(seq(1, reps), each=npoints), sep="_"),
                    randomPoints(x, npoints*reps, ...)))
    colnames(output) <- c("species", "lon", "lat")

    if(outfile != FALSE){write.csv(output, file=outfile, quote=FALSE, row.names=FALSE)}
    return(output)
  }
}
