#' reps.background.csv, creates Monte Carlo replicates for ENM background test using .csv
#' input files
#'
#' This function generates replicates for the background test, as described in
#' Warren et al. 2008
#'
#'
#' @param x Path to a .csv file to be used for drawing random points
#' @param outfile An output .csv file to write replicates to.  If not provided, ENMTools
#'        will return a data frame instead.
#' @param reps The number of replicates to create
#' @param verbose Controls printing of diagnostic messages
#'
#' @return output A data frame containing Monte Carlo replicates to be used in constructing
#'         models for the background test.
#'
#' @keywords keywords
#'
#' @export reps.background.df
#'
#' @examples
#' reps.background.df(ahli[,2:3], outfile = "~/myreps.csv", npoints = 10, species = "allogus_east", reps=100)

reps.background.df <- function(x, outfile = FALSE, npoints, reps = 10, species="species", verbose=FALSE, ...){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps
  proceed <- TRUE
  if(npoints > length(x[,1])){
    print(paste("Number of points per rep (", npoints ,") exceeds number of points provided (", length(x[,1]) ,")!"), sep = "")
    proceed <- FALSE
  }
  if(proceed){
    if(verbose){print(paste("Starting background reps at", Sys.time()))}

    inds <- c()
    for(i in 1:reps){
      inds <- rbind(inds, sample(1:length(x[,1]), npoints))
    }
    print(length(inds))
    output <- as.data.frame(cbind(paste(species, "rep", rep(seq(1, reps), each=npoints), sep="_"), x[inds,]))
    colnames(output) <- c("species", "lon", "lat")
    rownames(output) <- c()

    if(outfile != FALSE){write.csv(output, file=outfile, quote=FALSE, row.names=FALSE)}
    return(output)
  }
}
