#' reps.identity, creates Monte Carlo replicates for ENM identity test
#'
#' This function generates replicates for the identity test, as described in
#' Warren et al. 2008
#'
#'
#' @param x One or more .csv files containing species occurrence data
#' @param outfile An output .csv file to write replicates to.  If not provided, ENMTools
#'        will return a data frame instead.
#' @param reps The number of replicates to create
#' @param verbose Controls printing of diagnostic messages
#'
#' @return output A data frame containing Monte Carlo replicates to be used in constructing
#'         models for the identity test.
#'
#' @keywords keywords
#'
#' @export reps.identity
#'
#' @examples
#' reps.identity(x = rbind(ahli, allogus_east), outfile = "myreps.csv", reps=100)

reps.identity <- function(x, outfile = FALSE, reps = 10, verbose=FALSE, name.column = 1){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps

  # Test if the arg is a csv or a path to a file
  if(class(x) == "character"){

    if(!file.exists(x)){
      stop(paste(x, "not found!"))
    }

    # File exists, read it in
    x <- read.csv(x)
  }

  if(verbose){print(paste("Starting identity reps on", paste(infiles, collapse = " and "), "at", Sys.time()))}

  if(verbose){print(table(x[,1]))}  #Prints number of points per species

  sample.vector <- c()
  for(i in 1:reps){
    sample.vector <- c(sample.vector, sample(nrow(x), nrow(x), replace=FALSE))
  }

  output <- cbind(paste(rep(x[,name.column], reps), paste("rep", rep(seq(1:reps), each = nrow(x)), sep = "_"),sep = "_"),
                  x[sample.vector,-name.column])

  colnames(output) <- colnames(x)

  if(outfile != FALSE){write.csv(output, file=outfile, quote=FALSE, row.names=FALSE)}
  return(output)
}
