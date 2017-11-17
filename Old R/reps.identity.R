#' reps.identity, creates Monte Carlo replicates for ENM identity test
#'
#' This function generates replicates for the identity test, as described in
#' Warren et al. 2008
#'
#'
#' @param x One or more .csv files containing species occurrence data
#' @param outfile An output .csv file to write replicates to.  If not provided, ENMTools
#'        will just return a data frame.
#' @param reps The number of replicates to create
#' @param verbose Controls printing of diagnostic messages
#'
#' @return output A data frame containing Monte Carlo replicates to be used in constructing
#'         models for the identity test.
#'
#' @keywords keywords
#'
#' @export reps.identity.csv
#'
#' @examples
#' reps.identity.csv(infiles = "~/myfile.csv", outfile = "~/myreps.csv", reps=100)

reps.identity <- function(x, species.col = "species", outfile = FALSE, reps = 10, verbose=FALSE){

  proceed <- TRUE

  # If x is a series of file names
  if(is.character(x)){

    # x is a character vector, presumably of file names
    for(i in 1:length(x)){ # Checking to see if files are really there
      if(!file.exists(x[i])){
        print(paste(x[i], "not found!"))
        proceed <- FALSE
        break
      }
    }

    # Now stitch the infiles all together
    if(proceed == TRUE){
      thisdata <- do.call("rbind", lapply(infiles, read.csv, header = TRUE, stringsAsFactors = FALSE))  #Throwing all input data files into a single matrix
    }

  } else if (is.data.frame(x)){

    # x is a data frame so we can just go with it as it is
    thisdata <- x

  } else {

    # x isn't a character vector or data frame, so we don't know what to do
    proceed <- FALSE
    stop("Error, don't know how to handle input class ", class(x))

  }

  if(!(species.col %in% col.names(thisdata))){
    proceed <- FALSE
    stop("Error, couldn't find column ", species.col, ".  Try setting the column name when calling identity.reps")
  }


  if(proceed == TRUE){

    # At this point x should be a data frame regardless of what the input was
    if(verbose){print(paste("Starting identity reps at", Sys.time()))}

    output <- matrix(ncol=length(thisdata[1,]), nrow=reps*length(thisdata[,1]))
    colnames(output) <- colnames(thisdata)
    if(verbose){print(table(thisdata[,1]))}  #Prints number of points per species
    pointcounter <- 1
    for(i in 1:reps){
      thismat <- sample(length(thisdata[,1]), length(thisdata[,1]), replace=FALSE)
      # thismat contains a list of scrambled numbers 1 to length(thisdata), they will be used as indices for sampling
      for(j in 1:length(thismat)){
        thisname <- paste(thisdata[j,1], "_rep_", i, sep="")
        output[pointcounter,1] <- thisname
        for(k in 2:length(thisdata[1,])){
          output[pointcounter,k] <- thisdata[thismat[j],k]
        }
        pointcounter <- pointcounter + 1
      }
    }
    output <- as.data.frame(output, stringsAsFactors = FALSE)
    for(i in 1:length(output[1,])){
      if(!is.na(suppressWarnings(as.numeric(output[1,i])))){output[,i] <- as.numeric(output[,i])}
    }

    if(outfile != FALSE){write.csv(output, file=outfile, quote=FALSE, row.names=FALSE)}
    return(output)
  }
}
