#' reps.rangebreak.blob, generates Monte Carlo replicates for the blob rangebreak
#' test of Glor and Warren 2011.
#'
#' This function applies blob breaks to the pooled distribution of two species.  Used
#' in order to construct a null distribution of expected overlaps between allopatric species
#' for which a linear boundary doesn't make sense.
#'
#' @param infiles One or more .csv files of species occurrence data
#' @param outfile Path to an output file.  If not provided, replicates will be returned
#'        in a data frame.
#' @param reps Number of replicates
#' @param verbose Controls printing of diagnostic messages
#'
#' @return output A data frame containing replicates for the Monte Carlo test
#'
#' @keywords keywords
#'
#' @export reps.rangebreak.blob
#'
#' @examples
#' reps.rangebreak.blob("~/species_A_and_B.csv", reps=100)


reps.rangebreak.blob <- function(infiles = x, outfile = FALSE, reps = 10, verbose=FALSE){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps
  proceed <- TRUE
  for(i in 1:length(infiles)){ # Checking to see if files are really there
    if(!file.exists(infiles[i])){
      print(paste(infiles[i], "not found!"))
      proceed <- FALSE
    }
  }
  if(proceed){
    if(verbose){print(paste("Starting blob rangebreak reps on", paste(infiles, collapse = " and "), "at", Sys.time()))}
    thisdata <- do.call("rbind", lapply(infiles, read.csv, header = TRUE, stringsAsFactors = FALSE))  #Throwing all input data files into a single matrix
    output <- matrix(ncol=length(thisdata[1,]), nrow=reps*length(thisdata[,1]))
    colnames(output) <- colnames(thisdata)
    speciesnames <- table(thisdata[,1])
    speciesnames <- speciesnames[order(speciesnames)] #Orders species by number of points
    if(verbose){print(speciesnames)}  #Prints number of points per species
    pointcounter <- 1
    ncolumns <- ncol(thisdata)
    #print(dim(output))
    for(i in 1:reps){
      startpoint <- thisdata[runif(1, min=1, max=length(thisdata[,1])),] #Choosing a random starting point
      if(verbose){print(startpoint)}
      thisrep <- thisdata
      for(j in 1:length(thisrep[,1])){
        thisrep[j,ncolumns + 1] <- (thisrep[j,3] - startpoint[3])**2 +  (thisrep[j,2] - startpoint[2])**2 #Adding Euclidean distance from start point
      }
      thisrep <- thisrep[order(thisrep[,ncolumns + 1]),] #Sort by Euclidean dstance
      species1 <- paste(rownames(speciesnames)[1], "_rep_", i, sep="")
      species2 <- paste(rownames(speciesnames)[2], "_rep_", i, sep="")
      for(k in 1:length(thisrep[,1])){
          if(k < speciesnames[1]){ #Point belongs to smaller of the two species
            output[pointcounter,1] <- species1
            for(m in 2:length(thisdata[1,])){
              output[pointcounter,m] <- thisrep[k,m]
              #print(paste(1, k, m, pointcounter))
            }
          }
          else{ #Point belongs to species 2
            output[pointcounter,1] <- species2
            for(m in 2:length(thisdata[1,])){
              output[pointcounter,m] <- thisrep[k,m]
              #print(paste(2, k, m, pointcounter))
            }
          }
          pointcounter <- pointcounter + 1
      }
    }
  }
  output <- as.data.frame(output, stringsAsFactors = FALSE)
  for(i in 1:length(output[1,])){
    if(!is.na(suppressWarnings(as.numeric(output[1,i])))){output[,i] <- as.numeric(output[,i])}
  }
  if(outfile != FALSE){write.csv(output, file=outfile, quote=FALSE, row.names=FALSE)}
  return(output)
}
