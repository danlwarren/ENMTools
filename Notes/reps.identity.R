#####
# Part of the ENMTools R package
# Given a .csv file, creates replicates for conducting the indentity test of Warren et al. 2008.
# Expects an input file name, optional output file name, and number of replicates.
# Assumes the first line of the input file is a header and the first column is a species name.
# Returns a data frame containing points, and writes a file to disk when specified.
#####

reps.identity <- function(infiles = x, outfile = FALSE, reps = 10){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps
  proceed <- TRUE
  for(i in 1:length(infiles)){ # Checking to see if files are really there
    if(!file.exists(infiles[i])){
      print(paste(infiles[i], "not found!"))
      proceed <- FALSE
    }
  }
  if(proceed){
    if(verbose){print(paste("Starting identity reps on", paste(infiles, collapse = " and "), "at", Sys.time()))}
    thisdata <- do.call("rbind", lapply(infiles, read.csv, header = TRUE, stringsAsFactors = FALSE))  #Throwing all input data files into a single matrix
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
  }
  output <- as.data.frame(output, stringsAsFactors = FALSE)
  for(i in 1:length(output[1,])){
    if(!is.na(suppressWarnings(as.numeric(output[1,i])))){output[,i] <- as.numeric(output[,i])}
  }
  if(outfile != FALSE){write.csv(output, file=outfile, quote=FALSE, row.names=FALSE)}
  return(output)
}