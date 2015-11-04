#####
# Part of the ENMTools R package
# Given an ASCII file, creates replicates for conducting the background test of Warren et al. 2008.
# Expects an input file name, optional output file name, number of points, and number of replicates.
# Assumes the first line of the input file is a header and the first column is a species name.
# Returns a data frame containing points, and writes a file to disk when specified.
#####

reps.background.asc <- function(infile = x, outfile = FALSE, points, reps = 10){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps
  proceed <- TRUE
  if(!file.exists(infile)){
    print(paste(infile, "not found!"))
    proceed <- FALSE
  }
  if(proceed){
    if(verbose){print(paste("Starting background reps on", infile, "at", Sys.time()))}
    output <- matrix(ncol=3, nrow=reps*points)
    colnames(output) <- c("Species", "Latitude", "Longitude")
    if(verbose){print(paste("Generating a total of", reps*points, "points in", reps, "replicates"))}  #Prints number of points per species
    pointcounter <- 1
    ascfile <- import.asc(infile)
    allpoints <- getXYcoords(ascfile)
    prefix <- gsub(".asc$", "", x=infile, perl=TRUE)
    for(i in 1:reps){
      thisname <- paste(prefix, "_background_rep_", i, sep = "")
      # thismat contains a list of scrambled numbers 1 to length(thisdata), they will be used as indices for sampling
      for(j in 1:points){
        location <- ((i-1) * points) + j
        goodpoint <- FALSE
        while(goodpoint == FALSE){
          thisx <- sample(length(ascfile[,1]), 1)
          thisy <- sample(length(ascfile[1,]), 1)
          if(!is.na(ascfile[thisx,thisy])){
            goodpoint <- TRUE
            #print (paste(location,thisx,thisy))
            output[location,1] <- thisname
            output[location,2] <- allpoints$y[thisy]
            output[location,3] <- allpoints$x[thisx]
          }
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
}