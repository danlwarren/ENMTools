reps.background.csv <-
function(infile = x, outfile = FALSE, points, reps = 10, verbose=FALSE){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps
  proceed <- TRUE
  if(!file.exists(infile)){
    print(paste(infile, "not found!"))
    proceed <- FALSE
  }
  csvfile <- read.csv(infile, header=TRUE)
  if(points > length(csvfile[,1])){
    print(paste("Number of points per rep (", points ,") exceeds number of points provided (", length(csvfile[,1]) ,")!"), sep = "")  
    proceed <- FALSE
  }
  if(proceed){
    if(verbose){print(paste("Starting background reps on", infile, "at", Sys.time()))}  
    prefix <- gsub(".csv$", "", x=infile, perl=TRUE)
    output <- matrix(ncol=3, nrow=reps*points)
    colnames(output) <- colnames(csvfile)
    if(verbose){print(paste("Generating a total of", reps*points, "points in", reps, "replicates"))}  #Prints number of points per species
    pointcounter <- 1
    for(i in 1:reps){
      thisname <- paste(prefix, "_background_rep_", i, sep = "")
      thismat <- sample(length(csvfile[,1]), size=points, replace=FALSE)
      # thismat contains a list of scrambled numbers 1 to length(thisdata), they will be used as indices for sampling
      for(j in 1:length(thismat)){
        location <- ((i-1) * points) + j
        thisx <- csvfile[thismat[j],2]
        thisy <- csvfile[thismat[j],3]
        output[location,1] <- thisname
        output[location,2] <- thisx
        output[location,3] <- thisy
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
