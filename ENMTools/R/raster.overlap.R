raster.overlap <-
function(x, y, verbose=FALSE){
  proceed <- TRUE
  if(!file.exists(x)){
    print(paste(x, "not found!"))
    proceed <- FALSE
  }
  if(!file.exists(y)){
    print(paste(y, "not found!"))
    proceed <- FALSE
  }
  if(proceed){  #All pre-analysis checks have been completed
    if(verbose){print(paste("Starting overlap on", x, "and", y, "at", Sys.time()))}
    sum1 <- raster.sum(x) # Sum of all values in file 1, used for standardization
    sum2 <- raster.sum(y) # Sum of all values in file 2, used for standardization
    if(verbose == TRUE){print(paste("File sums are",sum1, sum2))}
    con1 <- file(x, "rt")
    con2 <- file(y, "rt")
    sum3 <- 0
    sum4 <- 0
    while ((length(theselines1 <- readLines(con1, n = maxlines, warn = FALSE)) > 0) && (length(theselines2 <- readLines(con2, n = maxlines, warn = FALSE)) > 0)) {
      for(j in 1:length(theselines1)){
        thisline1 <- unlist(strsplit(theselines1[j], "\\s+", perl=TRUE))
        thisline2 <- unlist(strsplit(theselines2[j], "\\s+", perl=TRUE))
        if((length(grep("^[-0123456789]", thisline1[1], perl=TRUE)) > 0) && (length(grep("^[-0123456789]", thisline2[1], perl=TRUE)) > 0)){  # Line contains numerical data
          thisline1 <- as.numeric(thisline1)
          thisline2 <- as.numeric(thisline2)
          thisline1[thisline1 == na.value] <- NA  # Convert na.value to NA
          thisline2[thisline2 == na.value] <- NA  # Convert na.value to NA
          sum3 <- sum3 + sum(abs(thisline1/sum1 - thisline2/sum2), na.rm = TRUE)
          sum4 <- sum4 + sum((sqrt(thisline1/sum1) - sqrt(thisline2/sum2))^2, na.rm = TRUE)
        }
        else{
          #print(paste("Found header line:  ", thisline[1], thisline[2]))
        }
      }
    }
  }
  sum3 <- 1 - sum3/2
  sum4 <- 1 - sum4/2
  results <- c(sum3, sum4)
  names(results) <- c("D", "I")
  if(verbose==TRUE){print(paste("D and I are",sum3, sum4))}
  close(con1)
  close(con2)
  if(verbose){print(Sys.time())}
  return(results)
}
