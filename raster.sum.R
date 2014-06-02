#####
#  Sum raster file, mostly used when standardizing files so that they can be treated as probability distributions.
#  Expects to be fed a filename, returns a sum.
#####
raster.sum <- function(x){
  if(verbose){print(paste("Starting sums on", x, "at", Sys.time()))}
  sum1 <- 0
  con <- file(x, "rt")
  while (length(theselines <- readLines(con, n = maxlines, warn = FALSE)) > 0) {
    for(j in 1:length(theselines)){
      thisline <- unlist(strsplit(theselines[j], "\\s+", perl=TRUE))
      if(length(grep("^[-0123456789]", thisline[1], perl=TRUE)) > 0){  # Line contains numerical data
        thisline <- as.numeric(thisline)
        thisline[thisline == na.value] <- NA  # Convert na.value to NA
        sum1 <- sum1 + sum(thisline, na.rm = TRUE)
        #print(sum1) 
      }
      else{
        #print(paste("Found header line:  ", thisline[1], thisline[2]))
      }
    }
  }
  close(con)
  return (sum1)
}
