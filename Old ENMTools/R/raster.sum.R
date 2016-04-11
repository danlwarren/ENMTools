#' raster.sum, sums all values in a raster file
#' 
#' This function is used by a lot of the metrics calculated by ENMTools, in order to
#' standardize suitability scores so they sum to 1 over a geographic space.
#' 

#'
#' @param x A raster file
#' @param verbose Controls printing of diagnostic messages
#'
#' @return sum1 The sum of all suitability scores in the raster
#' 
#' @keywords keywords
#'
#' @export raster.sum
#' @examples
#' raster.sum(x)

raster.sum <-
function(x, verbose=FALSE){
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
