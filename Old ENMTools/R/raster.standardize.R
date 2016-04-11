#' raster.standardize, standardizes all values in a raster file
#' 
#' This function is used by a lot of the metrics calculated by ENMTools, in order to
#' standardize suitability scores so they sum to 1 over a geographic space.
#' 
#'
#' @param x A raster file in the .asc format, provided as a path
#' @param y A path to output a standardized raster file
#' @param verbose Controls printing of diagnostic messages
#'
#' 
#' @keywords keywords
#'
#' @examples
#' raster.standardize(x)

raster.standardize <-
function(infile = x, outfile = y, verbose=FALSE){
  if(verbose){print(paste("Starting standardize on", infile, "at", Sys.time(), "writing results to", outfile))}
  output1 <- file(outfile, "wt")
  sum1 <- raster.sum(infile)
  con1 <- file(infile, "rt")
  while (length(theselines <- readLines(con1, n = maxlines, warn = FALSE)) > 0) {
    for(j in 1:length(theselines)){
      thisline <- unlist(strsplit(theselines[j], "\\s+", perl=TRUE))
      if(length(grep("^[-0123456789]", thisline[1], perl=TRUE)) > 0){  # Line contains numerical data
        thisline <- as.numeric(thisline)
        thisline[thisline == na.value] <- NA  # Convert na.value to NA
        thisline <- thisline/sum1
        thisline[is.na(thisline)] <- na.value
        #print(thisline) 
        writeLines(as.character(paste(thisline, collapse = " ")), con=output1)
      }
      else{
        #print (thisline)
        writeLines(as.character(paste(thisline, collapse = " ")), con=output1)
      }
    }
  }
  close(con1)
  close(output1)
}
