raster.breadth <-
function(x){
  proceed <- TRUE
  if(!file.exists(x)){
    print(paste(x, "not found!"))
    proceed <- FALSE
  }
  if(proceed){  #All pre-analysis checks have been completed
    if(verbose){print(paste("Starting breadth on", x, "at", Sys.time()))}
    sum1 <- raster.sum(x) # Sum of all values in file 1, used for standardization
    con1 <- file(x, "rt")
    B1 <- 0
    B2 <- 0
    cellcount <- 0
    while ((length(theselines <- readLines(con1, n = maxlines, warn = FALSE)) > 0)) {
      for(j in 1:length(theselines)){
        thisline1 <- unlist(strsplit(theselines[j], "\\s+", perl=TRUE))
        if((length(grep("^[-0123456789]", thisline1[1], perl=TRUE)) > 0)){  # Line contains numerical data
          thisline1 <- as.numeric(thisline1)
          thisline1[thisline1 == na.value] <- NA  # Convert na.value to NA
          cellcount <- cellcount + length(thisline1) - sum(is.na(thisline1))  # Counting numbers of non-NA cells
          B1 <- B1 + sum((thisline1/sum1)*(thisline1/sum1), na.rm = TRUE)
          B2 <- B2 +sum((thisline1/sum1)*log(thisline1/sum1), na.rm = TRUE)
        }
        else{
          #print(paste("Found header line:  ", thisline[1], thisline[2]))
        }
      }
    }
  }
  B1 <- ((1/B1) - 1)/(cellcount - 1)
  B2 <- 0 - B2/log(cellcount)
  results <- c(B1, B2)
  names(results) <- c("B1", "B2")
  if(verbose){print(paste("B1 and B2 are", B1, B2))}
  close(con1)
  return(results)
}
