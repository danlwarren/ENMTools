#' reps.rangebreak.ribbon, generates Monte Carlo replicates for the ribbon rangebreak
#' test of Glor and Warren 2011.
#' 
#' This function applies paired linear breaks to the pooled distribution of two species.  Used 
#' in order to construct a null distribution of expected overlaps between species separated
#' by a ribbon boundary, or between occurrences inside and outside the ribbon.  
#'
#' @param infiles One or more .csv files of species occurrence data
#' @param outfile Path to an output file.  If not provided, replicates will be returned 
#'        in a data frame.
#' @param separate I honestly don't remember what this argument does
#' @param reps Number of replicates
#' @param width Width of ribbon, in the same units as your input data (i.e., decimal degrees)
#' @param plotty Controls plotting of replicates as they are constructed.
#' @param background.raster Raster to use as backrgound for plotting replicates
#' @param verbose Controls printing of diagnostic messages
#'
#' @return output A data frame containing replicates for the Monte Carlo test
#' 
#' @keywords keywords
#'
#' @export reps.rangebreak.ribbon
#' @examples
#' reps.rangebreak.ribbon("~/species_A_and_B.csv", reps=100)

reps.rangebreak.ribbon <- function(infiles = x, outfile = FALSE, separate = TRUE, reps = 10, width = 1, plotty=TRUE, background.raster=NA, verbose=FALSE){
  # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps
  proceed <- TRUE
  for(i in 1:length(infiles)){ # Checking to see if files are really there
    if(!file.exists(infiles[i])){
      print(paste(infiles[i], "not found!"))
      proceed <- FALSE
    }
  }
  if(proceed){
    if(verbose){print(paste("Starting ribbon rangebreak reps on", paste(infiles, collapse = " and "), "at", Sys.time()))}
    thisdata <- do.call("rbind", lapply(infiles, read.csv, header = TRUE, stringsAsFactors = FALSE))  #Throwing all input data files into a single matrix
    output <- matrix(ncol=length(thisdata[1,]), nrow=reps*length(thisdata[,1]))
    colnames(output) <- colnames(thisdata)
    if(verbose){print(table(thisdata[,1]))}  #Prints number of points per species
    if(plotty == TRUE){
      if(is.na(background.raster)){
          plot(c(min(thisdata[,2]),max(thisdata[,2])), c(min(thisdata[,3]),max(thisdata[,3])), type = "n", xlab="X", ylab="Y", asp = 1)
      }
      else{
          plot(raster(background.raster))
      }
      points(thisdata[,2], thisdata[,3])
    }
    speciesnames <- table(thisdata[,1])
    pointcounter <- 1
    ncolumns <- ncol(thisdata)
    #print(dim(output))
    for(i in 1:reps){
      angle <- runif(1, min=0, max=pi)
      slope = sin(angle)/cos(angle)
      intercept_modifier <- (width/2)/cos(angle)
      if(slope < 0){intercept_modifier <- 0 - intercept_modifier}
      #print(slope)
      thisrep <- thisdata
      for(j in 1:length(thisrep[,1])){
        thisrep[j,ncolumns + 1] <- thisrep[j,3] - (slope * thisrep[j,2])
      }
      thisrep <- thisrep[order(thisrep[,ncolumns + 1]),]
      intercept <- 0 # This will be used to hold the intercept that splits the species, will later be used to with intercept_modifier to get cutoffs
      species1 <- paste(rownames(speciesnames)[1], "_rep_", i, sep="")
      species2 <- paste(rownames(speciesnames)[2], "_rep_", i, sep="")
      ribbon <- paste("ribbon_rep_", i, sep="")
      merged <- paste("merged_rep_", i, sep="")
      switchpoint <- 1 # This will tell the plot function when to plot the line, will be switched off when line is plotted for each rep
      if(runif(1, min=0, max=2) > 1){  #Start first species from top
        intercept <- mean(thisrep[speciesnames[1],ncolumns + 1], thisrep[speciesnames[1] + 1,ncolumns + 1])
        abline(a=intercept + intercept_modifier, b=slope)
        abline(a=intercept - intercept_modifier, b=slope)
        for(k in 1:length(thisrep[,1])){
          if(thisrep[k, ncolumns+1] < intercept - intercept_modifier){ #Point belongs to species 1
            if(separate == TRUE){output[pointcounter,1] <- species1}
            else{output[pointcounter,1] <- merged}
            for(m in 2:length(thisdata[1,])){
              output[pointcounter,m] <- thisrep[k,m]
              #print(paste(1, k, m, pointcounter))
            }
          }
          else if(thisrep[k, ncolumns+1] > intercept - intercept_modifier && thisrep[k, ncolumns+1] < intercept + intercept_modifier){ #Point belongs to ribbon
            output[pointcounter,1] <- ribbon
            for(m in 2:length(thisdata[1,])){
              output[pointcounter,m] <- thisrep[k,m]
              #print(paste(1, k, m, pointcounter))
            }
          }
          else{ #Point belongs to species 2
            if(separate == TRUE){output[pointcounter,1] <- species2}
            else{output[pointcounter,1] <- merged}
            for(m in 2:length(thisdata[1,])){
              output[pointcounter,m] <- thisrep[k,m]
              #print(paste(2, k, m, pointcounter))
            }
          }
          pointcounter <- pointcounter + 1
        }
      }
      else{ #Start second species from top
        intercept <- mean(thisrep[speciesnames[2],ncolumns + 1], thisrep[speciesnames[2] + 1,ncolumns + 1])
        abline(a=intercept + intercept_modifier, b=slope)
        abline(a=intercept - intercept_modifier, b=slope)
        for(k in 1:length(thisrep[,1])){
          if(thisrep[k, ncolumns+1] < intercept - intercept_modifier){ #Point belongs to species 1
            if(separate == TRUE){output[pointcounter,1] <- species2}
            else{output[pointcounter,1] <- merged}
            for(m in 2:length(thisdata[1,])){
              output[pointcounter,m] <- thisrep[k,m]
              #print(paste(1, k, m, pointcounter))
            }
          }
          else if(thisrep[k, ncolumns+1] > intercept - intercept_modifier && thisrep[k, ncolumns+1] < intercept + intercept_modifier){ #Point belongs to ribbon
            output[pointcounter,1] <- ribbon
            for(m in 2:length(thisdata[1,])){
              output[pointcounter,m] <- thisrep[k,m]
              #print(paste(1, k, m, pointcounter))
            }
          }
          else{ #Point belongs to species 2
            if(separate == TRUE){output[pointcounter,1] <- species1}
            else{output[pointcounter,1] <- merged}
            for(m in 2:length(thisdata[1,])){
              output[pointcounter,m] <- thisrep[k,m]
              #print(paste(2, k, m, pointcounter))
            }
          }
          pointcounter <- pointcounter + 1
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