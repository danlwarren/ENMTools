#####
# Part of the ENMTools R package
# Given an ASCII file, randomly samples points for use in simulating data.
# Expects an input file name, number of points per replicate, and number of replicates.
# Assumes the first line of the input file is a header.
# Returns a data frame containing points, and writes a file to disk when specified.
#####

raster.sample <- function(infile, outfile, points = 10, reps = 10, sample.type = "linear", ...){
    # Will write an output csv file if it receives an outfile name, otherwise just returns the matrix of reps
    proceed <- TRUE
    if(!file.exists(infile)){
        print(paste(infile, "not found!"))
        proceed <- FALSE
    }
    if(proceed){
        if(verbose){print(paste("Starting sample reps on", infile, "at", Sys.time()))}
        output <- matrix(ncol=3, nrow=0)
        colnames(output) <- c("Species", "Latitude", "Longitude")
        if(verbose){print(paste("Generating a total of", reps*points, "points in", reps, "replicates"))}  #Prints number of points per species
        
        ade.raster <- import.asc(infile)
        coords <- getXYcoords(ade.raster)
        prob.raster <- raster(infile)
        prob.vector <- getValues(prob.raster)
        if(sample.type == "linear"){
            prob.vector[is.na(prob.vector)] <- 0
        }
        if(sample.type == "exponential"){
            prob.vector <- prob.vector/sum(as.numeric(prob.vector), na.rm=TRUE)
            prob.vector <- exp(prob.vector)
            prob.vector[is.na(prob.vector)] <- 0
        }
        if(sample.type == "constant"){
            prob.vector <- as.numeric(!is.na(prob.vector))
        }
        lat.vector <- rep(rev(coords$y), each = length(coords$x))
        lon.vector <- rep(coords$x, length(coords$y))
        ind.vector <- seq(1:length(lon.vector))
        
        prefix <- gsub(".asc$", "", x=infile, perl=TRUE)
        for(i in 1:reps){
            if(verbose){print(paste("Starting rep", i, "on", infile, "at", Sys.time()))}
            thisname <- paste(prefix, "_background_rep_", i, sep = "")
            # thismat contains a list of scrambled numbers 1 to length(thisdata), they will be used as indices for sampling
            
            inds <- sample(ind.vector, size=points, prob=prob.vector, ...)
            thesepoints <- cbind(rep(thisname, times=points), lat.vector[inds], lon.vector[inds])
            output <- rbind(output, thesepoints)
            
        }
        output <- as.data.frame(output, stringsAsFactors = FALSE)
        for(i in 1:length(output[1,])){
            if(!is.na(suppressWarnings(as.numeric(output[1,i])))){output[,i] <- as.numeric(output[,i])}
        }
        if(outfile != FALSE){write.csv(output, file=outfile, quote=FALSE, row.names=FALSE)}
        
        return(output)
    }
}