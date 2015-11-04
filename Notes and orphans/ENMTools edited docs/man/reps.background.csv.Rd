\name{reps.background.csv}
\alias{reps.background.csv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
reps.background.csv
}
\description{
This function generates reps for the background test from Warren et al. 2008, using a .csv file of lat/lon points as input.
}
\usage{
reps.background.csv(infile = x, outfile = FALSE, points, reps = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infile}{
%%     ~~Describe \code{infile} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{points}{
%%     ~~Describe \code{points} here~~
}
  \item{reps}{
%%     ~~Describe \code{reps} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (infile = x, outfile = FALSE, points, reps = 10) 
{
    proceed <- TRUE
    if (!file.exists(infile)) {
        print(paste(infile, "not found!"))
        proceed <- FALSE
    }
    csvfile <- read.csv(infile, header = TRUE)
    if (points > length(csvfile[, 1])) {
        print(paste("Number of points per rep (", points, ") exceeds number of points provided (", 
            length(csvfile[, 1]), ")!"), sep = "")
        proceed <- FALSE
    }
    if (proceed) {
        if (verbose) {
            print(paste("Starting background reps on", infile, 
                "at", Sys.time()))
        }
        prefix <- gsub(".csv$", "", x = infile, perl = TRUE)
        output <- matrix(ncol = 3, nrow = reps * points)
        colnames(output) <- colnames(csvfile)
        if (verbose) {
            print(paste("Generating a total of", reps * points, 
                "points in", reps, "replicates"))
        }
        pointcounter <- 1
        for (i in 1:reps) {
            thisname <- paste(prefix, "_background_rep_", i, 
                sep = "")
            thismat <- sample(length(csvfile[, 1]), size = points, 
                replace = FALSE)
            for (j in 1:length(thismat)) {
                location <- ((i - 1) * points) + j
                thisx <- csvfile[thismat[j], 2]
                thisy <- csvfile[thismat[j], 3]
                output[location, 1] <- thisname
                output[location, 2] <- thisx
                output[location, 3] <- thisy
            }
        }
        output <- as.data.frame(output, stringsAsFactors = FALSE)
        for (i in 1:length(output[1, ])) {
            if (!is.na(suppressWarnings(as.numeric(output[1, 
                i])))) {
                output[, i] <- as.numeric(output[, i])
            }
        }
        if (outfile != FALSE) {
            write.csv(output, file = outfile, quote = FALSE, 
                row.names = FALSE)
        }
        return(output)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
