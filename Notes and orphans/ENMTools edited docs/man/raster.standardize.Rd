\name{raster.standardize}
\alias{raster.standardize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
raster.standardize
}
\description{
Divides every grid cell in a raster that is not na.value by the sum of all values, resulting in a raster that sums to 1.
}
\usage{
raster.standardize(infile = x, outfile = y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infile}{
%%     ~~Describe \code{infile} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
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
function (infile = x, outfile = y) 
{
    if (verbose) {
        print(paste("Starting standardize on", infile, "at", 
            Sys.time(), "writing results to", outfile))
    }
    output1 <- file(outfile, "wt")
    sum1 <- raster.sum(infile)
    con1 <- file(infile, "rt")
    while (length(theselines <- readLines(con1, n = maxlines, 
        warn = FALSE)) > 0) {
        for (j in 1:length(theselines)) {
            thisline <- unlist(strsplit(theselines[j], "\\s+", 
                perl = TRUE))
            if (length(grep("^[-0123456789]", thisline[1], perl = TRUE)) > 
                0) {
                thisline <- as.numeric(thisline)
                thisline[thisline == na.value] <- NA
                thisline <- thisline/sum1
                thisline[is.na(thisline)] <- na.value
                writeLines(as.character(paste(thisline, collapse = " ")), 
                  con = output1)
            }
            else {
                writeLines(as.character(paste(thisline, collapse = " ")), 
                  con = output1)
            }
        }
    }
    close(con1)
    close(output1)
    return(TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
