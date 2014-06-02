\name{raster.sum}
\alias{raster.sum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
raster.sum
}
\description{
Sums all values in a raster that are not na.value and returns the result.
}
\usage{
raster.sum(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    if (verbose) {
        print(paste("Starting sums on", x, "at", Sys.time()))
    }
    sum1 <- 0
    con <- file(x, "rt")
    while (length(theselines <- readLines(con, n = maxlines, 
        warn = FALSE)) > 0) {
        for (j in 1:length(theselines)) {
            thisline <- unlist(strsplit(theselines[j], "\\s+", 
                perl = TRUE))
            if (length(grep("^[-0123456789]", thisline[1], perl = TRUE)) > 
                0) {
                thisline <- as.numeric(thisline)
                thisline[thisline == na.value] <- NA
                sum1 <- sum1 + sum(thisline, na.rm = TRUE)
            }
            else {
            }
        }
    }
    close(con)
    return(sum1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
