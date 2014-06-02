\name{raster.overlap}
\alias{raster.overlap}
raster.overlap
\title{
Measures overlap between rasters using the I and D metrics introduced in Warren et al. 2008.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
raster.overlap(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (x, y) 
{
    proceed <- TRUE
    if (!file.exists(x)) {
        print(paste(x, "not found!"))
        proceed <- FALSE
    }
    if (!file.exists(y)) {
        print(paste(y, "not found!"))
        proceed <- FALSE
    }
    if (proceed) {
        if (verbose) {
            print(paste("Starting overlap on", x, "and", y, "at", 
                Sys.time()))
        }
        sum1 <- raster.sum(x)
        sum2 <- raster.sum(y)
        if (verbose == TRUE) {
            print(paste("File sums are", sum1, sum2))
        }
        con1 <- file(x, "rt")
        con2 <- file(y, "rt")
        sum3 <- 0
        sum4 <- 0
        while ((length(theselines1 <- readLines(con1, n = maxlines, 
            warn = FALSE)) > 0) && (length(theselines2 <- readLines(con2, 
            n = maxlines, warn = FALSE)) > 0)) {
            for (j in 1:length(theselines1)) {
                thisline1 <- unlist(strsplit(theselines1[j], 
                  "\\s+", perl = TRUE))
                thisline2 <- unlist(strsplit(theselines2[j], 
                  "\\s+", perl = TRUE))
                if ((length(grep("^[-0123456789]", thisline1[1], 
                  perl = TRUE)) > 0) && (length(grep("^[-0123456789]", 
                  thisline2[1], perl = TRUE)) > 0)) {
                  thisline1 <- as.numeric(thisline1)
                  thisline2 <- as.numeric(thisline2)
                  thisline1[thisline1 == na.value] <- NA
                  thisline2[thisline2 == na.value] <- NA
                  sum3 <- sum3 + sum(abs(thisline1/sum1 - thisline2/sum2), 
                    na.rm = TRUE)
                  sum4 <- sum4 + sum((sqrt(thisline1/sum1) - 
                    sqrt(thisline2/sum2))^2, na.rm = TRUE)
                }
                else {
                }
            }
        }
    }
    sum3 <- 1 - sum3/2
    sum4 <- 1 - sum4/2
    results <- c(sum3, sum4)
    names(results) <- c("D", "I")
    if (verbose == TRUE) {
        print(paste("D and I are", sum3, sum4))
    }
    close(con1)
    close(con2)
    if (verbose) {
        print(Sys.time())
    }
    return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
