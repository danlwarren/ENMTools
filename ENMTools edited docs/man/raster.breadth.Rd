\name{raster.breadth}
\alias{raster.breadth}
raster.breadth
\title{
Measures breadth of ENM using Levins' 1968 metrics, as introduced in Mandel et al. 2010 and Nakazato et al. 2010.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
raster.breadth(x)
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
    proceed <- TRUE
    if (!file.exists(x)) {
        print(paste(x, "not found!"))
        proceed <- FALSE
    }
    if (proceed) {
        if (verbose) {
            print(paste("Starting breadth on", x, "at", Sys.time()))
        }
        sum1 <- raster.sum(x)
        con1 <- file(x, "rt")
        B1 <- 0
        B2 <- 0
        cellcount <- 0
        while ((length(theselines <- readLines(con1, n = maxlines, 
            warn = FALSE)) > 0)) {
            for (j in 1:length(theselines)) {
                thisline1 <- unlist(strsplit(theselines[j], "\\s+", 
                  perl = TRUE))
                if ((length(grep("^[-0123456789]", thisline1[1], 
                  perl = TRUE)) > 0)) {
                  thisline1 <- as.numeric(thisline1)
                  thisline1[thisline1 == na.value] <- NA
                  cellcount <- cellcount + length(thisline1) - 
                    sum(is.na(thisline1))
                  B1 <- B1 + sum((thisline1/sum1) * (thisline1/sum1), 
                    na.rm = TRUE)
                  B2 <- B2 + sum((thisline1/sum1) * log(thisline1/sum1), 
                    na.rm = TRUE)
                }
                else {
                }
            }
        }
    }
    B1 <- ((1/B1) - 1)/(cellcount - 1)
    B2 <- 0 - B2/log(cellcount)
    results <- c(B1, B2)
    names(results) <- c("B1", "B2")
    if (verbose) {
        print(paste("B1 and B2 are", B1, B2))
    }
    close(con1)
    return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
