\name{reps.identity}
\alias{reps.identity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
reps.identity
}
\description{
This function generates replicates for executing the identity test of Warren et al. 2008.
}
\usage{
reps.identity(infiles = x, outfile = FALSE, reps = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infiles}{
%%     ~~Describe \code{infiles} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
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
function (infiles = x, outfile = FALSE, reps = 10) 
{
    proceed <- TRUE
    for (i in 1:length(infiles)) {
        if (!file.exists(infiles[i])) {
            print(paste(infiles[i], "not found!"))
            proceed <- FALSE
        }
    }
    if (proceed) {
        if (verbose) {
            print(paste("Starting identity reps on", paste(infiles, 
                collapse = " and "), "at", Sys.time()))
        }
        thisdata <- do.call("rbind", lapply(infiles, read.csv, 
            header = TRUE, stringsAsFactors = FALSE))
        output <- matrix(ncol = length(thisdata[1, ]), nrow = reps * 
            length(thisdata[, 1]))
        colnames(output) <- colnames(thisdata)
        if (verbose) {
            print(table(thisdata[, 1]))
        }
        pointcounter <- 1
        for (i in 1:reps) {
            thismat <- sample(length(thisdata[, 1]), length(thisdata[, 
                1]), replace = FALSE)
            for (j in 1:length(thismat)) {
                thisname <- paste(thisdata[j, 1], "_rep_", i, 
                  sep = "")
                output[pointcounter, 1] <- thisname
                for (k in 2:length(thisdata[1, ])) {
                  output[pointcounter, k] <- thisdata[thismat[j], 
                    k]
                }
                pointcounter <- pointcounter + 1
            }
        }
    }
    output <- as.data.frame(output, stringsAsFactors = FALSE)
    for (i in 1:length(output[1, ])) {
        if (!is.na(suppressWarnings(as.numeric(output[1, i])))) {
            output[, i] <- as.numeric(output[, i])
        }
    }
    if (outfile != FALSE) {
        write.csv(output, file = outfile, quote = FALSE, row.names = FALSE)
    }
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
