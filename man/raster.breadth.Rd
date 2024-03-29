% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raster.breadth.R
\name{raster.breadth}
\alias{raster.breadth}
\title{raster.breadth, applies measures of niche breadth to an ENM}
\usage{
raster.breadth(x, verbose = FALSE)
}
\arguments{
\item{x}{An object of class raster or RasterLayer, or an ENMTools model object containing a suitability raster.}

\item{verbose}{Controls printing of diagnostic messages}
}
\value{
results A vector containing the two metrics B1 and B2 for niche breadth.
}
\description{
This function measures the spatial heterogeneity of the distribution of suitability
scores from an ENM.  It returns Levins' (1968) two metrics of niche breadth.
}
\details{
Levins, R. 1968. Evolution in changing environments: some theoretical explorations. Princeton University Press.
}
\examples{
aurelioi.glm <- enmtools.glm(iberolacerta.clade$species$aurelioi, euro.worldclim,
f = pres ~ bio1 + bio12)
raster.breadth(aurelioi.glm)
}
\keyword{keywords}
