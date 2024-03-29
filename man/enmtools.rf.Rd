% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enmtools.rf.R
\name{enmtools.rf}
\alias{enmtools.rf}
\title{Takes an emtools.species object with presence and background points, and builds a random forest model}
\usage{
enmtools.rf(
  species,
  env,
  f = NULL,
  test.prop = 0,
  eval = TRUE,
  nback = 1000,
  env.nback = 10000,
  report = NULL,
  overwrite = FALSE,
  rts.reps = 0,
  bg.source = "default",
  verbose = FALSE,
  clamp = TRUE,
  corner = NA,
  bias = NA,
  ...
)
}
\arguments{
\item{species}{An enmtools.species object}

\item{env}{A SpatRaster of environmental data.}

\item{f}{A formula for fitting the model}

\item{test.prop}{Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.}

\item{eval}{Determines whether model evaluation should be done.  Turned on by default, but moses turns it off to speed things up.}

\item{nback}{Number of background points to draw from range or env, if background points aren't provided}

\item{env.nback}{Number of points to draw from environment space for environment space discrimination metrics.}

\item{report}{Optional name of an html file for generating reports}

\item{overwrite}{TRUE/FALSE whether to overwrite a report file if it already exists}

\item{rts.reps}{The number of replicates to do for a Raes and ter Steege-style test of significance}

\item{bg.source}{Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.}

\item{verbose}{Controls printing of various messages progress reports.  Defaults to FALSE.}

\item{clamp}{When set to TRUE, clamps the environmental layers so that predictions made outside the min/max of the training data for each predictor are set to the value for the min/max for that predictor. Prevents the model from extrapolating beyond the min/max bounds of the predictor space the model was trained in, although there could still be projections outside the multivariate training space if predictors are strongly correlated.}

\item{corner}{An integer from 1 to 4.  Selects which corner to use for "block" test data.  By default the corner is selected randomly.}

\item{bias}{An optional raster estimating relative sampling effort per grid cell.  Will be used for drawing background data.}

\item{...}{Arguments to be passed to rf()}
}
\value{
An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
}
\description{
Takes an emtools.species object with presence and background points, and builds a random forest model
}
\examples{
\donttest{
enmtools.rf(iberolacerta.clade$species$monticola, env = euro.worldclim, nback = 500)
}
}
