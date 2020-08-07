#' Takes two emtools.species objects with range rasters, calculates overlap between them as in Cardillo and Warren 2016
#'
#' @param x An enmtools.species object containing presence points
#' @param y An enmtools.species object containing presence points
#'
#' @return A numeric value measuring the overlap between point distributions.
#'
#' @examples
#' \donttest{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni <- iberolacerta.clade$species$cyreni
#' monticola <- iberolacerta.clade$species$monticola
#' if(requireNamespace("fields", quietly = TRUE)) {
#'     point.overlap(cyreni, monticola)
#' }
#' }

point.overlap <- function(x, y){

  check.packages("fields")

  if(!inherits(x$presence.points, c("data.frame"))){
    stop(paste("Species", x$species.name, "does not have presence points!"))
  }

  if(!inherits(y$presence.points, c("data.frame"))){
    stop(paste("Species", y$species.name, "does not have presence points!"))
  }

  # Get distance matrices for within and between species
  within1 <- fields::rdist(x$presence.points, x$presence.points)
  within2 <- fields::rdist(y$presence.points, y$presence.points)
  between <- fields::rdist(x$presence.points, y$presence.points)

  # Init some empty score arrays for each species
  score1 <- rep(NA,nrow(within1))
  score2 <- rep(NA,nrow(within2))

  for(k in 1:nrow(within1)){
    thisscore <- min(within1[k,-(k)])/min(between[k,])
    score1[k] <- thisscore
  }

  for(k in 1:nrow(within2)){
    thisscore <- min(within2[k,-(k)])/min(between[,k])
    score2[k] <- thisscore
  }

  score1 <- length(which(score1>1))/length(score1)
  score2 <- length(which(score2>1))/length(score2)
  return(mean(c(score1, score2)))
}
