#' Simulate a point process from an enmtools.model object
#'
#' Function that take an \code{enmtools.model} object and simulates points
#' from it using a point process.
#'
#' @param x entools.model object to simulate from
#' @param n.points approximate number of points to simulate. You may get small number fewer or greater.
#' If you need an exact number of points, generate too many, then drop the extra ones.
#' @method simulate enmtools.model
#' @export
simulate.enmtools.model <- function(x, n.points = 1000) {
  ## check if the model outputs predictions as probabilities (otherwise assume densities)
  suit <- x$suitability
  # if(all(na.omit(values(suit)) < 1 | na.omit(values(suit)) > 0)) {
  #   probs <- TRUE
  # } else {
  #   probs <- FALSE
  # }

  total_dens <- sum(na.omit(values(suit)))
  suit <- suit * (n.points / total_dens) * (1 / prod(res(suit)))

  suit.im <- raster.as.im(suit)
  pnts <- rpoispp(suit.im)
  res <- data.frame(x = pnts$x, y = pnts$y)
  res
}

#' Function to convert raster to pixel image
#' Credit goes to Jeffrey Evans via stack exchange:
#' https://gis.stackexchange.com/questions/115159/converting-raster-to-im-object-for-point-process-model-covariate-in-r
raster.as.im <- function(im) {
  r <- raster::res(im)
  orig <- sp::bbox(im)[, 1] + 0.5 * r
  dm <- dim(im)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(r[1], dm[1] - 1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(r[2], dm[2] - 1))))
  return(spatstat::im(matrix(raster::values(im), ncol = dm[1],
                             nrow = dm[2], byrow = TRUE)[dm[2]:1, ],
                      xcol = xx, yrow = yy))
}
