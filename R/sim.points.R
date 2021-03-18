#' Simulate a point process from an enmtools.model object
#'
#' Function that takes an \code{enmtools.model} object and simulates points
#' from it using a point process.
#'
#' @param object entools.model object to simulate from
#' @param n.points approximate number of points to simulate. You may get small number fewer or greater.  If you need an exact number of points, generate too many, then drop the extra ones.
#' @param seed optional seed for the random number generator
#' @param sample.type method for sampling occurrence points.  Default is "ppp", which is a poisson point process.  Also accepts "binomial" for treating suitabilities as binomial trials, "thresh.pa" for sampling with equal probability every grid cell above a certain threshold value, and "thresh.con" for sampling in proportion to suitability above a certain threshold value.
#' @param replace whether to sample with replacement.
#' @param threshold a threshold suitability below which probability of sampling drops to zero.  Used for "thresh.pa" and "thresh.con" sampling.
#' @param ... additional parameters, not currently used but included for S3 compatability
#'
#' @return A data frame of simulated points from the enmtools.model object

sim.points <- function(object, n.points = 1000, seed = NA, sample.type = "ppp", replace = FALSE, threshold = NA, ...) {

  if(!is.na(seed)){
    set.seed(seed)
  }

  ## check if the model outputs predictions as probabilities (otherwise assume densities)
  suit <- object$suitability
  # if(all(na.omit(values(suit)) < 1 | na.omit(values(suit)) > 0)) {
  #   probs <- TRUE
  # } else {
  #   probs <- FALSE
  # }



  if(sample.type == "ppp"){
    # Standardize suitability scores
    total.dens <- sum(na.omit(values(suit)))

    suit <- suit * (n.points / total.dens) * (1 / prod(res(suit)))
    suit.im <- raster.as.im(suit)
    pnts <- rpoispp(suit.im)
    pres.points <- data.frame(x = pnts$x, y = pnts$y)
  }

  # Here we're just going to manipulate the suitability raster so that
  # everything above the threshold is 1 and everything below is zero
  if(sample.type == "thresh.pa"){
    if(is.na(threshold) | !is.numeric(threshold)){
      stop("Sample type is thresh.pa but thresdhold was not supplied or is not numeric!")
    }
    suit <- suit >= threshold
  }

  # Here we're going to manipulate the suitability raster so that
  # everything below the threshold is zero
  if(sample.type == "thresh.con"){
    if(is.na(threshold) | !is.numeric(threshold)){
      stop("Sample type is thresh.con but thresdhold was not supplied or is not numeric!")
    }
    suit <- suit * (suit > threshold)
  }

  if(sample.type %in% c("binomial", "thresh.pa", "thresh.con")){

    suit <- suit/max(getValues(suit), na.rm = TRUE)
    # Get lat, lon, and suitability
    sample.df <- rasterToPoints(suit)

    # Randomize order
    sample.df <- sample.df[sample(1:nrow(sample.df)),]

    # Build output df
    pres.points <- data.frame(x = numeric(0),
                              y = numeric(0))

    # Loop until you've got enough points
    npres <- 0
    while(npres < n.points){
      for(i in 1:nrow(sample.df)){

        # We need to do this check because we're taking rows out of the df when
        # replace == FALSE
        if(i >= nrow(sample.df)){
          next
        }

        # Get the suitability value
        this.suit <- sample.df[i,"layer"]

        # If you sample a presence point, stash it in pres.points
        if(rbinom(1, 1, prob = this.suit) == 1){
          this.row <- sample.df[i, 1:2]
          npres <- npres + 1
          pres.points[npres,] <- this.row
          if(replace == FALSE){
            sample.df <- sample.df[-i,]
          }
        }
        if(npres >= n.points){
          break
        }
      }
    }
  }

  return(pres.points)

}

# Function to convert raster to pixel image
# Credit goes to Jeffrey Evans via stack exchange:
# https://gis.stackexchange.com/questions/115159/converting-raster-to-im-object-for-point-process-model-covariate-in-r
raster.as.im <- function(im) {
  r <- raster::res(im)
  orig <- sp::bbox(im)[, 1] + 0.5 * r
  dm <- dim(im)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(r[1], dm[1] - 1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(r[2], dm[2] - 1))))
  return(spatstat.geom::im(matrix(raster::values(im), ncol = dm[1],
                             nrow = dm[2], byrow = TRUE)[dm[2]:1, ],
                      xcol = xx, yrow = yy))
}
