#' Takes a set of points, a buffer radius, a sample size, and a mask and returns
#' randomly sampled points from within that buffer radius.

#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' @param species A list of enmtools.species objects or an enmtools.clade object
#' @param nreps Number of replicates to generate
#' @param mask A raster to use as a mask
#'
#' @return A data frame of x and y coordinates for pseudoreplicates sampled according to the
#' identity test of Warren et al. 2008
#'
#' @keywords niche conservatism, niche identity
#'
#' @export identity.reps
#'
#' @examples
#'

identity.reps <- function(species, nreps){

}
