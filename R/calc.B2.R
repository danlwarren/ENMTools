#' calc.B2, Calculates standardized version of Levins (1968) B2 measure of niche breadth given a vector of suitabilities
#'
#' @param x A numeric vector
#'
#' @return B2 A calculation of Levins (1968) B2 metric
#'
#' @keywords niche breadth sdm enm
#'
#' @export calc.B2
#'
#' @examples
#' calc.B2(1, .001, .001)

calc.B2 <- function(x){
  x <- x[!is.na(x)]
  x <- x/sum(x)
  min.B2 <- 1
  max.B2 <- 1/(length(x) * (1/length(x))**2)
  this.B2 <- 1/sum(x**2)
  return((this.B2 -min.B2)/(max.B2 - min.B2))
}
