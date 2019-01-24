#' calc.B1, Calculates standardized version of Levins (1968) B1 measure of niche breadth given a vector of suitabilities
#'
#' @param x A numeric vector
#'
#' @return B1 A calculation of Levins (1968) B1 metric
#'
#' @keywords niche breadth sdm enm
#'
#' @examples
#' calc.B1(c(1, .001, .001))

calc.B1 <- function(x){
  x <- x[!is.na(x)]
  x <- x/sum(x)

  ## replace values below machine precision
  x[x < .Machine$double.xmin] <- .Machine$double.xmin

  max.B1 <- length(x) * (1/length(x)) * log(1/length(x))
  return(sum(x * log(x))/max.B1)
}
