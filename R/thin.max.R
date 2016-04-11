#' reps.rangebreak.ribbon, rarefaction of point data in any number of dimensions.
#'
# Function to rarefy point data in any number of dimensions.  The goal here is to
# take a large data set and reduce it in size in such a way as to approximately maximize the
# difference between points.  For instance, if you have 2000 points but suspect a lot of
# spatial autocorrelation between them, you can pass in your data frame, the names (or indices)
# of the lat/lon columns, and the number 200, and you get back 200 points from your original data
# set that are chosen to be as different from each other as possible given a randomly chosen
# starting point
#'
#' @param x A data frame containing the columns to be used to calculate distances along with whatever other data you need
#' @param cols A vector of column names or indices to use for calculating distances
#' @param npoints The number of rarefied points to spit out
#'
#' @return output A data frame containing the rarefied point data
#'
#' @keywords keywords
#'
#' @export thin.max
#'
#' @examples
#' thin.max(my.data, c("latitude", "longitude"), 200)

thin.max <- function(x, cols, npoints){
  #Create empty vector for output
  inds <- vector(mode="numeric")

  #Create distance matrix
  this.dist <- as.matrix(dist(x[,cols], upper=TRUE))

  #Draw first index at random
  inds <- c(inds, as.integer(runif(1, 1, length(this.dist[,1]))))

  #Get second index from maximally distant point from first one
  #Necessary because apply needs at least two columns or it'll barf
  #in the next bit
  inds <- c(inds, which.max(this.dist[,inds]))

  while(length(inds) < npoints){
    #For each point, find its distance to the closest point that's already been selected
    min.dists <- apply(this.dist[,inds], 1, min)

    #Select the point that is furthest from everything we've already selected
    this.ind <- which.max(min.dists)

    #Get rid of ties, if they exist
    if(length(this.ind) > 1){
      print("Breaking tie...")
      this.ind <- this.ind[1]
    }
    inds <- c(inds, this.ind)
  }

  return(x[inds,])
}
