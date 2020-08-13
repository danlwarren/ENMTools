#' install.extras
#'
#' Convenience function that installs all extra packages used in ENMTools.  ENMTools uses
#' functions from a lot of external packages, and due to CRAN best practices it doesn't install
#' those packages by default.  The function install.extras() just calls install.packages with a
#' list of all of the extra packages.  At present this list includes mgcv, ecospat, randomForest,
#' hypervolume, ape, ppmlasso, leaflet, ranger, CalibratR, caret, and ResourceSelection.
#'
#' @param repos URL for the repository to use for installing R packages
#' @param ... Other parameters to be passed to \code{install.packages}
#'
#' @return No return value.
#'
#' @examples
#' \donttest{
#' install.extras()
#' }

install.extras <- function(repos='http://cran.us.r-project.org', ...) {
  install.packages(c("mgcv",
                     "ecospat",
                     "randomForest",
                     "hypervolume",
                     "ape",
                     "ppmlasso",
                     "leaflet",
                     "ranger",
                     "CalibratR",
                     "caret",
                     "ResourceSelection",
                     "fields",
                     "rJava",
                     "vip",
                     "pdp",
                     "fastshap",
                     "reshape2",
                     "viridis",
                     "progress"), repos=repos, ...)
}

# This function checks if a package is able to be loaded and triggers an error if not
check.packages <- function(package.names) {

  missing.packages <- c()

  for(i in package.names){
    if (!requireNamespace(i, quietly = TRUE)) {
      missing.packages <- c(missing.packages, as.character(i))
    }
  }

  if(length(missing.packages > 0)){
    stop("The following packages are needed for this function to work but are not installed. \n\n
You can install extra packages used in ENMTools manually or install all of them at once by running install.extras().\n\nPackage list: ",
         paste(missing.packages,  collapse = ", "),
         call. = FALSE)

  }

}



