#' install.extras
#'
#' Convenience function that installs all extra packages used in ENMTools.  ENMTools uses
#' functions from a lot of external packages, and due to CRAN best practices it doesn't install
#' those packages by default.  The function install.extras() just calls install.packages with a
#' list of all of the extra packages.  At present this list includes mgcv, ecospat, randomForest,
#' hypervolume, ape, ppmlasso, leaflet, ranger, CalibratR, caret, and ResourceSelection.
#'
#' @examples
#' \dontrun{
#' install.extras()
#' }

install.extras <- function() {
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
                     "ResourceSelection"))
}

# This function checks if a package is able to be loaded and triggers an error if not
check.package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(paste("Package", package_name,  "needed for this function to work. Please install it.
               You can install all extra packages used in ENMTools by running install.extras()"),
         call. = FALSE)
  }
}



