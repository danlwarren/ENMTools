# This function checks if a package is able to be loaded and triggers an error if not
check.package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(paste("Package", package_name,  "needed for this function to work. Please install it.
               You can install all extra packages used in ENMTools by running install.extras()"),
         call. = FALSE)
  }
}

#' Convenience function that installs all extra packages used in ENMTools
#'
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
