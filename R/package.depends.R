#' install.extras
#'
#' Convenience function that installs all extra packages used in ENMTools.  ENMTools uses
#' functions from a lot of external packages, and due to CRAN best practices it doesn't install
#' those packages by default.  The function install.extras() calls install.packages with a
#' list of all of the extra packages that are not already available. Use [check.extras()] to find out which packages are needed for a list of functions.
#'
#' @param funs character vector of ENMTools function names to install extra dependencies for. If `NULL`, extras for all ENMTools functions will be installed.
#' @param install Should the packages be installed?
#' @param ... Other parameters to be passed to \code{install.packages}
#'
#' @return The list of missing packages is return invisibly.
#'
#' @examples
#' \donttest{
#' install.extras(install = FALSE)
#' }
install.extras <- function(funs = NULL, install = TRUE, ...) {
  options(install.packages.check.source = "no")

  installs <- find.extras.missing(funs)

  if(install && length(installs) > 0){
    install.packages(installs, ...)
  }

  invisible(installs)

}

#' Assert that the extra packages needed for an ENMTools function are installed and available
#'
#' Triggers an error if any of the extra packages required for an ENMTools function are not available.
#'
#' @inheritParams install.extras
#'
#' @return None, this function is used for its side-effects
#' @export
#'
#' @examples
#' if(check.extras("enmtools.gam")) {
#'   assert.extras("enmtools.gam")
#' }
assert.extras <- function(funs = NULL) {

  missing.packages <- find.extras.missing(funs)

  if(length(missing.packages) > 0){
    stop("The following packages are needed for this function to work but are not installed. \n\n
You can install extra packages used in ENMTools manually or install all of them at once by running install.extras().\n\nPackage list: ",
         paste(missing.packages,  collapse = ", "),
         call. = FALSE)

  }

}

#' Find the extra packages needed for an ENMTools function
#'
#' @inheritParams install.extras
#'
#' @return A character vector with the names of extra packages required by `funs`
#' @export
#'
#' @examples
#' find.extras("enmtools.calibrate")
find.extras <- function(funs = NULL) {

  depends <- list(`drop.species` = "ape",
                  `enmtools.aoc` = "ape",
                  `enmtools.calibrate` = c("ecospat", "CalibratR", "caret", "ResourceSelection"),
                  `enmtools.ecospat.bg` = "ecospat",
                  `enmtools.ecospat.id` = "ecospat",
                  `enmtools.gam` = "mgcv",
                  `enmtools.hypervolume` = "hypervolume",
                  `enmtools.maxent` = "rJava",
                  `enmtools.rf` = "randomForest",
                  `enmtools.rf.ranger` = "ranger",
                  `enmtools.tidy` = "tidymodels",
                  `enmtools.vip` = c("vip", "pdp", "fastshap", "reshape2", "viridis"),
                  `env.overlap` = "CalibratR",
                  `hypervolume.overlap` = "hypervolume",
                  `interactive.plot` = "leaflet",
                  `node.overlap` = "ape",
                  `get.mult` = "ape",
                  `raster.cor.plot` = "reshape2")

  if(!is.null(funs)) {
    res <- unique(unlist(depends[funs]))
  } else {
    res <- unique(unlist(depends))
  }

  res

}

#' Find the extra packages needed for an ENMTools function that are currently missing (not available)
#'
#' @inheritParams install.extras
#'
#' @return A character vector with the names of extra packages required by `funs` that are currently not available
#' @export
#'
#' @examples
#' find.extras.missing("enmtools.calibrate")
find.extras.missing <- function(funs = NULL) {

  package.names <- find.extras(funs)

  missing.packages <- c()

  for(i in package.names){
    if (!requireNamespace(i, quietly = TRUE)) {
      missing.packages <- c(missing.packages, as.character(i))
    }
  }

  missing.packages
}

#' Check that the extra packages needed for an ENMTools function are installed and available
#'
#' @inheritParams install.extras
#'
#' @return Logical, `TRUE` if all required extra packages are available, `FALSE` otherwise
#' @export
#'
#' @examples
#' check.extras("enmtools.gam")
check.extras <- function(funs = NULL) {
  length(find.extras.missing(funs)) == 0
}

## uses some code from here: https://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine
## This function passes the name of its calling function to assert.extras(). When called from a function it throws an error if the
## extras required for that function are not available
assert.extras.this.fun <- function() {
  fun <- deparse(sys.calls()[[sys.nframe()-1]][[1]])
  assert.extras(fun)
}
