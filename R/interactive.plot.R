#' Generic function for making interactive plots of ENMTools models and species
#'
#' Function that takes an \code{enmtools.model} or \code{enmtools.species}  object and
#' calls the class-appropriate \code{interactive.plot.xx} function for it.
#' These functions use \code{\link[leaflet]{leaflet}} for mapping
#' and will only function properly if you have an active internet connection.
#'
#' @param x entools.model or species object to plot
#' @param ... other arguments passed to interactive.plot.enmtools.model or interactive.plot.enmtools.species
#'
#' @return An interactive leaflet plot depicting the predictions and data from the enmtools.model object.

interactive.plot <- function (x, ...) {
  UseMethod("interactive.plot", x)
}
