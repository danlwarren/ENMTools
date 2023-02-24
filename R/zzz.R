globalVariables(c("f", "predict", "value"))

# Package setup options

.onLoad <- function(libname, pkgname){
  # options("rgdal_show_exportToProj4_warnings"="none")
  # utils::data(euro.worldclim, package = pkgname,
  #             envir = parent.env(environment()))
  # utils::data(iberolacerta.clade, package = pkgname,
  #             envir = parent.env(environment()))
  euro.worldclim <<- terra::rast(euro.worldclim)
  y <- iberolacerta.clade
  y$species <- lapply(iberolacerta.clade$species,
                      function(x) {
                        range <- terra::rast(x$range)
                        x$presence.points <- terra::vect(x$presence.points,
                                                         geom = c("Longitude", "Latitude"),
                                                         crs = terra::crs(range))
                        x$range <- range
                        x
                      })
  iberolacerta.clade <<- y

}

