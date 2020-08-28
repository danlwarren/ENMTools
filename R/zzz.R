globalVariables(c("f", "predict", "value"))

# Package setup options

.onLoad <- function(libname, pkgname){
  options("rgdal_show_exportToProj4_warnings"="none")
}
