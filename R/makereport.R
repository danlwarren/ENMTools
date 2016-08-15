#'  makereport, creates an html document containing information about an enmtools object
#'
#' @param x An enmtools.model, species, test, or clade object
#' @param outfile Path to an output file
#'
#' @return NULL
#'
#' @keywords niche plot sdm enm overlap report output html
#'
#' @export makereport
#'
#' @examples
#' makereport(ahli.glm, "./ahli.html")

makereport <- function(x, outfile){

  path <- paste0(path.package("ENMTools"), "/reports/")

  # GLM
  if(inherits(x, "enmtools.glm"))  {
    knit2html(paste0(path, 'makereport.glm.Rmd'), outfile, force_v1 = TRUE)
  }

  # GAM
  if(inherits(x, "enmtools.gam"))  {
    knit2html(paste0(path, 'makereport.gam.Rmd'), outfile, force_v1 = TRUE)
  }

  # BIOCLIM
  if(inherits(x, "enmtools.bc"))  {
    knit2html(paste0(path, 'makereport.bc.Rmd'), outfile, force_v1 = TRUE)
  }

  # Domain
  if(inherits(x, "enmtools.dm"))  {
    knit2html(paste0(path, 'makereport.dm.Rmd'), outfile, force_v1 = TRUE)
  }

  # Maxent
  if(inherits(x, "enmtools.maxent"))  {
    knit2html(paste0(path, 'makereport.mx.Rmd'), outfile, force_v1 = TRUE)
  }

}
