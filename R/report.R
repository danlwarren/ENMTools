#' report Build an html report file for an enmtools obect
#'
#'
#' @param x An object of class enmtools.species, enmtools.clade, enmtools.model, or enmtools.hypothesis.test
#' @param title The title to print in the report file
#' @param author The author name to print in the report file
#' @param date Report generation date - will be automatically populated if not provided
#' @param path Path for saving the .html report file
#' @param ... Extra arguments to be passed to the knit function
#'
#' @return Returns the path to the .html report file
#'
#' @keywords ENMTools report html
#'
#' @examples
#' report(iberolacerta.clade$species$monticola)
#'

report <- function(x, path, title, author, date = NA, overwrite = TRUE, ...){

  print(path)

  if(file.exists(path) & overwrite == FALSE){
    stop("File exists and overwrite is set to FALSE")
  }

  if(is.na(date)){
    date = date()
  }

  if(inherits(x, "enmtools.species")){
    rmarkdown::render("inst/rmarkdown/templates/enmtools-species/skeleton/skeleton.rmd",
                output_file = path,
                params = list(x = x,
                              title = title,
                              author = author,
                              date = date))

  } else if(inherits(x, "enmtools.clade")){

  } else if(inherits(x, "enmtools.model")){

  } else if(inherits(x, "enmtools.hypothesis.test")){

  }

  return(path)
}
