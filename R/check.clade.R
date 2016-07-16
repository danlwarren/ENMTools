#' Checking for complians of an enmtools.clade object
#' @param this.clade An enmtools.clade object
#'
#' @export check.clade


check.clade <- function(this.clade){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  if(!isTRUE(is.na(this.clade$species))){

    # Checking to see if species is a list
    if(!"list" %in% class(this.clade$species)){
      stop("Argument species requires a list of enmtools.species objects")
    }

    # This if statement is asking whether any of the list elements don't have
    # enmtools.species in their class definitions
    if(any(unlist(lapply(this.clade$species, function(x) !"enmtools.species" %in% class(x))))){
      stop("The following objects in the species list do not appear to be enmtools.species objects:")
      stop(names(which(unlist(lapply(this.clade$species, function(x) !"enmtools.species" %in% class(x))))))
    }
    lapply(this.clade$species, function(x) check.species(x))

  }

  if(!isTRUE(is.na(this.clade$tree))){
    # Checking to see if species is a list
    if(!"phylo" %in% class(this.clade$tree)){
      stop("Argument tree requires a phylo object")
    }
  }

  # Might turn off returning the clade eventually
  return(this.clade)
}

