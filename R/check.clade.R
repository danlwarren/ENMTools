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
    if(!inherits(this.clade$species, "list")){
      stop("Argument species requires a list of enmtools.species objects")
    }

    # This if statement is asking whether any of the list elements don't have
    # enmtools.species in their class definitions
    if(any(unlist(lapply(this.clade$species, function(x) !inherits(x, "enmtools.species"))))){
      stop("The following objects in the species list do not appear to be enmtools.species objects:")
      stop(names(which(unlist(lapply(this.clade$species, function(x) !inherits(x, "enmtools.species"))))))
    }
    lapply(this.clade$species, function(x) check.species(x))
    names(this.clade$species) <- lapply(this.clade$species, function(x) x$species.name)

  }

  if(!isTRUE(is.na(this.clade$tree))){
    # Checking to see if species is a list
    if(!inherits(this.clade$tree, "phylo")){
      stop("Argument tree requires a phylo object")
    }
  }

  # Build a summary table of data, chuck it into clade object
  species.name <- names(this.clade$species)

  in.tree <- rep(NA, length(species.name))
  if(!isTRUE(is.na(this.clade$tree))){
    in.tree <- species.name %in% this.clade$tree$tip.label
  }

  presence <- lapply(this.clade$species, function(x) nrow(x$presence.points))

  background <- unlist(lapply(this.clade$species, function(x) nrow(x$background.points)))
  background[which(is.null(background))] <- 0

  range <- lapply(this.clade$species, function(x) !is.na(x$range))

  this.clade$summary <- cbind(species.name, in.tree, presence, background, range)

  return(this.clade)
}

