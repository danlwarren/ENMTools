#' Checking for complians of an enmtools.clade object
#' @param this.clade An enmtools.clade object
#'
#' @return An enmtools.clade object with appropriate formatting.
#'
#' @examples
#' data(iberolacerta.clade)
#' check.clade(iberolacerta.clade)


check.clade <- function(this.clade){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  # This bit replaces NULL values with NA values
  expect <- c("species", "tree")
  nulls <- names(which(sapply(expect, function(x) is.null(this.clade[[x]]))))

  # Have to do this in a loop because sapply won't assign NAs for some reason
  for(i in nulls){
    this.clade[[i]] <- NA
  }


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

  } else {
    # this.clade$species is just an NA
    stop("Argument species requires a list of enmtools.species objects, is currently NA")
  }

  if(!isTRUE(is.na(this.clade$tree))){
    # Checking to see if species is a list
    if(!inherits(this.clade$tree, "phylo")){
      stop("Argument tree requires a phylo object")
    }
  }


  # Make sure species names exist and match names in species list
  # then reorder list to match tree tip labels
  species.names <- unlist(lapply(this.clade$species, function(x) x$species.name))
  if(!all(is.character(species.names))){
    stop(paste("Some species in clade to not have names set.  Existing names are: ", paste(species.names, collapse= ", ")))
  }

  # Rename species list to match species names
  names(this.clade$species) <- species.names

  # Check names and tip labels against each other
  if(any(is.na(match(names(this.clade$species), this.clade$tree$tip.label)))){
    missing <- which(is.na(match(names(this.clade$species), this.clade$tree$tip.label)))
    stop(paste("Species in clade not found in tree: ", paste(names(this.clade$species)[missing]), collapse = ", "))
  }

  if(any(is.na(match(this.clade$tree$tip.label, names(this.clade$species))))){
    missing <- which(is.na(match(this.clade$tree$tip.label, names(this.clade$species))))
    stop(paste("Species in tree not found in clade: ", paste(this.clade$tree$tip.label[missing]), collapse = ", "))
  }

  # Reorder list to match tree
  this.clade$species <- this.clade$species[this.clade$tree$tip.label]

  # Build a summary table of data, chuck it into clade object
  in.tree <- rep(NA, length(species.names))
  if(!isTRUE(is.na(this.clade$tree))){
    in.tree <- species.names %in% this.clade$tree$tip.label
  }

  presence <- lapply(this.clade$species, function(x) nrow(x$presence.points))

  background <- unlist(lapply(this.clade$species, function(x) nrow(x$background.points)))
  background[which(is.null(background))] <- 0


  range <- unlist(lapply(this.clade$species, function(x) inherits(x$range, c("raster", "RasterLayer"))))
  range[which(range == TRUE)] <- "present"
  range[which(range == FALSE)] <- "absent"

  this.clade$summary <- cbind(species.names, in.tree, presence, background, range)

  return(this.clade)
}

