#' Takes a an ENMTools clade object and a vector of species names.  Drops the species from the tree and removes data from the clade object.
#'
#' @param clade An ENMTools clade object
#' @param species A name or vector of names of species within the enmtools.clade object.
#'
#' @examples
#' data(iberolacerta.clade)
#' new.clade <- drop.species(iberolacerta.clade, c("cyreni", "monticola"))


drop.species <- function(clade, species){

  check.package("ape")

  if(!inherits(clade, "enmtools.clade")){
    stop("Clade is not an enmtools.clade object!")
  }

  if(any(!species %in% names(clade$species))){
    stop(paste("Species not found in clade: ", species[!species %in% clade$species]))
  }

  dropped <- clade

  dropped$species <- dropped$species[!names(dropped$species) %in% species]

  dropped$tree <- ape::drop.tip(dropped$tree, species)

  return(dropped)
}
