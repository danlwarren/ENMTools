#' Takes a an ENMTools clade object and a vector of species names.  Drops the species from the tree and removes data from the clade object.
#'
#' @param clade An ENMTools clade object
#' @param species A name or vector of names of species within the enmtools.clade object.
#'
#' @return An enmtools.clade object with the provided species dropped both from the tree and from the set of enmtools.species objects.
#'
#' @examples
#' \dontrun{
#' data(iberolacerta.clade)
#' if(requireNamespace("ape", quietly = TRUE)) {
#'     new.clade <- drop.species(iberolacerta.clade, c("cyreni", "monticola"))
#' }
#' }


drop.species <- function(clade, species){

  check.packages("ape")

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
