#' Defining a class for enmtools.clade.  Each clade gets:
#' @param species A list of enmtools.species objects
#' @param tree A tree showing the relationships between the species
#'
#' @export enmtools.clade


enmtools.clade <- function(species = NA, tree = NA, root.species = NA){

   # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
   # know how to do is.na on raster data, so it was barfing and error when a raster
   # was passed in.

   if(!isTRUE(is.na(species))){

      # Checking to see if species is a list
      if(!"list" %in% class(species)){
         print("Argument species requires a list of enmtools.species objects")
      }

      # This if statement is asking whether any of the list elements don't have
      # enmtools.species in their class definitions
      if(any(unlist(lapply(species, function(x) !"enmtools.species" %in% class(x))))){
         print("The following objects in the species list do not appear to be enmtools.species objects:")
         print(names(which(unlist(lapply(species, function(x) !"enmtools.species" %in% class(x))))))
      }

   }

   if(!isTRUE(is.na(tree))){
      # Checking to see if species is a list
      if(!"phylo" %in% class(tree)){
         print("Argument tree requires a phylo object")
      }
   }

   output <- list(species = species,
                  tree = tree)

   class(output) <- c("list", "enmtools.clade")

   return(output)
}

