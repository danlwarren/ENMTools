#' Defining a class for enmtools.clade.  Each clade gets:
#' @param species A list of enmtools.species objects
#' @param tree A tree showing the relationships between the species
#'
#' @export enmtools.clade
#' @export summary.enmtools.clade
#' @export print.enmtools.clade
#' @export plot.enmtools.clade


enmtools.clade <- function(species = NA, tree = NA, root.species = NA){

   # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
   # know how to do is.na on raster data, so it was barfing an error when a raster
   # was passed in.

   if(!isTRUE(is.na(species))){

      # Checking to see if species is a list
      if(!"list" %in% class(species)){
         print("Argument species requires a list of enmtools.species objects")
      }

      # This if-statement is asking whether any of the list elements don't have
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

summary.enmtools.clade <- function(this.clade){

  this.clade <- check.clade(this.clade)

  cat(paste("\n\nAn enmtools.clade object with", length(this.clade$species), "species\n"))

  cat("\nSpecies names: \n")
  cat(paste("\t", lapply(this.clade$species, function(x) x$species.name)))

  cat("\n\nTree: \n")
  print(this.clade$tree)

  cat("\n\nData Summary: \n")
  print(kable(this.clade$summary))

  cat("\n")
}

plot.enmtools.clade <- function(this.clade){

  # Figure out how many plots you need.  We'll do one for each species (up to 15)
  # and one for the tree.
  n.plot <- min(16, length(this.clade$species))

  # We'll use this to keep track of how many plots we've made
  plotted <- 0

  # Figure out how many rows and columns we need, declare a new plot
  n.rows <- ceiling(sqrt(n.plot))
  n.cols <- ceiling(n.plot/n.rows)

  plot.new()
  par(mfrow = c(n.rows, n.cols))

  for(i in 1:n.plot){
    plot(this.clade$species[[i]])
  }

  par(mfrow = c(1,1))

}

print.enmtools.clade <- function(this.clade){
  summary(this.clade)
}
