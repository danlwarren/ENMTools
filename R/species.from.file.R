#' Takes a csv file and uses it to construct one or more enmtools.species objects. It constructs one object per unique value in the "species.col" column.
#'
#' @param filename Name of a .csv file
#' @param species.col Name of the column from the csv file to use for species names.  Default is "species".
#'
#' @return A list containing species objects, one for each unique value in species.col.

species.from.file <- function(filename, species.col = "species"){

  input.df <- read.csv(filename, header = TRUE)

  if(!species.col %in% colnames(input.df)){
    stop(paste("Column", species.col, "not found in", filename))
  }

  sp.names <- unique(input.df[,species.col])

  if(length(sp.names) == 1){

    # There's only one species in the file, return an enmtools.species object
    this.species <- enmtools.species(species.name = as.character(sp.names[1]),
                                     presence.points = input.df[input.df[,species.col] == sp.names[1],])
    this.species <- check.species(this.species)

    return(this.species)

  } else {

    species.list <- list()

    # More than one species in the file, create and return a list of enmtools.species objects
    for(i in sp.names){
      this.species <- enmtools.species(species.name = i,
                                       presence.points = input.df[input.df[,species.col] == i,])
      this.species <- check.species(this.species)
      species.list[[i]] <- this.species
    }

    return(species.list)

  }
}
