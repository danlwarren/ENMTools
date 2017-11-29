#' Takes a list of enmtools.species objects and combines them into a single enmtools.species object
#'
#' @param species.list List of enmtools.species objects that you want to combine together
#'
#' @export combine.species


combine.species <- function(species.list){
  combined <- species.list[[1]]

  # Add data from other species
  for(i in 2:length(species.list)){
    combined$presence.points <- rbind(combined$presence.points, species.list[[i]][["presence.points"]])
    combined$background.points <- rbind(combined$background.points, species.list[[i]][["background.points"]])
    combined$range <- merge(combined$range, species.list[[i]][["range"]])
    combined$species.name <- paste(combined$species.name, species.list[[i]][["species.name"]], sep = " + ")
  }

  return(combined)
}
