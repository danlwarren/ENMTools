#' Takes a list of enmtools.species objects and merges them into a single enmtools.species object
#'
#' @param species.list List of enmtools.species objects that you want to merge together
#'
#' @export merge.species


merge.species <- function(species.list){
  merged <- species.list[[1]]

  # Add data from other species
  for(i in 2:length(species.list)){
    merged$presence.points <- rbind(merged$presence.points, species.list[[i]][["presence.points"]])
    merged$background.points <- rbind(merged$background.points, species.list[[i]][["background.points"]])
    merged$range <- merge(merged$range, species.list[[i]][["range"]])
    merged$species.name <- paste(merged$species.name, species.list[[i]][["species.name"]], sep = " + ")
  }

  return(merged)
}
