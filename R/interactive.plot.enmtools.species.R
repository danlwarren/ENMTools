#' Plot an enmtools.species object on an interactive map
#'
#' Function that take an \code{\link{enmtools.species}} object and plots an
#' interactive map of the presence points, background points (if applicable), and
#' species range raster (if applicable). This function uses \code{\link[leaflet]{leaflet}} for mapping
#' and will only function properly if you have an active internet connection.
#'
#' @param x entools.species object to plot
#' @param map.provider Name of a map provider for the underlying interactive base map.
#' Default is "Esri.WorldPhysical", and attractive topographic map with no place labels. A
#' preview of all map provider options can be viewed at \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' @param cluster.points Should points be clustered? If TRUE, points close together
#' will be grouped into clusters that can be interactively expanded by clicking
#' on them.
#' @param ... other arguments (not used currently)
#'
#' @return An interactive leaflet plot visualizing the data present in the species object.

interactive.plot.enmtools.species <- function(x, map.provider = "Esri.WorldPhysical", cluster.points = FALSE, ...) {

  check.packages("leaflet")

  m <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(map.provider, group = "Base map")

  overlays <- "Base map"
  labels <- NULL
  colors <- NULL

  if(class(x$range) == "RasterLayer"){
    m <- m %>%
      leaflet::addRasterImage(x$range, function(y) ifelse(is.na(y), "#00000000", "#00000090"),
                     group = "Range")
    overlays <- c(overlays, "Range")
    labels <- c(labels, "Range raster")
    colors <- c(colors, "black")
  }


  if(class(x$background.points)  %in% c("data.frame", "matrix")){
    background.points <- x$background.points
    m <- m %>%
      leaflet::addCircleMarkers(~Longitude, ~Latitude, color = "red",
                       data = background.points, stroke = FALSE, fillOpacity = 0.5,
                       radius = 8, group = "Background points")
    overlays <- c(overlays, "Background points")
    labels <- c(labels, "Background points")
    colors <- c(colors, "red")
  }

  if(class(x$presence.points)  %in% c("data.frame", "matrix")){
    presence.points <- x$presence.points
    if(cluster.points) {
      m <- m %>%
        leaflet::addCircleMarkers(~Longitude, ~Latitude, color = "green",
                         stroke = FALSE, fillOpacity = 0.5, radius = 8,
                         data = presence.points, clusterOptions = leaflet::markerClusterOptions(),
                         group = "Occurrence points")
    } else {
      m <- m %>%
        leaflet::addCircleMarkers(~Longitude, ~Latitude, color = "green",
                         data = presence.points, stroke = FALSE, fillOpacity = 0.5,
                         radius = 8, group = "Occurrence points")
    }
    overlays <- c(overlays, "Occurrence points")
    labels <- c(labels, "Occurrence points")
    colors <- c(colors, "green")
  }

  m <- m %>% leaflet::addLayersControl(
    overlayGroups = overlays,
    options = leaflet::layersControlOptions(collapsed = FALSE, position = "bottomleft")
  )

  # Build the legend given what's defined

  m <- m %>%
    leaflet::addLegend("bottomright", colors = colors,
              labels = labels)

  m

}
