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
#' @param max.bytes Maximum size of range raster image to plot.  Defaults to 4MB (4194304 bytes) but can be overridden if you have a large range raster.  Be aware that the image will be knitted into an output file if you're working in R Markdown, causing your output file to be huge if the raster is huge.
#' @param ... other arguments (not used currently)
#'
#' @return An interactive leaflet plot visualizing the data present in the species object.

interactive.plot.enmtools.species <- function(x, map.provider = "Esri.WorldPhysical", cluster.points = FALSE, max.bytes = 4194304, ...) {

  assert.extras.this.fun()

  m <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(map.provider, group = "Base map")

  overlays <- "Base map"
  labels <- NULL
  colors <- NULL

  if(inherits(raster::raster(x$range), "RasterLayer")){
    m <- m %>%
      leaflet::addRasterImage(raster::raster(x$range), function(y) ifelse(is.na(y), "#00000000", "#00000090"),
                     group = "Range", maxBytes = max.bytes)
    overlays <- c(overlays, "Range")
    labels <- c(labels, "Range raster")
    colors <- c(colors, "black")
  }


  if(class(x$background.points) %in% c("SpatVector")){
    background.points <- terra::as.data.frame(x$background.points, geom = "XY")
    m <- m %>%
      leaflet::addCircleMarkers(~x, ~y, color = "red",
                       data = background.points, stroke = FALSE, fillOpacity = 0.5,
                       radius = 8, group = "Background points")
    overlays <- c(overlays, "Background points")
    labels <- c(labels, "Background points")
    colors <- c(colors, "red")
  }

  if(class(x$presence.points)  %in% c("SpatVector")){
    presence.points <- terra::as.data.frame(x$presence.points, geom = "XY")
    if(cluster.points) {
      m <- m %>%
        leaflet::addCircleMarkers(~x, ~y, color = "green",
                         stroke = FALSE, fillOpacity = 0.5, radius = 8,
                         data = presence.points, clusterOptions = leaflet::markerClusterOptions(),
                         group = "Occurrence points")
    } else {
      m <- m %>%
        leaflet::addCircleMarkers(~x, ~y, color = "green",
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
