#' Plot an enmtools.model object on an interactive map
#'
#' Function that take an \code{enmtools.model} object and plots an
#' interactive map of the presence points, background points (if applicable), and
#' species suitability map. This function uses \code{\link[leaflet]{leaflet}} for mapping
#' and will only function properly if you have an active internet connection.
#'
#' @param x entools.model object to plot
#' @param map.provider Name of a map provider for the underlying interactive base map.
#' Default is "Esri.WorldPhysical", and attractive topographic map with no place labels. A
#' preview of all map provider options can be viewed at \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' @param raster.opacity Specifies the opacity level of the suitability raster.
#' @param cluster.points Should points be clustered? If TRUE, points close together
#' will be grouped into clusters that can be interactively expanded by clicking on them.
#' @param ... other arguments (not used currently)
#'
#' @return An interactive leaflet plot depicting the predictions and data from the enmtools.model object.

interactive.plot.enmtools.model <- function(x, map.provider = "Esri.WorldPhysical", cluster.points = FALSE, raster.opacity = 1, ...) {

  check.packages("leaflet")

  presence.points <- NA
  background.points <- NA


  if(!is.null(x$analysis.df$presence)) {
    pnts <- x$analysis.df[ , c("Longitude", "Latitude", "presence")]
    presence.points <- pnts[pnts$presence == 1, ]
    background.points <- pnts[pnts$presence == 0, ]
  } else {
    pnts <- x$analysis.df[ , c("Longitude", "Latitude")]
    presence.points <- x$analysis.df[ , c("Longitude", "Latitude")]
  }

  m <- leaflet::leaflet(pnts) %>%
    leaflet::addProviderTiles(map.provider, group = "Base map") %>%
    leaflet::addRasterImage(x$suitability, colors = "inferno", opacity = raster.opacity, group = "Model")

  if(is.data.frame(background.points)){
    m <- m %>%
      leaflet::addCircleMarkers(~Longitude, ~Latitude, color = "black",
                       stroke = FALSE, fillOpacity = 0.5, radius= 8,
                       data = background.points, group = "Background points")
  }

  if(cluster.points) {
    m <- m %>%
      leaflet::addCircleMarkers(~Longitude, ~Latitude, color = "red",
                       stroke = FALSE, fillOpacity = 0.5, radius= 8,
                       clusterOptions = leaflet::markerClusterOptions(),
                       data = presence.points, group = "Training points")
  } else {
    m <- m %>%
      leaflet::addCircleMarkers(~Longitude, ~Latitude, color = "red",
                       stroke = FALSE, fillOpacity = 0.5, radius= 8,
                       data = presence.points, group = "Training points")
  }


  if(is.data.frame(x$test.data)){
    m <- m %>%
      leaflet::addCircleMarkers(~Longitude, ~Latitude, color = "green",
                       stroke = FALSE, fillOpacity = 0.5, radius= 8,
                       group = "Test points", data = x$test.data)
  }

  m <- m %>%
    leaflet::addLegend(pal = leaflet::colorNumeric("inferno", c(min(values(x$suitability), na.rm = TRUE),
                                                       max(values(x$suitability), na.rm = TRUE))), values = c(min(values(x$suitability), na.rm = TRUE),
                                                                                                              max(values(x$suitability), na.rm = TRUE)))


  if(is.data.frame(x$test.data) & is.data.frame(background.points)){
    m <- m %>% leaflet::addLayersControl(
      overlayGroups = c("Base map", "Model", "Training points", "Test points", "Background points"),
      options = leaflet::layersControlOptions(collapsed = FALSE, position = "bottomleft")
    )

    m <- m %>%
      leaflet::addLegend("bottomright", colors = c("green", "red", "black"),
                labels = c("Test points", "Training presences", "Background points"))

  } else if(!is.data.frame(x$test.data) & is.data.frame(background.points)) {
    m <- m %>% leaflet::addLayersControl(
      overlayGroups = c("Base map", "Model", "Training points", "Background points"),
      options = leaflet::layersControlOptions(collapsed = FALSE, position = "bottomleft")
    )

    m <- m %>%
      leaflet::addLegend("bottomright", colors = c("red", "black"),
                labels = c("Training presences", "Background points"))

  } else if(is.data.frame(x$test.data) & !is.data.frame(background.points)){
    m <- m %>% leaflet::addLayersControl(
      overlayGroups = c("Base map", "Model", "Training points", "Test points"),
      options = leaflet::layersControlOptions(collapsed = FALSE, position = "bottomleft")
    )

    m <- m %>%
      leaflet::addLegend("bottomright", colors = c("green", "red"),
                labels = c("Test points", "Training presences"))

  } else {
    m <- m %>% leaflet::addLayersControl(
      overlayGroups = c("Base map", "Model", "Training points"),
      options = leaflet::layersControlOptions(collapsed = FALSE, position = "bottomleft")
    )

    m <- m %>%
      leaflet::addLegend("bottomright", colors = c("red"),
                labels = c("Training presences"))
  }

  m
}
