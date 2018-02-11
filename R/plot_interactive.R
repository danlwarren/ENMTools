#' Plot an enmtools.species object on an interactive map
#'
#' Function that take an \code{\link{enmtools.species}} object and plots an
#' interactive map of the presence points, background points (if applicable), and
#' species range raster (if applicable). This function uses \code{\link{leaflet}} for mapping
#' and will only function properly if you have an active internet connection.
#'
#' @param x entools.species object to plot
#' @param map_provider Name of a map provider for the underlying interactive base map.
#' Default is "Esri.WorldPhysical", and attractive topographic map with no place labels. A
#' preview of all map provider options can be viewed at \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' @param plot_bg Should background points be plotted?
#' @param cluster_points Should points be clustered? If TRUE, points close together
#' will be grouped into clusters that can be interactively expanded by clicking
#' on them.
#' @export plot_interactive.enmtools.species
plot_interactive.enmtools.species <- function(x, map_provider = "Esri.WorldPhysical", plot_bg = FALSE, cluster_points = FALSE) {
  dat_df <- NULL
  if(class(x$background.points)  %in% c("data.frame", "matrix")){
    bg_df <- as.data.frame(x$background.points[,1:2])
    bg_df$present <- 0
    dat_df <- rbind(dat_df, bg_df)
  }
  if(class(x$presence.points)  %in% c("data.frame", "matrix")){
    pr_df <- as.data.frame(x$presence.points[ , 1:2])
    pr_df$present <- 1
    dat_df <- rbind(dat_df, pr_df)
  }

  pal <- colorFactor(c("grey10", "red"), domain = c("0", "1"))

  if(!plot_bg) {
    dat_df <- dat_df[dat_df$present == 1, ]
  }

  m <- leaflet(dat_df) %>%
    addProviderTiles(map_provider)

  if(class(x$range) == "RasterLayer"){
    m <- m %>%
      addRasterImage(x$range, function(y) ifelse(is.na(y), "#00000000", "#228B2273"))
  }

  if(cluster_points) {
    m <- m %>%
      addCircleMarkers(~Longitude, ~Latitude, color = ~pal(as.factor(present)),
                       stroke = FALSE, fillOpacity = 0.5, radius = 8,
                       clusterOptions = markerClusterOptions())
  } else {
    m <- m %>%
      addCircleMarkers(~Longitude, ~Latitude, color = ~pal(as.factor(present)),
                       stroke = FALSE, fillOpacity = 0.5, radius = 8)
  }

  if(plot_bg) {
    m <- m %>%
      addLegend("bottomright", colors = "#228B22", labels = "Species Range", opacity = 0.45) %>%
      addLegend("bottomright", pal = pal, values = c("0", "1"),
                labFormat = function(type, x) {
                  labs = c("0" = "Background points",
                           "1" = "Presence points")
                  labs[x]
                })
  } else {
    m <- m %>%
      addLegend("bottomright", colors = "#228B22", labels = "Species Range", opacity = 0.45) %>%
      addLegend("bottomright", pal = pal, values = c("1"),
                labFormat = function(type, x) {
                  labs = c("0" = "Background points",
                           "1" = "Presence points")
                  labs[x]
                })
  }

  m

}

#' Plot an enmtools.model object on an interactive map
#'
#' Function that take an \code{enmtools.model} object and plots an
#' interactive map of the presence points, background points (if applicable), and
#' species suitability map. This function uses \code{\link{leaflet}} for mapping
#' and will only function properly if you have an active internet connection.
#'
#' @param x entools.model object to plot
#' @param map_provider Name of a map provider for the underlying interactive base map.
#' Default is "Esri.WorldPhysical", and attractive topographic map with no place labels. A
#' preview of all map provider options can be viewed at \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' @param plot_bg Should background points be plotted?
#' @param cluster_points Should points be clustered? If TRUE, points close together
#' will be grouped into clusters that can be interactively expanded by clicking
#' on them.
#' @export plot_interactive.enmtools.model
plot_interactive.enmtools.model <- function(x, map_provider = "Esri.WorldPhysical", plot_bg = FALSE, cluster_points = FALSE) {

  pal <- colorFactor(c("grey10", "red"), domain = c("0", "1"))

  pnts <- x$analysis.df[ , c("Longitude", "Latitude", "presence")]

  if(!plot_bg) {
    pnts <- pnts[pnts$presence == 1, ]
  }

  m <- leaflet(pnts) %>%
    addProviderTiles(map_provider) %>%
    addRasterImage(x$suitability, colors = "viridis", opacity = 0.65)


  if(cluster_points) {
    m <- m %>%
      addCircleMarkers(~Longitude, ~Latitude, color = ~pal(as.factor(presence)),
                       stroke = FALSE, fillOpacity = 0.5, radius= 8,
                       clusterOptions = markerClusterOptions())
  } else {
    m <- m %>%
      addCircleMarkers(~Longitude, ~Latitude, color = ~pal(as.factor(presence)),
                       stroke = FALSE, fillOpacity = 0.5, radius= 8)
  }


  m <- m %>% addLegend(pal = colorNumeric("viridis", c(min(values(x$suitability), na.rm = TRUE),
                                                  max(values(x$suitability), na.rm = TRUE))), values = c(min(values(x$suitability), na.rm = TRUE),
                                              max(values(x$suitability), na.rm = TRUE)))

  if(plot_bg) {
    m <- m %>%
      addLegend("bottomright", pal = pal, values = c("0", "1"),
                     labFormat = function(type, x) {
                       labs = c("0" = "Background points",
                                "1" = "Presence points")
                       labs[x]
                     })
  } else {
    m <- m %>%
      addLegend("bottomright", pal = pal, values = c("1"),
                labFormat = function(type, x) {
                  labs = c("0" = "Background points",
                           "1" = "Presence points")
                  labs[x]
                })
  }

  m
}
