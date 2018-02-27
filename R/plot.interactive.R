#' Plot an enmtools.species object on an interactive map
#'
#' Function that take an \code{\link{enmtools.species}} object and plots an
#' interactive map of the presence points, background points (if applicable), and
#' species range raster (if applicable). This function uses \code{\link{leaflet}} for mapping
#' and will only function properly if you have an active internet connection.
#'
#' @param x entools.species object to plot
#' @param map.provider Name of a map provider for the underlying interactive base map.
#' Default is "Esri.WorldPhysical", and attractive topographic map with no place labels. A
#' preview of all map provider options can be viewed at \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' @param plot.bg Should background points be plotted?
#' @param cluster.points Should points be clustered? If TRUE, points close together
#' will be grouped into clusters that can be interactively expanded by clicking
#' on them.
#' @export plot.interactive.enmtools.species
plot.interactive.enmtools.species <- function(x, map.provider = "Esri.WorldPhysical", plot.bg = FALSE, cluster.points = FALSE) {
  dat.df <- NULL
  if(class(x$background.points)  %in% c("data.frame", "matrix")){
    bg.df <- as.data.frame(x$background.points[,1:2])
    bg.df$present <- 0
    dat.df <- rbind(dat.df, bg.df)
  }
  if(class(x$presence.points)  %in% c("data.frame", "matrix")){
    pr.df <- as.data.frame(x$presence.points[ , 1:2])
    pr.df$present <- 1
    dat.df <- rbind(dat.df, pr.df)
  }

  pal <- colorFactor(c("grey10", "red"), domain = c("0", "1"))

  if(!plot.bg) {
    dat.df <- dat.df[dat.df$present == 1, ]
  }

  m <- leaflet(dat.df) %>%
    addProviderTiles(map.provider)

  if(class(x$range) == "RasterLayer"){
    m <- m %>%
      addRasterImage(x$range, function(y) ifelse(is.na(y), "#00000000", "#228B2273"))
  }

  if(cluster.points) {
    m <- m %>%
      addCircleMarkers(~Longitude, ~Latitude, color = ~pal(as.factor(present)),
                       stroke = FALSE, fillOpacity = 0.5, radius = 8,
                       clusterOptions = markerClusterOptions())
  } else {
    m <- m %>%
      addCircleMarkers(~Longitude, ~Latitude, color = ~pal(as.factor(present)),
                       stroke = FALSE, fillOpacity = 0.5, radius = 8)
  }

  if(plot.bg) {
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
#' @param map.provider Name of a map provider for the underlying interactive base map.
#' Default is "Esri.WorldPhysical", and attractive topographic map with no place labels. A
#' preview of all map provider options can be viewed at \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' @param plot.bg Should background points be plotted?
#' @param cluster.points Should points be clustered? If TRUE, points close together
#' will be grouped into clusters that can be interactively expanded by clicking
#' on them.
#' @export plot.interactive.enmtools.model
plot.interactive.enmtools.model <- function(x, map.provider = "Esri.WorldPhysical", plot.bg = FALSE, cluster.points = FALSE) {

  pal <- colorFactor(c("grey10", "red"), domain = c("0", "1"))

  if(!is.null(x$analysis.df$presence)) {
    pnts <- x$analysis.df[ , c("Longitude", "Latitude", "presence")]

    if(!plot.bg) {
      pnts <- pnts[pnts$presence == 1, ]
    }
  } else {
    pnts <- x$analysis.df[ , c("Longitude", "Latitude")]
  }

  m <- leaflet(pnts) %>%
    addProviderTiles(map.provider) %>%
    addRasterImage(x$suitability, colors = "viridis", opacity = 0.65)

  if(!is.null(x$analysis.df$presence)) {
    if(cluster.points) {
      m <- m %>%
        addCircleMarkers(~Longitude, ~Latitude, color = ~pal(as.factor(presence)),
                         stroke = FALSE, fillOpacity = 0.5, radius= 8,
                         clusterOptions = markerClusterOptions())
    } else {
      m <- m %>%
        addCircleMarkers(~Longitude, ~Latitude, color = ~pal(as.factor(presence)),
                         stroke = FALSE, fillOpacity = 0.5, radius= 8)
    }
  } else {
    if(cluster.points) {
      m <- m %>%
        addCircleMarkers(~Longitude, ~Latitude, color = "red",
                         stroke = FALSE, fillOpacity = 0.5, radius= 8,
                         clusterOptions = markerClusterOptions())
    } else {
      m <- m %>%
        addCircleMarkers(~Longitude, ~Latitude, color = "red",
                         stroke = FALSE, fillOpacity = 0.5, radius= 8)
    }
  }

  m <- m %>% addLegend(pal = colorNumeric("viridis", c(min(values(x$suitability), na.rm = TRUE),
                                                  max(values(x$suitability), na.rm = TRUE))), values = c(min(values(x$suitability), na.rm = TRUE),
                                              max(values(x$suitability), na.rm = TRUE)))

  if(plot.bg) {
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
