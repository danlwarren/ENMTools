plot_interactive <- function(x, map_provider = "Esri.WorldPhysical") {
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

  m <- leaflet(dat_df) %>%
    addProviderTiles(map_provider) %>%
    addCircleMarkers(~Longitude, ~Latitude, color = "red")

  if(class(x$range) == "RasterLayer"){
    m <- m %>%
      addRasterImage(x$range, function(y) ifelse(is.na(y), "#00000000", "#228B2273")) %>%
      addLegend("bottomright", colors = "#228B22", labels = "Species Range", opacity = 0.45)
  }


  m

}
