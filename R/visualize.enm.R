#' visualize.enm, Makes a heatmap of suitability of habitat in environment space according to a given model
#'
#' @param model An enmtools.model object
#' @param env A set of environmental layers
#' @param nbins The number of bins per layer to use for drawing environment space
#' @param layers A vector of layer names to use for drawing environment space
#' @param plot.points Logical determining whether presence points should be plotted on suitability plot
#' @param plot.test.data Logical determining whether test data should be plotted, if present.  If test data is plotted, it will appear as translucent green triangles.
#' @param minmax Either "env", to set the minima and maxima using the environment layers, "points" to set the minima and maxima using the presence and background data, or a named list of minima and maxima for each layer.
#'
#' @return suit.plot A list containing two dimensional plot of an ENM in environment space and a plot of the available environments.
#'
#' @keywords niche plot sdm enm
#'
#' @examples
#' aurelioi.glm <- enmtools.glm(iberolacerta.clade$species$aurelioi, euro.worldclim,
#' f = pres ~ poly(bio1, 4) + poly(bio12, 4))
#' visualize.enm(aurelioi.glm, euro.worldclim, layers = c("bio1", "bio12"))

visualize.enm <- function(model, env, nbins = 100, layers = colnames(model$analysis.df)[1:2], plot.test.data = FALSE, plot.points = TRUE, minmax = "env"){

  if(!inherits(model, "enmtools.model")){
    stop("This function requires an enmtools.model object!")
  }

  if(length(layers) != 2){
    stop("Layers argument must be a character vector specifying two environmental layers to use for plotting!")
  }

  if(!all(layers %in% names(env))){
    stop("Layer names provided do not match environmental rasters!")
  }

  # Grab points
  if(inherits(model, c("enmtools.bc", "enmtools.dm"))){
    points <- model$analysis.df
  } else {
    points <- model$analysis.df[model$analysis.df$presence == 1,1:2]
  }

  # Getting min and max for each env layer
  if(minmax == "env"){

    # Getting values from rasters
    layer1.min <- min(terra::values(env[[layers[1]]]), na.rm=TRUE)
    layer2.min <- min(terra::values(env[[layers[2]]]), na.rm=TRUE)
    layer1.max <- max(terra::values(env[[layers[1]]]), na.rm=TRUE)
    layer2.max <- max(terra::values(env[[layers[2]]]), na.rm=TRUE)

  } else if(minmax == "points"){

    # Getting values from presence and background
    layer1.min <- min(model$analysis.df[,layers[1]], na.rm = TRUE)
    layer2.min <- min(model$analysis.df[,layers[2]], na.rm = TRUE)
    layer1.max <- max(model$analysis.df[,layers[1]], na.rm = TRUE)
    layer2.max <- max(model$analysis.df[,layers[2]], na.rm = TRUE)

  } else {

    # Named list of minima and maxima
    if(!all(is.na(minmax))){
      layer1.min <- min(minmax[[layers[1]]])
      layer2.min <- min(minmax[[layers[2]]])
      layer1.max <- max(minmax[[layers[1]]])
      layer2.max <- max(minmax[[layers[2]]])
    }
  }





  # Build plot df
  plot.df <- cbind(rep(seq(layer1.min, layer1.max, length = nbins), nbins),
                   rep(seq(layer2.min, layer2.max, length = nbins), each = nbins))

  # Create a vector of names in the right order for plot.df
  names <- layers

  for(i in names(env)){
    if(!(i %in% layers)){
      layer.values <- unlist(terra::extract(env[[i]], points, ID = FALSE, na.rm = TRUE))
      plot.df <- cbind(plot.df, rep(mean(layer.values, na.rm=TRUE), nrow(plot.df)))
      names <- c(names, i)
    }
  }

  # Get data for plotting training points
  if(plot.points == TRUE){
    pointdata <- as.data.frame(terra::extract(env[[layers]], points, ID = FALSE))
  }

  # Grab test points
  if(plot.test.data == TRUE){
    if(!inherits(model$test.data, "SpatVector")){
      stop("Test data is not present, but plot.test.data was set to TRUE")
    }
    test.points <- as.data.frame(cbind(terra::extract(env[[layers]], model$test.data, ID = FALSE)))
  }

  colnames(plot.df) <- names

  plot.df <- data.frame(plot.df)

  if(inherits(model$model, "DistModel")){
    pred <- predict(model$model, x = plot.df, type = "prob")
  } else {
    if(inherits(model$model, "ranger")) {
      pred <- predict(model$model, data = plot.df, type = "prob")$predictions[ , 2, drop = TRUE]
    } else {
      pred <- predict(model$model, new_data = plot.df, type = "prob")
    }
  }

  plot.df <- cbind(plot.df[,1:2], pred)

  suit.plot <- ggplot(data = plot.df, aes(y = .data[[names[2]]], x = .data[[names[1]]])) +
    geom_raster(aes(fill = plot.df[,".pred_1"])) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    theme_classic() +
    ggtitle(label = "Predicted suitability in environment space") +
    theme(plot.title = element_text(hjust = 0.5))

  if(plot.points == TRUE){
    suit.plot <- suit.plot  + geom_point(data = pointdata, aes(y = .data[[names[2]]], x = .data[[names[1]]]),
                                         pch = 21, fill = "white", color = "black", size = 3)
  }

  if(plot.test.data == TRUE){
    suit.plot <- suit.plot + geom_point(data = test.points, aes(y = .data[[names[2]]], x = .data[[names[1]]]),
                                        pch = 24, fill = "green", color = "black", size = 3, alpha = 0.6)
  }

  # Do density plot of presence vs. background data
  if(inherits(model, c("enmtools.bc", "enmtools.dm"))){
    background.plot <- NA
  } else {
    bgpoints <- model$analysis.df[model$analysis.df$presence == 0,1:2]
    bgdata <- as.data.frame(terra::extract(env[[layers]], bgpoints, ID = FALSE, na.rm = TRUE))
    background.plot <- ggplot(bgdata, aes(y = .data[[names[2]]], x = .data[[names[1]]])) +
      stat_density_2d(aes(fill = after_stat(density)),
                      geom = "raster",
                      contour = FALSE,
                      na.rm = TRUE) +
      xlim(layer1.min, layer1.max) + ylim(layer2.min, layer2.max) +
      scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) + theme_classic() +
      ggtitle(label = "Presence points and background density in environment space") +
      theme(plot.title = element_text(hjust = 0.5))

    if(plot.points == TRUE){
      background.plot <- background.plot + geom_point(data = pointdata, aes(y = .data[[names[2]]], x = .data[[names[1]]]),
                                                      pch = 21, fill = "white", color = "black", size = 3)
    }
  }

  output <- list(background.plot = background.plot,
                 suit.plot = suit.plot)

  return(output)
}
