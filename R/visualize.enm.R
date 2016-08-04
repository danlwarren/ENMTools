#' visualize.enm, Makes a heatmap of suitability of habitat in environment space according to a given model
#'
#' @param model An enmtools.model object
#' @param env A set of environmental layers
#' @param nbins The number of bins per layer to use for drawing environment space
#' @param layers A vector of layer names to use for drawing environment space
#' @param plot.points Logical determining whether presence points should be plotted on suitability plot
#'
#' @return suit.plot A two dimensional plot of an ENM
#'
#' @keywords niche plot sdm enm
#'
#' @export visualize.enm
#'
#' @examples
#' visualize.enm(1, .001, .001)

visualize.enm <- function(model, env, nbins = 100, layers, plot.points = TRUE){

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

  layer1.min <- min(getValues(env[[layers[1]]]), na.rm=TRUE)
  layer2.min <- min(getValues(env[[layers[2]]]), na.rm=TRUE)
  layer1.max <- max(getValues(env[[layers[1]]]), na.rm=TRUE)
  layer2.max <- max(getValues(env[[layers[2]]]), na.rm=TRUE)

  # Build plot df
  plot.df <- cbind(rep(seq(layer1.min, layer1.max, length = nbins), nbins),
                   rep(seq(layer2.min, layer2.max, length = nbins), each = nbins))

  # Create a vector of names in the right order for plot.df
  names <- layers

  for(i in names(env)){
    if(!(i %in% layers)){
      layer.values <- extract(env[[i]], points)
      plot.df <- cbind(plot.df, rep(mean(layer.values, na.rm=TRUE), nrow(plot.df)))
      names <- c(names, i)
    }
  }

  pointdata <- as.data.frame(extract(env[[layers]], points))

  colnames(plot.df) <- names

  plot.df <- data.frame(plot.df)

  pred <- predict(model$model, plot.df, type = "response")

  plot.df <- cbind(plot.df[,1:2], pred)

  suit.plot <- ggplot(data = plot.df, aes_string(y = names[2], x = names[1])) +
    geom_raster(aes(fill = pred)) +
    scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability")) +
    theme_classic() +
    ggtitle(label = "Predicted suitability in environment space")

  if(plot.points == TRUE){
    suit.plot <- suit.plot  + geom_point(data = pointdata, aes_string(y = names[2], x = names[1]),
                                         pch = 21, fill = "white", color = "black", size = 3)
  }

  # Do density plot of presence vs. background data
  if(inherits(model, c("enmtools.bc", "enmtools.dm"))){
    background.plot <- NA
  } else {
    bgpoints <- model$analysis.df[model$analysis.df$presence == 0,1:2]
    bgdata <- as.data.frame(extract(env[[layers]], bgpoints))
    background.plot <- ggplot(bgdata, aes_string(y = names[2], x = names[1])) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      xlim(layer1.min, layer1.max) + ylim(layer2.min, layer2.max) +
      scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) + theme_classic() +
      ggtitle(label = "Presence points and background density in environment space")

    if(plot.points == TRUE){
      background.plot <- background.plot + geom_point(data = pointdata, aes_string(y = names[2], x = names[1]),
                                                      pch = 21, fill = "white", color = "black", size = 3)
    }
  }

  output <- list(suit.plot = suit.plot,
                 background.plot = background.plot)

  return(output)
}
