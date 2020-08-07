#' marginal.plots Plots the marginal response of a model to an environmental variable with all other variables held at their mean in env
#'
#'
#' @param model An enmtools model object
#' @param env A RasterLayer or RasterStack object containing environmental data
#' @param layer The name of the layer to plot
#' @param standardize Whether to set the maximum of the response function to 1, or to instead use the raw values.
#'
#' @return results A plot of the marginal response of the model to the environmental variable.
#'
#' @keywords plot sdm enm response
#'
#' @examples
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni.glm <- enmtools.glm(iberolacerta.clade$species$cyreni,
#' f = pres ~ bio1 + bio12, euro.worldclim)
#' marginal.plots(cyreni.glm, euro.worldclim, "bio1")

marginal.plots <- function(model, env, layer, standardize = TRUE){

  if(!layer %in% names(env)){
    stop(paste("Couldn't find layer named", layer, "in environmental rasters!"))
  }

  if(inherits(model, c("enmtools.bc", "enmtools.dm"))){
    points <- model$analysis.df
  } else {
    points <- model$analysis.df[model$analysis.df$presence == 1,1:2]
  }

  if(any(is.na(minValue(env))) | any(is.na(maxValue(env)))){
    env <- setMinMax(env)
    message("\n\nSetting min and max for environment layers...\n\n")
  }

  # Create a vector of names in the right order for plot.df
  names <- layer

  plot.df <- seq(minValue(env[[layer]]), maxValue(env[[layer]]), length = 100)

  for(i in names(env)){
    if(i != layer){
      layer.values <- extract(env[[i]], points)
      plot.df <- cbind(plot.df, rep(mean(layer.values), 100))
      names <- c(names, i)
    }
  }

  if(standardize == TRUE){
    plot.df <- data.frame(plot.df)
  }


  colnames(plot.df) <- names


  # Hacked together to handle different ways different models keep
  # their presence data.
  # Also grabbing background directly from analysis df for models that
  # have that info, but sampling random bg for those that don't
  if(inherits(model, c("enmtools.bc", "enmtools.dm"))){
    pres.env <- extract(env[[layer]], model$analysis.df)
    pres.dens <- density(pres.env, from = minValue(env[[layer]]), to = maxValue(env[[layer]]), n = 100, na.rm = TRUE)$y
    pres.dens <- pres.dens/max(pres.dens)
    bg.env <- extract(env[[layer]], randomPoints(mask = env[[layer]], n = 1000))
    bg.dens <- density(bg.env, from = minValue(env[[layer]]), to = maxValue(env[[layer]]), n = 100, na.rm = TRUE)$y
    bg.dens <- bg.dens/max(bg.dens)
  } else {
    pres.env <- extract(env[[layer]], model$analysis.df[model$analysis.df$presence == 1,c(1,2)])
    pres.dens <- density(pres.env, from = minValue(env[[layer]]), to = maxValue(env[[layer]]), n = 100, na.rm = TRUE)$y
    pres.dens <- pres.dens/max(pres.dens)
    bg.env <- extract(env[[layer]], model$analysis.df[model$analysis.df$presence == 0,c(1,2)])
    bg.dens <- density(bg.env, from = minValue(env[[layer]]), to = maxValue(env[[layer]]), n = 100, na.rm = TRUE)$y
    bg.dens <- bg.dens/max(bg.dens)
  }




  if(inherits(model$model, what = "DistModel")){
    pred <- predict(model$model, x = plot.df, type = "response")
  } else {
    if(inherits(model$model, "ranger")) {
      pred <- predict(model$model, data = plot.df, type = "response")$predictions[ , 2, drop = TRUE]
    } else {
      pred <- predict(model$model, newdata = plot.df, type = "response")
    }
  }

  pred <- pred/max(pred)

  plot.df.long <- data.frame(layer = c(plot.df[,layer], plot.df[,layer], plot.df[,layer]),
                             value = c(pred, pres.dens, bg.dens),
                             source = c(rep("Suitability", 100),
                                        rep("Presence", 100),
                                        rep("Background", 100)))

  response.plot <- qplot(layer, value, geom = "line", data = plot.df.long,
                         xlab = layer, ylab = "Value", colour = source, linetype = source) +
                          theme_bw() + scale_color_manual(values = c("green4", "red", "blue")) +
                          scale_linetype_manual(values = c( "dashed", "twodash", "solid")) +
                          theme(plot.title = element_text(hjust = 0.5)) +
                          theme(plot.title = element_text(hjust = 0.5)) +
                          theme(legend.title=element_blank())


  return(response.plot)

}
