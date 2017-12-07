#' marginal.plots Plots the marginal response of a model to an environmental variable with all other variables held at their mean in env
#'
#'
#' @param model An enmtools model object
#' @param env A RasterLayer or RasterStack object containing environmental data
#' @param layer The name of the layer to plot
#'
#' @return results A plot of the marginal response of the model to the environmental variable
#'
#' @keywords plot, sdm, enm, response
#'
#' @export marginal.plots
#'
#' @examples
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni.mx <- enmtools.maxent(iberolacerta.clade$species$cyreni, euro.worldclim)
#' marginal.plots(cyreni.mx, env, "bio3")

marginal.plots <- function(model, env, layer){

  if(!layer %in% names(env)){
    stop(paste("Couldn't find layer named", layer, "in environmental rasters!"))
  }

  if(inherits(model, c("enmtools.bc", "enmtools.dm"))){
    points <- model$analysis.df
  } else {
    points <- model$analysis.df[model$analysis.df$presence == 1,1:2]
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

  plot.df <- data.frame(plot.df)

  colnames(plot.df) <- names

  if(inherits(model$model, what = "DistModel")){
    pred <- predict(model$model, x = plot.df, type = "response")
  } else {
    pred <- predict(model$model, newdata = plot.df, type = "response")
  }


  #print(pred)

  response.plot <- qplot(plot.df[,1], pred, geom = "line",
                         xlab = layer, ylab = "Response") + theme_bw() +
                         theme(plot.title = element_text(hjust = 0.5)) +
                         theme(plot.title = element_text(hjust = 0.5))


  return(response.plot)

}
