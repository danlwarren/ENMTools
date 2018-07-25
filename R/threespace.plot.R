#' threespace.plot, A plot that compares the environmental distribution of presence points, background points, and the set of supplied environmental layers.
#'
#' @param model An enmtools.model object
#' @param env A set of environment layers
#'
#' @return A ggplot2 object that plots the distribution of environments in the climate layers to the distribution of environments at background and presence localities.
#'
#' @keywords pca environment presence background comparison extrapolation
#'
#' @export threespace.plot
#'
#' @examples
#' my.model<- enmtools.gam(iberolacerta.clade$species$monticola, euro.worldclim)
#' threespace.plot(my.model, euro.worldclim)

threespace.plot <- function(model, env){

  # Check classes of inputs
  if(!inherits(model, "enmtools.model")){
    stop("The supplied model is not an enmtools.model object!")
  }
  if(!inherits(env, c("raster", "RasterBrick", "RasterStack"))){
    stop("The supplied environmental layers were not recognized as raster objects!")
  }

  # Chuck out X and Y and stop on any unmatched env variables
  model.df <- model$analysis.df[,-c(1,2)]

  if(any(is.na(match(colnames(model.df), c(names(env), "presence"))))){
    stop("Mismatch in column names between model analysis.df and env layer names")
  }

  if(any(is.na(match(c(names(env), "presence"), colnames(model.df))))){
    stop("Mismatch in column names between model analysis.df and env layer names")
  }


  # Extract data from env layers
  allpoints <- as.data.frame(rasterToPoints(env))
  allpoints$presence <- rep(2, nrow(allpoints))
  allpoints <- allpoints[,-c(1,2)]

  # Combine model and env data frames
  allpoints <- rbind(allpoints, model.df)

  # Run PCA
  allpoints.pca <- princomp(allpoints[,1:ncol(allpoints) - 1,], cor = T)

  # Stitch everything together
  allpoints.pca <- data.frame(PC1 = allpoints.pca$scores[,1],
                              PC2 = allpoints.pca$scores[,2],
                              source = allpoints$presence)

  # Plot layers in order
  output <- ggplot(allpoints.pca, aes_string("PC1", "PC2")) +
    layer(geom = "point", stat = "identity", position = "identity",
          data = allpoints.pca[allpoints.pca$source == 2,], aes(color = "Env Layers")) +
    layer(geom = "point", stat = "identity", position = "identity",
          data = allpoints.pca[allpoints.pca$source == 0,], aes(color = "Background")) +
    layer(geom = "point", stat = "identity", position = "identity",
          data = allpoints.pca[allpoints.pca$source == 1,], aes(color = "Presence"))

  return(output)
}
