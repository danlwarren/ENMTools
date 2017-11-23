#' utility functions for ENMTools
#'
#' @importFrom graphics abline par plot plot.new points title
#' @importFrom stats aggregate as.formula contrasts complete.cases cor delete.response glm is.empty.model lm model.frame model.matrix rbinom rnorm reformulate runif sd terms var
#' @importFrom utils combn head stack
#' @importFrom ppmlasso ppmlasso
#' @importFrom dismo circles randomPoints bioclim
#' @importFrom ape getMRCA
#' @importFrom raster getValues sampleRandom rasterToPoints extract
#' @importFrom ggplot2 ggplot ggtitle stat_density_2d theme_bw theme_classic xlab xlim ylim geom_abline geom_contour geom_point geom_raster geom_vline guide_colourbar qplot aes_string aes
#' @importFrom viridis scale_fill_viridis
#' @importFrom gridExtra grid.arrange
#' @importFrom rgeos gUnaryUnion
#' @importFrom ecospat ecospat.grid.clim.dyn ecospat.niche.equivalency.test ecospat.niche.similarity.test
#' @importFrom mgcv gam gam.check

predict.ppmlasso <- ppmlasso:::predict.ppmlasso
predict.gam <- dismo::predict
predict.glm <- dismo::predict
predict.randomForest <- dismo::predict
predict.randomForest.formula <- dismo::predict
predict.MaxEnt <- dismo::predict
predict.Bioclim <- dismo::predict
predict.Domain <- dismo::predict
predict.raster <- raster:::predict
predict.RasterBrick <- raster:::predict
predict.Raster <- raster:::predict
predict.RasterStackBrick <- raster:::predict
predict.BasicRaster <- raster:::predict
