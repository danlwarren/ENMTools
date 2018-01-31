#' Takes a raster stack and returns a data frame containing Pearson correlation coefficients
#' between the included rasters
#'
#' @param env A RasterStack object containing two or more rasters
#' @param method Type of correlation to measure.  Can do "pearson", "kendall", or "spearman"
#'
#' @export raster.cor.matrix
#'
#' @examples
#' data(euro.worldclim)
#' raster.cor.plot(euro.worldclim)

raster.cor.plot <- function(env, method = "pearson"){

  n.layers <- length(names(env))

  cor.mat <- as.data.frame(matrix(nrow = n.layers, ncol = n.layers))
  colnames(cor.mat) <- names(env)
  rownames(cor.mat) <- names(env)

  for(i in 1:n.layers){
    for(j in i:n.layers){
      cor.mat[i,j] <- abs(raster.cor(env[[i]], env[[j]], method = method))
      cor.mat[j,i] <- cor.mat[i,j]
    }
  }
  colnames(cor.mat) <- names(env)
  row.names(cor.mat) <- names(env)
  melted.cor.mat <- reshape2::melt(as.matrix(cor.mat))

  colnames(melted.cor.mat) <- c("Var1", "Var2", "value")
  output <- ggplot(data = melted.cor.mat, aes_string(x="Var1", y="Var2", fill="value")) +
    geom_tile() + scale_fill_viridis() + theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle("Absolute value of correlation\nbetween predictor variables")

  return(output)

}
