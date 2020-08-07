#' Takes a raster stack and returns a data frame containing Pearson correlation coefficients
#' between the included rasters
#'
#' @param env A RasterStack object containing two or more rasters
#' @param method Type of correlation to measure.  Can do "pearson", "kendall", or "spearman"
#'
#' @return A list of two plots.  The first maps the correlations between rasters into an MDS space, so that predictors that fall close together in that space are more correlated.  The second plot is a heatmap depicting the correlations between pairs of layers.
#'
#' @examples
#' data(euro.worldclim)
#' raster.cor.plot(euro.worldclim)

raster.cor.plot <- function(env, method = "pearson"){

  check.packages("reshape2")

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

  # Plot variables in MDS space of correlation
  d <- as.dist(1 - cor.mat)
  mds.cor <- as.data.frame(cmdscale(d))
  colnames(mds.cor) <- c("X", "Y")

  cor.mds.plot <- ggplot(mds.cor, aes_string(x = "X", y = "Y"))  +
    geom_text(aes(label = rownames(mds.cor))) + theme_bw()

  # Make heatmap
  melted.cor.mat <- reshape2::melt(as.matrix(cor.mat))
  colnames(melted.cor.mat) <- c("Var1", "Var2", "value")

  cor.heatmap <- ggplot(data = melted.cor.mat, aes_string(x="Var1", y="Var2", fill="value")) +
    geom_tile() + scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  output <- list(cor.mds.plot = cor.mds.plot,
                 cor.heatmap = cor.heatmap)

  return(output)

}
