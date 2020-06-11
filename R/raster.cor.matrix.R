#' Takes a raster stack and returns a data frame containing Pearson correlation coefficients
#' between the included rasters
#'
#' @param env A RasterStack object containing two or more rasters
#' @param method Type of correlation to measure.  Can do "pearson", "kendall", or "spearman"
#'
#' @return A data frame of correlation coefficients for a set of rasters.
#'
#' @examples
#' \donttest{
#' data(euro.worldclim)
#' raster.cor.matrix(euro.worldclim)
#' }

raster.cor.matrix <- function(env, method = "pearson"){

  n.layers <- length(names(env))

  output <- as.data.frame(matrix(nrow = n.layers, ncol = n.layers))
  colnames(output) <- names(env)
  rownames(output) <- names(env)

  for(i in 1:n.layers){
    for(j in i:n.layers){
      output[i,j] <- raster.cor(env[[i]], env[[j]], method = method)
      output[j,i] <- raster.cor(env[[i]], env[[j]], method = method)
    }
  }

  return(output)

}
