#' Takes a raster stack and builds a variogram for each layer.
#'
#' @param env A raster or raster stack of environmental data.
#' @param maxpoints The maximum number of points to use for making your semivariograms. If set to NA uses all points, but this could be quite slow if you have a large number of rasters or high resolution.
#'
#' @return A list containing the variogram data in a data frame as well as a plot of the scaled gammas for each variable.
#'
#' @examples
#' multi.variogram(euro.worldclim, maxpoints = 500)

multi.variogram <- function(env, maxpoints = NA){

  check.packages(c("gstat"))

  env.df <- raster::rasterToPoints(env, spatial = TRUE)

  if(!is.na(maxpoints) & maxpoints < nrow(env.df)){
    env.df <- env.df[sample(1:nrow(env.df), size = maxpoints),]
  }

  multi.var <- gstat::variogram(env.df[[1]]~x+y, data = env.df)
  multi.var$scaled.gamma <- multi.var$gamma/max(multi.var$gamma)
  multi.var$var <- names(env)[1]

  if(length(names(env)) > 1){
    for(i in 2:length(names(env))){
      thisvar <- gstat::variogram(env.df[[i]]~1, data = env.df)
      thisvar$scaled.gamma <- thisvar$gamma/max(thisvar$gamma)
      thisvar$var <- names(env)[i]
      multi.var <- rbind(multi.var, thisvar)
    }
  }

  thisplot <- qplot(dist, scaled.gamma, data = multi.var, geom = "line", color = var) +
    theme_minimal() + ylab("Scaled Gamma") + xlab("Distance")

  output <- list(data = multi.var,
                 plot = thisplot)

  return(output)
}



