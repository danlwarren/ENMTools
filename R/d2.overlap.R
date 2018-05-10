#' Takes two emtools.species objects with rasters, calculates Mahlanobis distance between them
#'
#' @param sp1.env An enmtools.species object containing presence points
#' @param sp2.env An enmtools.species object containing presence points
#' @param env A set of environmental layers
#' @param layers A vector of length <1 containing the names of the layers to be used.  If no layer names are provided and there are more than two layers in env, enmtools will perform a pca and use the top two layers from that.
#'
#' @return results A list containing a replicates, models for the empirical data, and summary statistics and plots.
#'
#' @export d2.overlap
#'
#' @keywords identity, Mahalanobis, enmtools, hypothesis testing
#'
#' @examples
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' env <- euro.worldclim
#' cyreni <- iberolacerta.clade$species$cyreni
#' monticola <- iberolacerta.clade$species$monticola
#' d2.overlap(cyreni, monticola)

d2.overlap <- function(species.1, species.2, env, layers){

  # if objects passed are data frames
  if(inherits(species.1,"data.frame") & inherits(species.2,"data.frame")){
    species.1 <-
      species.1[, (tolower(names(species.1)) %in% c("species", names(env)))]
    species.2 <-
      species.2[, (tolower(names(species.2)) %in% c("species", names(env)))]

    if(length(colnames(species.1))<3 |
       length(colnames(species.2))<3)
      stop(
        "First three columnames should be Species, Longitude, Latitude follwed by at least 2 bioclim vars"
      )

    #print(paste("data frames with",ncol(species.2)," variables found. Running d2 test."))

    # do d2 test
    sp1.env <- species.1[,2:ncol(species.1)]
    sp2.env <- species.2[,2:ncol(species.2)]

    ## mahalanobis distance orginal
    sp1.mat <- cov(sp1.env)
    sp2.mat <- cov(sp2.env)
    n1 <- nrow(sp1.env)
    n2 <- nrow(sp2.env)
    n3 <- n1 + n2
    ### pooled matrix
    mat3 <- ((n1 / n3) * sp1.mat) + ((n2 / n3) * sp2.mat)
    ### inverse pooled matrix
    mat4 <- solve(mat3, tol = 1e-40)
    ### mean diff
    mat5 <- as.matrix(colMeans(sp1.env) - colMeans(sp2.env))
    ### multiply
    mat6 <- t(mat5) %*% mat4
    ### multiply
    d2.org <- sqrt(mat6 %*% mat5)
    return(d2.org)
  }

  # check if enmtool object
  if(!inherits(species.1, "enmtools.species")){
    stop("Species.1 is not an enmtools.species object!")
  }

  if(!inherits(species.2, "enmtools.species")){
    stop("Species.2 is not an enmtools.species object!")
  }

  if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
    stop("Environmental layers are not a RasterLayer or RasterStack object!")
  }

  check.species(species.1)

  if(!inherits(species.1$presence.points, "data.frame")){
    stop("Species 1 presence.points do not appear to be an object of class data.frame")
  }

  check.species(species.2)

  if(!inherits(species.2$presence.points, "data.frame")){
    stop("Species 2 presence.points do not appear to be an object of class data.frame")
  }

  if(any(!colnames(species.1$presence.points) %in% colnames(species.2$presence.points))){
    stop("Column names for species presence points do not match!")
  }

  if(is.na(species.1$species.name)){
    stop("Species 1 does not have a species.name set!")
  }

  if(is.na(species.2$species.name)){
    stop("Species 2 does not have a species.name set!")
  }

  # Extraction of clim vars
  layers<-names(env)
  # Grabbing environmental data for species 1 points
  print("Two valid enm.tool species objects found along with environmental data. Extracting matrics.")
  sp1.env <- extract(env, species.1$presence.points)
  sp1.env <- cbind(rep(species.1$species.name, nrow(species.1$presence.points)),
                   species.1$presence.points,
                   sp1.env)
  sp1.env <- sp1.env[complete.cases(sp1.env),]
  colnames(sp1.env) <- c("Species", colnames(species.1$presence.points), layers)

  #Grabbing environmental data for species 2 points
  sp2.env <- extract(env, species.2$presence.points)
  sp2.env <- cbind(rep(paste0(species.2$species.name, ".bg"), nrow(species.2$presence.points)),
                   species.2$presence.points,
                   sp2.env)
  sp2.env <- sp2.env[complete.cases(sp2.env),]
  colnames(sp2.env) <- c("Species", colnames(species.2$presence.points), layers)


  # do d2 test
  # sp1.env <- sp1.env[,4:ncol(sp1.env)]
  # sp2.env <- sp2.env[,4:ncol(sp2.env)]

  ## mahalanobis distance orginal
  sp1.mat <- cov(sp1.env)
  sp2.mat <- cov(sp2.env)
  n1 <- nrow(sp1.env)
  n2 <- nrow(sp2.env)
  n3 <- n1 + n2
  ### pooled matrix
  mat3 <- ((n1 / n3) * sp1.mat) + ((n2 / n3) * sp2.mat)
  ### inverse pooled matrix
  mat4 <- solve(mat3, tol = 1e-40)
  ### mean diff
  mat5 <- as.matrix(colMeans(sp1.env) - colMeans(sp2.env))
  ### multiply
  mat6 <- t(mat5) %*% mat4
  ### multiply
  d2.org <- sqrt(mat6 %*% mat5)
  return(d2.org)
}

