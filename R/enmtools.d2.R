#' enmtools.ecospat.id, Runs an ecospat identity test using enmtool.species objects.
#' @importFrom(ecospat,ecospat.grid.clim.dyn)
#' @param species.1 An enmtools.species object
#' @param species.2 An enmtools.species object
#' @param env A set of environmental layers
#' @param nreps The number of pseudoreplicates to perform #permutations#
#' @param layers A vector of length <1 containing the names of the layers to be used.  If no layer names are provided and there are more than two layers in env, enmtools will perform a pca and use the top two layers from that.
#' @param pca A logical determining if a pca is to be performed
#' @param pca.var Number of variables to result from pca
#' @param th.sp Quantile of species densities used as a threshold to exclude low species density values. See documentation for ecospat.grid.clim.dyn.
#' @param th.env Quantile of environmental densities across study sites used as threshold to exclude low environmental density values. See documentation for ecospat.grid.clim.dyn.
#' @param nback Number of background points to use for density calculations.
#' @param R Resolution of the grid. See documentation for ecospat.grid.clim.dyn.
#'
#' @return results d2.org: Mahalanobis distance between sp1 and sp2 climate envelope,
#' 95 percentile: based on nreps permuations of both climatic envelops, significant if 95 percentile < orginal distance,
#' Novelty: percentage of points outside first climate envelope,
#' MIV: most influential variable, variable increasing te d2.org most significantly
#'
#' @keywords niche similarity sdm enm mahalanobis
#'
#' @export enmtools.d2
#'
#' @examples
#' \dontrun{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' monticola <- iberolacerta.clade$species$monticola
#' cyreni <- iberolacerta.clade$species$cyreni
#' enmtools.mahalanobis.id(monticola, cyreni, euro.worldclim)
#' }

enmtools.mahalanobis.id <- function(species.1, species.2, env, nreps = 99, pca = FALSE, pca.var = 2){
  options(warn=-1)
  #species.1 <- check.bg(species.1, env, nback)
  #species.2 <- check.bg(species.2, env, nback)

  if (pca == F){
    layers <- names(env)
    print(paste("Using",length(layers), "variables. No PCA performed."))
  } else if (pca == T) {
    print("Performing PCA over the data set.")
    env <- raster.pca(env, n = pca.var)
    layers <- names(env)
  }

  ecospat.id.precheck(species.1, species.2, env, nreps, layers)
  #TODO: add option for occurence thinning based on env raster resolution

  #Grabbing environmental data for species 1 points
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

  #Grabbing environmental data for species 1 background points
  # sp1.bg.env <- extract(env, species.1$background.points)
  # sp1.bg.env <- cbind(rep(paste0(species.1$species.name, ".bg"), nrow(species.1$background.points)),
  #                     species.1$background.points,
  #                     sp1.bg.env)
  # sp1.bg.env <- sp1.bg.env[complete.cases(sp1.bg.env),]
  # colnames(sp1.bg.env) <- c("Species", colnames(species.1$background.points), layers)
  #
  # #Grabbing environmental data for species 2 background points
  # sp2.bg.env <- extract(env, species.2$background.points)
  # sp2.bg.env <- cbind(rep(species.2$species.name, nrow(species.2$background.points)),
  #                     species.2$background.points,
  #                     sp2.bg.env)
  # sp2.bg.env <- sp2.bg.env[complete.cases(sp2.bg.env),]
  # colnames(sp2.bg.env) <- c("Species", colnames(species.2$background.points), layers)

  #Extracting background env data at all points for env
  # background.env <- as.data.frame(rasterToPoints(env))
  # background.env <- cbind(rep("background", length(background.env[,1])), background.env)
  # colnames(background.env) <- c("Species", colnames(species.1$presence.points), names(env))
  # background.env <- background.env[complete.cases(background.env),]

  #sp1.niche <- ecospat.grid.clim.dyn(background.env[,4:5], sp1.bg.env[,4:5], sp1.env[,4:5], th.sp=th.sp, th.env=th.env, R=R)
  #sp2.niche <- ecospat.grid.clim.dyn(background.env[,4:5], sp2.bg.env[,4:5], sp2.env[,4:5], th.sp=th.sp, th.env=th.env, R=R)

  #eq <- ecospat.niche.equivalency.test(sp1.niche, sp2.niche, rep=nreps)
  eq <- d2.test(sp1.env = sp1.env, sp2.env = sp2.env, rep = nreps)

  # empline <- c(eq$obs$D, eq$obs$I)
  # names(empline) <- c("D", "I")
  # reps.overlap <- rbind(empline, eq$sim)
  # p.values <- apply(reps.overlap, 2, function(x) mean(x < x[1]))



  output <- list(description = paste("\n\nMahalanobis niche similarity test:", species.1$species.name, "vs.", species.2$species.name),
                 #sp1.env = head(sp1.env),
                 #sp2.env = head(sp2.env),
                 # sp1.bg.env = sp1.bg.env,
                 # sp2.bg.env = sp2.bg.env,
                 # background.env = background.env,
                 # sp1.niche = sp1.niche,
                 # sp2.niche = sp2.niche,
                 # sp1.bg.plot = sp1.bg.plot,
                 # sp1.env.plot = sp1.env.plot,
                 # sp1.env.plot.corr = sp1.env.plot.corr,
                 # sp2.bg.plot = sp2.bg.plot,
                 # sp2.env.plot = sp2.env.plot,
                 # sp2.env.plot.corr = sp2.env.plot.corr,
                 test.results = eq
                 #p.values = p.values,
                 #d.plot = d.plot,
                 #i.plot = i.plot
                 )
  class(output) <- "enmtools.d2.test"

  return(output)

}


ecospat.id.precheck <- function(species.1, species.2, env, nreps, layers){

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

  # if(!inherits(species.1$background.points, "data.frame")){
  #   stop("Species 1 background.points do not appear to be an object of class data.frame")
  # }

  check.species(species.2)

  if(!inherits(species.2$presence.points, "data.frame")){
    stop("Species 2 presence.points do not appear to be an object of class data.frame")
  }

  # if(!inherits(species.2$background.points, "data.frame")){
  #   stop("Species 2 background.points do not appear to be an object of class data.frame")
  # }
  #
  # if(any(!colnames(species.1$background.points) %in% colnames(species.2$background.points))){
  #   stop("Column names for species background points do not match!")
  # }

  if(any(!colnames(species.1$presence.points) %in% colnames(species.2$presence.points))){
    stop("Column names for species presence points do not match!")
  }

  if(is.na(species.1$species.name)){
    stop("Species 1 does not have a species.name set!")
  }

  if(is.na(species.2$species.name)){
    stop("Species 2 does not have a species.name set!")
  }

  if(is.null(layers)){
    stop("You must provide either a stack containing two layers, or a vector of two layer names to use for overlaps!")
  }

  if(length(layers) <= 2){
    print("You must specify more than 2 layers to use for overlaps!")
  }

}

# mahalanobis distance tests
d2.test <- function(sp1.env, sp2.env, rep) {
  sp1.env <- sp1.env[,4:ncol(sp1.env)]
  sp2.env <- sp2.env[,4:ncol(sp2.env)]

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



  ## permutation for significance test
  res <- rep(NA, rep)

  ### pooling both species
  sp.all.env <- rbind(sp1.env, sp2.env)
  seed <- 3554
  for (i in 1:rep)
  {
    ### Permutation: taking a random sample of the size of species1
    sp1.rand <-
      sp.all.env[sample(nrow(sp.all.env), nrow(sp1.env), replace = F), ]
    sp2.rand <-
      sp.all.env[!(rownames(sp.all.env) %in% rownames(sp1.rand)), ]

    ### cov matrix
    mat1.2 <- cov(sp1.rand)
    mat2.2 <- cov(sp2.rand)

    n1 <- nrow(sp1.rand)
    n2 <- nrow(sp2.rand)
    n3 <- n1 + n2

    ### pooled matrix
    mat3 <- ((n1 / n3) * mat1.2) + ((n2 / n3) * mat2.2)
    ### inverse pooled matrix
    mat4 <- solve(mat3, tol = 1e-40)
    ### mean diff
    mat5 <- as.matrix(colMeans(sp1.rand) - colMeans(sp2.rand))
    ### multiply
    mat6 <- t(mat5) %*% mat4
    ### multiply
    res[i] <- sqrt(mat6 %*% mat5)
  }

  ## significantluy differing climate space when d2.org > 95percentile of permutated distribution
  sig <- d2.org > quantile(res, 0.95, na.rm = T)


  ## novelty: percentage species2 occurences outside species1 envelope
  lag.mean <- colMeans(sp1.env)
  cov.lag <- cov(sp1.env)
  lag.diff <- sp1.env - lag.mean
  d2.sp1 <-
    mahalanobis(
      x = sp1.env,
      lag.mean,
      cov = cov.lag,
      inverted = F,
      tol = 1e-40
    )
  d2.sp2 <-
    mahalanobis(
      x = sp2.env,
      lag.mean,
      cov = cov.lag,
      inverted = F,
      tol = 1e-40
    )

  novel <- 100 * sum(d2.sp2 > max(d2.sp1)) / nrow(sp2.env)


  ## homogenety test: identifying the most infuential variable by removing individual vars
  d2.subset <- rep(NA, ncol(sp1.env))
  names(d2.subset)<- colnames(sp1.env)

  for (j in colnames(sp1.env)) {

    sp1.env.subset <- sp1.env[,!(names(sp1.env) %in% j)]
    sp2.env.subset <- sp2.env[,!(names(sp2.env) %in% j)]

    ### cov matrix
    sp1.mat.subset <- cov(sp1.env.subset)
    sp2.mat.subset <- cov(sp2.env.subset)

    n1 <- nrow(sp1.mat.subset)
    n2 <- nrow(sp2.mat.subset)

    n3 <- n1 + n2

    #pooled matrix
    mat3 <- ((n1 / n3) * sp1.mat.subset) + ((n2 / n3) * sp2.mat.subset)

    #inverse pooled matrix
    mat4 <- solve(mat3, tol = 1e-40)

    #mean diff
    mat5 <- as.matrix(colMeans(sp1.env.subset) - colMeans(sp2.env.subset))

    #multiply
    mat6 <- t(mat5) %*% mat4

    #multiply
    d2.subset[j] <- sqrt(mat6 %*% mat5)

  }

  #print(d2.subset)
  d2.subset <- as.data.frame(d2.subset)
  var <- rownames(d2.subset)[which.min(d2.subset[,1])]

  results <- rep(x = NA, 5)

  results <- c(d2.org, quantile(res, 0.95,na.rm=T), sig, novel, var)
  names(results)<-c("d2.org","95 percentile", "Significance", "Novelty", "MIV")
  return(results)
}
