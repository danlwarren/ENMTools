#' enmtools.d2, Runs an Mahalanobis identity test using enmtool.species objects.
#' @param species.1 An enmtools.species object
#' @param species.2 An enmtools.species object
#' @param env A set of environmental layers
#' @param nreps The number of pseudoreplicates to perform #permutations#
#' @param layers A vector of length <1 containing the names of the layers to be used.  If no layer names are provided and there are more than two layers in env, enmtools will perform a pca and use the top two layers from that.
#' @param pca A logical determining if a pca is to be performed
#' @param pca.var Number of variables to result from pca
#'
#' @return results d2.org: Mahalanobis distance between centroids of sp1 and sp2 climate envelopes,
#' 95 percentile: based on nreps permuations of both climate envelops, significant if 95 percentile < orginal distance,
#' Novelty: percentage of points outside first climate envelope,
#' MIV: most influential variable, variable increasing d2.org most significantly and summary statistics and plots.
#'
#' @keywords niche similarity sdm enmtools mahalanobis
#'
#' @export enmtools.mahalanobis.id
#'
#' @examples
#' \dontrun{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' env <- euro.worldclim
#' monticola <- iberolacerta.clade$species$monticola
#' cyreni <- iberolacerta.clade$species$cyreni
#' enmtools.mahalanobis.id(monticola, cyreni, euro.worldclim)
#' }

#TODO: rarefy/thin species records
#TODO: explain metric

enmtools.mahalanobis.id <- function(species.1, species.2, env, nreps = 99, pca = FALSE, pca.var = 2){
  options(warn=-1)

  if (pca == F){
    layers <- names(env)
    print(paste("Using",length(layers), "variables. No PCA performed."))
  } else if (pca == T) {
    print("Performing PCA over the data set.")
    env <- raster.pca(env, n = pca.var)
    layers <- names(env)
  }

  # precheck function
  mahalanobis.id.precheck(species.1, species.2, env, nreps, layers)
  #TODO: add option for occurence thinning based on env raster resolution

  # Extraction of clim vars
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

  sp1.env <- sp1.env[,4:ncol(sp1.env)]
  sp2.env <- sp2.env[,4:ncol(sp2.env)]

  sp1 <- species.1$species.name
  sp2 <- species.2$species.name


  # run the test
  ## d2.org
  d2.org <- d2.overlap(sp1.env, sp2.env, env, layers)

  ## permutation
  d2.perm <- rep(NA, nreps)
  sp.all.env <- rbind(sp1.env, sp2.env)

  for (i in 1:nreps)
  {
    ### Permutation: taking a random sample of the size of species1
    sp1.rand <-
      sp.all.env[sample(nrow(sp.all.env), nrow(sp1.env), replace = F),]
    sp2.rand <-
      sp.all.env[!(rownames(sp.all.env) %in% rownames(sp1.rand)),]
    d2.perm[i] <- d2.overlap(sp1.rand, sp2.rand, env, layers)
  }

  ## significantluy differing climate space when d2.org > 95percentile of permutated distribution
  sig <- d2.org > quantile(d2.perm, 0.95, na.rm = T)

  ## novelty: percentage species2 occurences outside species1 envelope
  sp1.mean <- colMeans(sp1.env)
  sp1.cov <- cov(sp1.env)
  #sp1.diff <- sp1.env - sp1.mean
  d2.sp1 <-
    mahalanobis(
      x = sp1.env,
      sp1.mean,
      cov = sp1.cov,
      inverted = F,
      tol = 1e-40
    )
  d2.sp2 <-
    mahalanobis(
      x = sp2.env,
      sp1.mean,
      cov = sp1.cov,
      inverted = F,
      tol = 1e-40
    )

  novelty <- 100 * sum(d2.sp2 > max(d2.sp1)) / nrow(sp2.env)

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

  d2.subset <- as.data.frame(d2.subset)
  miv <- rownames(d2.subset)[which.min(d2.subset[,1])]


  # Permutated distribution plot
  d2.perm.plot <-
    qplot(d2.perm, geom = "histogram", bins = nreps) +
    geom_vline(xintercept=d2.org, linetype = "longdash", color = "red") +
    annotate("text", label = paste("d2.org\n",round(d2.org,2)), x =round(d2.org,2), y=max(hist(d2.perm,breaks=nreps)$counts), color = "red") +
    geom_vline(xintercept=quantile(d2.perm, 0.95, na.rm=T), linetype = "longdash", color = "blue") +
    annotate("text", label = paste("d2.perm\n95percentile\n",round(quantile(d2.perm, 0.95, na.rm=T),2)), x =round(quantile(d2.perm, 0.95, na.rm=T),2), y=max(hist(d2.perm,breaks=nreps)$counts), color = "blue") +
    ggtitle(
      paste(
        "Permutated distribution")
    ) +
    xlab(label = paste("Mahalanois distance"))


  # Novelty plot
  d2.novelty.plot <-
    qplot(d2.sp2, geom = "histogram", bins = length(d2.sp2)) +
    geom_vline(xintercept = max(d2.sp1),color = "red", linetype = "longdash") +
    annotate("text", label = paste("max. distance\n of sp1 records to sp1.mean\n",round(max(d2.sp1),2)), x =max(d2.sp1),y=max(hist(d2.sp2,breaks=length(d2.sp2))$counts), color = "red") +
    ggtitle(
      paste(
        "Novelty test")
    ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab(label = paste("Distance between sp2 records and centroid of sp1"))




  # output
  output <- list(description = paste("\n\nMahalanobis identity test ", sp1, "vs.", sp2),
               reps = nreps,
               d2.org = as.numeric(d2.org),
               d2.perm.summary = summary(d2.perm),
               d2.perm95 = as.numeric(quantile(d2.perm, 0.95, na.rm=T)),
               significant = as.logical(sig),
               novelty = as.numeric(novelty),
               d2.perm.plot = d2.perm.plot,
               d2.novelty.plot = d2.novelty.plot
               )
class(output)<-"Mahalanobis identity test"

return(output)
}

mahalanobis.id.precheck <- function(species.1, species.2, env, nreps, layers){

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

  if(is.null(layers)){
    stop("You must provide either a stack containing two layers, or a vector of two layer names to use for overlaps!")
  }

  if(length(layers) <= 2){
    print("You must specify more than 2 layers to use for overlaps!")
  }
  if(is.null(nreps)){
    warning("Number of replications missing. Continuing with 9 reps.")
  }

}



