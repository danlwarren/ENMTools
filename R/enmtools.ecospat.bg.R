#' enmtools.ecospat.bg, Runs an ecospat background/similarity test using enmtool.species objects.
#'
#' @param species.1 An enmtools.species object
#' @param species.2 An enmtools.species object
#' @param env A set of environmental layers
#' @param nreps The number of pseudoreplicates to perform
#' @param layers A vector of length 2 containing the names of the layers to be used.  If no layer names are provided and there are more than two layers in env, enmtools will perform a pca and use the top two layers from that.
#' @param test.type Symmetric or asymmetric test.  An asymmetric test is bguivalent to the "one.sided" option in the ecospat documentation, while a symmetric one would be two-sided.
#' @param th.sp Quantile of species densities used as a threshold to exclude low species density values.  See documentation for ecospat.grid.clim.dyn.
#' @param th.env Quantile of environmental densities across study sites used as threshold to exclude low
#' environmental density values.  See documentation for ecospat.grid.clim.dyn.
#' @param nback Number of background points to use for density calculations.
#' @param R Resolution of the grid. See documentation for ecospat.grid.clim.dyn.
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param ... Further arguments to be passed to check.bg
#'
#' @return A list containing the ecospat output kernel density estimates for each species and their background, as well as the results of hypothesis tests and their accompanying plots.
#'
#' @keywords niche plot sdm enm
#'
#' @examples
#' \donttest{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' monticola <- iberolacerta.clade$species$monticola
#' cyreni <- iberolacerta.clade$species$cyreni
#' enmtools.ecospat.bg(monticola, cyreni, euro.worldclim[[1:2]], nback = 500)
#' }

enmtools.ecospat.bg <- function(species.1, species.2, env, nreps = 99, layers = NULL, test.type = "asymmetric", th.sp=0, th.env=0, R=100, nback = 1000, bg.source = "default", verbose = FALSE, ...){

  check.packages("ecospat")

  species.1 <- check.bg(species.1, env, nback, verbose = verbose, ...)
  species.2 <- check.bg(species.2, env, nback, verbose = verbose, ...)

  # Use supplied layers if there's two of them, otherwise do PCA
  if(length(names(env)) == 2){
    layers <- names(env)
  } else if (is.null(layers)) {
    message("More than two layers in environment stack and no layers argument passed, performing PCA...")

    env <- raster.pca(env, n = 2)

    env <- env$rasters

    layers <- names(env)
  }

  ecospat.bg.precheck(species.1, species.2, env, nreps, layers)

  #Grabbing environmental data for species 1 points
  sp1.env <- extract(env, species.1$presence.points)
  sp1.env <- cbind(rep(species.1$species.name, nrow(species.1$presence.points)),
                   species.1$presence.points,
                   sp1.env)
  sp1.env <- sp1.env[complete.cases(sp1.env),]
  colnames(sp1.env) <- c("Species", colnames(species.1$presence.points), layers)

  #Grabbing environmental data for species 1 background points
  sp1.bg.env <- extract(env, species.1$background.points)
  sp1.bg.env <- cbind(rep(paste0(species.1$species.name, ".bg"), nrow(species.1$background.points)),
                      species.1$background.points,
                      sp1.bg.env)
  sp1.bg.env <- sp1.bg.env[complete.cases(sp1.bg.env),]
  colnames(sp1.bg.env) <- c("Species", colnames(species.1$background.points), layers)

  #Grabbing environmental data for species 2 background points
  sp2.bg.env <- extract(env, species.2$background.points)
  sp2.bg.env <- cbind(rep(species.2$species.name, nrow(species.2$background.points)),
                      species.2$background.points,
                      sp2.bg.env)
  sp2.bg.env <- sp2.bg.env[complete.cases(sp2.bg.env),]
  colnames(sp2.bg.env) <- c("Species", colnames(species.2$background.points), layers)


  #Grabbing environmental data for species 2 points
  sp2.env <- extract(env, species.2$presence.points)
  sp2.env <- cbind(rep(paste0(species.2$species.name, ".bg"), nrow(species.2$presence.points)),
                   species.2$presence.points,
                   sp2.env)
  sp2.env <- sp2.env[complete.cases(sp2.env),]
  colnames(sp2.env) <- c("Species", colnames(species.2$presence.points), layers)


  #Extracting background env data at all points for env
  background.env <- as.data.frame(rasterToPoints(env))
  background.env <- cbind(rep("background", length(background.env[,1])), background.env)
  colnames(background.env) <- c("Species", colnames(species.1$presence.points), names(env))
  background.env <- background.env[complete.cases(background.env),]

  sp1.niche <- ecospat::ecospat.grid.clim.dyn(background.env[,4:5], sp1.bg.env[,4:5], sp1.env[,4:5], th.sp=th.sp, th.env=th.env, R=R)
  sp2.niche <- ecospat::ecospat.grid.clim.dyn(background.env[,4:5], sp2.bg.env[,4:5], sp2.env[,4:5], th.sp=th.sp, th.env=th.env, R=R)

  if(test.type == "symmetric"){
    rand.type = 1
  } else {
    rand.type = 2
  }

  bg <- ecospat::ecospat.niche.similarity.test(sp1.niche, sp2.niche, rep=nreps, rand.type = rand.type, ...)

  empline <- c(bg$obs$D, bg$obs$I)
  names(empline) <- c("D", "I")
  reps.overlap <- rbind(empline, bg$sim)
  p.values <- apply(reps.overlap, 2, function(x) min(rank(x)[1], rank(-x)[1])/length(x))

  d.plot <- qplot(bg$sim[,"D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = bg$obs$D, linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Ecospat background test:", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  i.plot <- qplot(bg$sim[,"I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = bg$obs$I, linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Ecospat background test:", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))


  sp1.bg.points <- data.frame(rasterToPoints(sp1.niche$Z))
  colnames(sp1.bg.points) <- c("X", "Y", "Density")
  sp1.bg.plot <-  ggplot(data = sp1.bg.points, aes_string(y = "Y", x = "X")) +
    geom_raster(aes_string(fill = "Density")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) +
    theme_classic() +
    ggtitle(paste(species.1$species.name, "available environment")) +
    theme(plot.title = element_text(hjust = 0.5))

  sp1.env.points <- data.frame(rasterToPoints(sp1.niche$z.uncor))
  colnames(sp1.env.points) <- c("X", "Y", "Density")
  sp1.env.plot <-  ggplot(data = sp1.env.points, aes_string(y = "Y", x = "X")) +
    geom_raster(aes_string(fill = "Density")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) +
    theme_classic() +
    ggtitle(paste(species.1$species.name, "occurrence in environment space")) +
    theme(plot.title = element_text(hjust = 0.5))

  sp1.env.corr.points <- data.frame(rasterToPoints(sp1.niche$z.cor))
  colnames(sp1.env.corr.points) <- c("X", "Y", "Density")
  sp1.env.plot.corr <-  ggplot(data = sp1.env.corr.points, aes_string(y = "Y", x = "X")) +
    geom_raster(aes_string(fill = "Density")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) +
    theme_classic() +
    ggtitle(paste(species.1$species.name, "occurrence scaled by availability")) +
    theme(plot.title = element_text(hjust = 0.5))

  sp2.bg.points <- data.frame(rasterToPoints(sp2.niche$Z))
  colnames(sp2.bg.points) <- c("X", "Y", "Density")
  sp2.bg.plot <-  ggplot(data = sp2.bg.points, aes_string(y = "Y", x = "X")) +
    geom_raster(aes_string(fill = "Density")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) +
    theme_classic() +
    ggtitle(paste(species.2$species.name, "available environment")) +
    theme(plot.title = element_text(hjust = 0.5))

  sp2.env.points <- data.frame(rasterToPoints(sp2.niche$z.uncor))
  colnames(sp2.env.points) <- c("X", "Y", "Density")
  sp2.env.plot <-  ggplot(data = sp2.env.points, aes_string(y = "Y", x = "X")) +
    geom_raster(aes_string(fill = "Density")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) +
    theme_classic() +
    ggtitle(paste(species.2$species.name, "occurrence in environment space")) +
    theme(plot.title = element_text(hjust = 0.5))

  sp2.env.corr.points <- data.frame(rasterToPoints(sp2.niche$z.cor))
  colnames(sp2.env.corr.points) <- c("X", "Y", "Density")
  sp2.env.plot.corr <-  ggplot(data = sp2.env.corr.points, aes_string(y = "Y", x = "X")) +
    geom_raster(aes_string(fill = "Density")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) +
    theme_classic() +
    ggtitle(paste(species.2$species.name, "occurrence scaled by availability")) +
    theme(plot.title = element_text(hjust = 0.5))



#   image(log(chlor$Z), main="Chlorocyanus environment", col=rainbow(10))
#   #points(chlorpoints[,4:5], pch=3)
#   image(log(chlor$z.uncor), main="Chlorocyanus density", col=rainbow(10))
#   #points(chlorpoints[,4:5], pch=3)
#   image(log(chlor$z.cor), main="Chlorocyanus occupancy", col=rainbow(10))
#   points(chlorpoints[,4:5], pch=3)

  output <- list(description = paste("\n\nEcospat background test", test.type, species.1$species.name, "vs.", species.2$species.name),
                 sp1.env = sp1.env,
                 sp2.env = sp2.env,
                 sp1.bg.env = sp1.bg.env,
                 sp2.bg.env = sp2.bg.env,
                 background.env = background.env,
                 sp1.niche = sp1.niche,
                 sp2.niche = sp2.niche,
                 sp1.bg.plot = sp1.bg.plot,
                 sp1.env.plot = sp1.env.plot,
                 sp1.env.plot.corr = sp1.env.plot.corr,
                 sp2.bg.plot = sp2.bg.plot,
                 sp2.env.plot = sp2.env.plot,
                 sp2.env.plot.corr = sp2.env.plot.corr,
                 test.results = bg,
                 p.values = p.values,
                 d.plot = d.plot,
                 i.plot = i.plot)
  class(output) <- "ecospat.bg.test"

  return(output)

}


ecospat.bg.precheck <- function(species.1, species.2, env, nreps, layers){

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

  if(!inherits(species.1$background.points, "data.frame")){
    stop("Species 1 background.points do not appear to be an object of class data.frame")
  }

  check.species(species.2)

  if(!inherits(species.2$presence.points, "data.frame")){
    stop("Species 2 presence.points do not appear to be an object of class data.frame")
  }

  if(!inherits(species.2$background.points, "data.frame")){
    stop("Species 2 background.points do not appear to be an object of class data.frame")
  }

  if(any(!colnames(species.1$background.points) %in% colnames(species.2$background.points))){
    stop("Column names for species background points do not match!")
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

  if(length(layers) != 2){
    stop("You must specify which two layers to use for overlaps!")
  }

}


summary.ecospat.bg.test <- function(object, ...){
  cat(paste("\n\n", object$description))

  # print(kable(head(object$sp1.env)))
  # print(kable(head(object$sp1.object.env)))
  # print(kable(head(object$sp2.env)))
  # print(kable(head(object$sp2.object.env)))
  # print(kable(head(object$background.env)))

  cat("\n\necospat.bg test empirical overlaps:\n")
  print(object$test.results$obs)

  cat("\n\necospat.bg test p-values:\n")
  print(object$p.values)

  plot(object)

}

print.ecospat.bg.test <- function(x, ...){

  print(summary(x))

}

plot.ecospat.bg.test <- function(x, ...){
  grid.arrange(x$d.plot, x$i.plot, nrow = 2)
  grid.arrange(x$sp1.bg.plot, x$sp2.bg.plot,
               x$sp1.env.plot, x$sp2.env.plot,
               x$sp1.env.plot.corr, x$sp2.env.plot.corr, ncol = 2) +
               theme(plot.title = element_text(hjust = 0.5))
}

