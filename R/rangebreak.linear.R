#' rangebreak.linear Conduct a linear rangebreak test as described in Glor and Warren 2011.
#'
#'
#' @param species.1 An emtools.species object
#' @param species.2 An enmtools.species object
#' @param env A RasterLayer or RasterStack object containing environmental data
#' @param type The type of model to construct, currently accepts "glm", "mx", "bc", "gam", or "dm"
#' @param f A function to use for model fitting.  Only required for GLM models at the moment.
#' @param nreps Number of replicates to perform
#' @param nback Number of background points for models
#' @param ... Additional arguments to be passed to model fitting functions.
#'
#' @return results A list containing a replicates, models for the empirical data, and summary statistics and plots.
#'
#' @keywords rangebreak, biogeography, barrier, enmtools, hypothesis testing
#'
#' @export rangebreak.linear
#'
#' @examples
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni <- iberolacerta.clade$species$cyreni
#' aranica <- iberolacerta.clade$species$aranica
#' rangebreak.linear(cyreni, aranica, env = euro.worldclim, type = "mx", nreps = 10)
#'

rangebreak.linear <- function(species.1, species.2, env, type, f = NULL, nreps = 99,  nback = 1000, ...){

  # Just for visualization
  #   plotraster <- env[[1]]
  #   plotraster[!is.na(plotraster)] <- 1

  species.1 <- check.bg(species.1, env, nback = nback)
  species.2 <- check.bg(species.2, env, nback = nback)

  rangebreak.linear.precheck(species.1, species.2, env, type, f, nreps)

  # Initialize a list to store reps in
  replicate.models <- list()

  # For starters we need to combine species background points so that each model
  # is being built with the same background
  species.1$background.points <- rbind(species.1$background.points, species.2$background.points)
  species.2$background.points <- rbind(species.1$background.points, species.2$background.points)

  combined.presence.points <- rbind(species.1$presence.points, species.2$presence.points)

  # Build models for empirical data
  cat("\nBuilding empirical models...\n")
  if(type == "glm"){
    empirical.species.1.model <- enmtools.glm(species.1, env, f, ...)
    empirical.species.2.model <- enmtools.glm(species.2, env, f, ...)
  }

  if(type == "gam"){
    empirical.species.1.model <- enmtools.gam(species.1, env, f, ...)
    empirical.species.2.model <- enmtools.gam(species.2, env, f, ...)
  }

  if(type == "mx"){
    empirical.species.1.model <- enmtools.maxent(species.1, env, ...)
    empirical.species.2.model <- enmtools.maxent(species.2, env, ...)
  }

  if(type == "bc"){
    empirical.species.1.model <- enmtools.bc(species.1, env, ...)
    empirical.species.2.model <- enmtools.bc(species.2, env, ...)
  }

  if(type == "dm"){
    empirical.species.1.model <- enmtools.dm(species.1, env, ...)
    empirical.species.2.model <- enmtools.dm(species.2, env, ...)
  }

  if(type == "rf"){
    empirical.species.1.model <- enmtools.rf(species.1, env, ...)
    empirical.species.2.model <- enmtools.rf(species.2, env, ...)
  }


  empirical.overlap <- c(unlist(raster.overlap(empirical.species.1.model, empirical.species.2.model)),
                          unlist(env.overlap(empirical.species.1.model, empirical.species.2.model, env = env)[1:3]))
  reps.overlap <- empirical.overlap

  lines.df <- data.frame(slope = rep(NA, nreps), intercept = rep(NA, nreps))

  cat("\nBuilding replicate models...\n")
  for(i in 1:nreps){
    cat(paste("\nReplicate", i, "...\n"))

    rep.species.1 <- species.1
    rep.species.2 <- species.2

    angle <- runif(1, min=0, max=pi)
    slope <- sin(angle)/cos(angle)

    part.points <- cbind(combined.presence.points, combined.presence.points[,2] - slope * combined.presence.points[,1])

    # Flip a coin to decide whether we're going from top to bottom or other way around
    if(rbinom(1,1,0.5) == 0){
      part.points <- part.points[order(part.points[,3]),]
    } else {
      part.points <- part.points[order(part.points[,3], decreasing = TRUE),]
    }

    # The intercept to split the two into the appropriate sizes should now be
    # the mean of the Nth and Nth + 1 values for column 3, where N is the sample size
    # of one of the species
    intercept <- mean(c(part.points[nrow(species.1$presence.points), 3],
                        part.points[nrow(species.2$presence.points), 3]))

    rep.species.1$presence.points <- part.points[1:nrow(species.1$presence.points), 1:2]
    rep.species.2$presence.points <- part.points[(nrow(species.1$presence.points) + 1):nrow(part.points), 1:2]


    #       plot(plotraster)
    #       abline(intercept, slope)

    lines.df[i,] <- c(slope, intercept)

    # Building models for reps
    if(type == "glm"){
      rep.species.1.model <- enmtools.glm(rep.species.1, env, f, ...)
      rep.species.2.model <- enmtools.glm(rep.species.2, env, f, ...)
    }

    if(type == "gam"){
      rep.species.1.model <- enmtools.gam(rep.species.1, env, f, ...)
      rep.species.2.model <- enmtools.gam(rep.species.2, env, f, ...)
    }

    if(type == "mx"){
      rep.species.1.model <- enmtools.maxent(rep.species.1, env, ...)
      rep.species.2.model <- enmtools.maxent(rep.species.2, env, ...)
    }

    if(type == "bc"){
      rep.species.1.model <- enmtools.bc(rep.species.1, env, ...)
      rep.species.2.model <- enmtools.bc(rep.species.2, env, ...)
    }

    if(type == "dm"){
      rep.species.1.model <- enmtools.dm(rep.species.1, env, ...)
      rep.species.2.model <- enmtools.dm(rep.species.2, env, ...)
    }

    if(type == "rf"){
      rep.species.1.model <- enmtools.rf(rep.species.1, env, ...)
      rep.species.2.model <- enmtools.rf(rep.species.2, env, ...)
    }

    # Appending models to replicates list
    replicate.models[[paste0(species.1$species.name, ".rep.", i)]] <- rep.species.1.model
    replicate.models[[paste0(species.2$species.name, ".rep.", i)]] <- rep.species.2.model

    reps.overlap <- rbind(reps.overlap, c(unlist(raster.overlap(rep.species.1.model, rep.species.2.model)),
                                          unlist(env.overlap(rep.species.1.model, rep.species.2.model, env = env)[1:3])))

  }


  rownames(reps.overlap) <- c("empirical", paste("rep", 1:nreps))

  p.values <- apply(reps.overlap, 2, function(x) 1 - mean(x > x[1]))

  d.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name))

  i.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name))

  cor.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"rank.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"rank.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name))

  env.d.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name))

  env.i.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name))

  env.cor.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name))

  output <- list(description = paste("\n\nLinear rangebreak test", species.1$species.name, "vs.", species.2$species.name),
                 reps.overlap = reps.overlap,
                 p.values = p.values,
                 empirical.species.1.model = empirical.species.1.model,
                 empirical.species.2.model = empirical.species.2.model,
                 replicate.models = replicate.models,
                 lines.df = lines.df,
                 d.plot = d.plot,
                 i.plot = i.plot,
                 cor.plot = cor.plot,
                 env.d.plot = env.d.plot,
                 env.i.plot = env.i.plot,
                 env.cor.plot = env.cor.plot)

  class(output) <- "enmtools.rangebreak.linear"

  return(output)

}

rangebreak.linear.precheck <- function(species.1, species.2, env, type, f, nreps){


  if(!inherits(species.1, "enmtools.species")){
    stop("Species.1 is not an enmtools.species object!")
  }

  if(!inherits(species.2, "enmtools.species")){
    stop("Species.2 is not an enmtools.species object!")
  }

  if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
    stop("Environmental layers are not a RasterLayer or RasterStack object!")
  }

  if(type == "glm"){
    if(!is.null(f)){
      if(!inherits(f, "formula")){
        stop("Type is set to GLM and f is not a formula object!")
      }
    }
  }

  if(type == "gam"){
    if(!is.null(f)){
      if(!inherits(f, "formula")){
        stop("Type is set to GAM and f is not a formula object!")
      }
    }
  }

  if(!type %in% c("glm", "mx", "bc", "dm", "gam", "rf")){
    stop(paste("Model type", type, "not understood! Select either bc, dm, mx, gam, rf, or glm."))
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

}


summary.enmtools.rangebreak.linear <- function(object, ...){

  cat(paste("\n\n", object$description))

  cat("\n\nrangebreak test p-values:\n")
  print(object$p.values)

  cat("\n\nReplicates:\n")
  print(kable(head(object$reps.overlap)))

  plot(object)

}

print.enmtools.rangebreak.linear <- function(x, ...){

  summary(x)

}

plot.enmtools.rangebreak.linear <- function(x, ...){

  x.raster <- x$empirical.species.1.model$suitability
  x.raster[!is.na(x.raster)] <- 1
  plot(x.raster)
  for(i in 1:nrow(x$lines.df)){
    abline(x$lines.df[i,2], x$lines.df[i,1])
  }

  grid.arrange(x$d.plot, x$env.d.plot,
               x$i.plot, x$env.i.plot,
               x$cor.plot, x$env.cor.plot, ncol = 2) +
    theme(plot.title = element_text(hjust = 0.5))
}

