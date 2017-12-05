#' background.test Conduct a background test (also called a similarity test), as described in Warren et al. 2008.
#' This test can either be run as an asymmetric test (species.1 vs background of species.2) or as a symmetric test
#' (background of species.1 vs background of species.2).  For GLM, Bioclim, and Domain models the replicates will be
#' constructed from the background points supplied for each species.  For Maxent, the replicates will be constructed
#' from the range rasters stored in the enmtools.species objects.
#'
#' @param species.1 An emtools.species object from which presence points (asymmetric) or background (symmetric) will be sampled.
#' @param species.2 An enmtools.species object from which background will be sampled.
#' @param env A RasterLayer or RasterStack object containing environmental data
#' @param type The type of model to construct, currently accepts "glm", "mx", "bc", "gam", or "dm"
#' @param f A function to use for model fitting.  Only required for GLM models at the moment.
#' @param nreps Number of replicates to perform
#' @param nback Number of background points for models
#' @param test.type Controls whether the background test will be "symmetric" or "asymmetric"
#' @param ... Additional arguments to be passed to model fitting functions.
#'
#' @return results A list containing replicates, models for the empirical data, and summary statistics and plots.
#'
#' @keywords background, equivalency, enmtools, hypothesis testing
#'
#' @export background.test
#'
#' @examples
#' background.test(ahli, allogus, env, type = "glm", nreps = 10, test.type = "asymmetric")
#'

background.test <- function(species.1, species.2, env, type, f = NULL, nreps = 99, test.type = "asymmetric", nback = 1000, ...){

  # Build a description of the analysis to use for summaries and plot titles
  if(test.type == "symmetric"){
    description <- paste("\n\nSymmetric background test", species.1$species.name, "background vs.", species.2$species.name, "background")
  } else {
    description <- paste("\n\nAsymmetric background test", species.1$species.name, "vs.", species.2$species.name, "background")
  }
  cat(paste("\n", description, "\n"))

  species.1 <- check.bg(species.1, env, nback = nback, ...)
  species.2 <- check.bg(species.2, env, nback = nback, ...)

  # Check to make sure everything's okay
  background.precheck(species.1, species.2, env, type, f, nreps, test.type)

  # Initialize a list to store reps in
  replicate.models <- list()

  # For starters we need to combine species background points so that each model
  # is being built with the same background
  species.1$background.points <- rbind(species.1$background.points, species.2$background.points)
  species.2$background.points <- rbind(species.1$background.points, species.2$background.points)


  # Building models for empirical data
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
                         unlist(env.overlap(empirical.species.1.model, empirical.species.2.model, env = env)))
  reps.overlap <- empirical.overlap

  cat("\nBuilding replicate models...\n")
  for(i in 1:nreps){
    cat(paste("\nReplicate", i, "...\n"))

    rep.species.1 <- species.1
    rep.species.2 <- species.2

    if(test.type == "symmetric"){

      #background sampling for species 1, only run when we're doing a symmetric test
      combined.points <- rbind(rep.species.1$presence.points, rep.species.1$background.points)
      sample.vector <- sample(nrow(combined.points))
      combined.points <- combined.points[sample.vector,]
      rep.species.1$presence.points <- combined.points[1:nrow(species.1$presence.points),]
      rep.species.1$background.points <- combined.points[(nrow(species.1$presence.points) + 1):nrow(combined.points),]

    }

    # Background sampling for species 2, run regardless of the type of test
    combined.points <- rbind(rep.species.2$presence.points, rep.species.2$background.points)
    sample.vector <- sample(nrow(combined.points))
    combined.points <- combined.points[sample.vector,]
    rep.species.2$presence.points <- combined.points[1:nrow(species.2$presence.points),]
    rep.species.2$background.points <- combined.points[(nrow(species.2$presence.points) + 1):nrow(combined.points),]

    # Building the models for reps
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

    # Appending overlap to results
    reps.overlap <- rbind(reps.overlap, c(unlist(raster.overlap(rep.species.1.model, rep.species.2.model)),
                                          unlist(env.overlap(rep.species.1.model, rep.species.2.model, env = env))))

  }

  rownames(reps.overlap) <- c("empirical", paste("rep", 1:nreps))

  p.values <- apply(reps.overlap, 2, function(x) 1 - mean(x > x[1]))


  d.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D") + ggtitle(description)

  i.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I") + ggtitle(description)

  cor.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"rank.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"rank.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") + ggtitle(description)

  env.d.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(description)

  env.i.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(description)

  env.cor.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(description)


  output <- list(description = description,
                 reps.overlap = reps.overlap,
                 p.values = p.values,
                 empirical.species.1.model = empirical.species.1.model,
                 empirical.species.2.model = empirical.species.2.model,
                 replicate.models = replicate.models,
                 d.plot = d.plot,
                 i.plot = i.plot,
                 cor.plot = cor.plot,
                 env.d.plot = env.d.plot,
                 env.i.plot = env.i.plot,
                 env.cor.plot = env.cor.plot)

  class(output) <- "enmtools.background.test"

  return(output)

}

background.precheck <- function(species.1, species.2, env, type, f, nreps, test.type){

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


summary.enmtools.background.test <- function(object, ...){

  cat(paste("\n\n", object$description))

  cat("\n\nbackground test p-values:\n")
  print(object$p.values)

  cat("\n\nReplicates:\n")
  print(kable(head(object$reps.overlap)))

  plot(object)

}

print.enmtools.background.test <- function(x, ...){

  summary(x)

}

plot.enmtools.background.test <- function(x, ...){
  grid.arrange(x$d.plot, x$env.d.plot,
               x$i.plot, x$env.i.plot,
               x$cor.plot, x$env.cor.plot, ncol = 2)
}
