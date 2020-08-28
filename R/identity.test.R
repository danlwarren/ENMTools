#' identity.test Conduct a niche identity/equivalency test as described in Warren et al. 2008.
#'
#'
#' @param species.1 An emtools.species object
#' @param species.2 An enmtools.species object
#' @param env A RasterLayer or RasterStack object containing environmental data
#' @param type The type of model to construct, currently accepts "glm", "mx", "bc", "gam", "rf", or "dm"
#' @param f A function to use for model fitting.  Only required for GLM models at the moment.
#' @param nreps Number of replicates to perform
#' @param nback Number of background points for models
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param low.memory When set to TRUE, replicate models are written to disc instead of being stored in the output object.  Replicate models stored in the output object contain paths to the replicate models on disk instead of the rasters themselves.
#' @param rep.dir Directory for storing replicate models when low.memory is set to TRUE.  If not specified, the working directory will be used.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param clamp Controls whether empirical and replicate models should be clamped to the environment space used for training.
#' @param ... Additional arguments to be passed to model fitting functions.
#'
#' @return results A list containing the replicates, models for the empirical data, and summary statistics and plots.
#'
#' @keywords identity equivalency enmtools hypothesis-testing
#'
#' @examples
#' \donttest{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni <- iberolacerta.clade$species$cyreni
#' monticola <- iberolacerta.clade$species$monticola
#' cyreni$range <- background.raster.buffer(cyreni$presence.points, 100000, euro.worldclim)
#' monticola$range <- background.raster.buffer(monticola$presence.points, 100000, euro.worldclim)
#' identity.test(cyreni, monticola, env = euro.worldclim, type = "glm",
#' f = pres ~ bio1 + bio12, nreps = 10)
#' }

identity.test <- function(species.1, species.2, env, type, f = NULL, nreps = 99, nback = 1000, bg.source = "default", low.memory = FALSE, rep.dir = NA, verbose = FALSE, clamp = TRUE, ...){

  species.1 <- check.bg(species.1, env, nback = nback, bg.source = bg.source, verbose = verbose)
  species.2 <- check.bg(species.2, env, nback = nback, bg.source = bg.source, verbose = verbose)

  identity.precheck(species.1, species.2, env, type, f, nreps)

  # Initialize a list to store reps in
  replicate.models <- list()

  # Set the output directory when low.memory = TRUE
  if(low.memory == TRUE){
    if(is.na(rep.dir)){
      rep.dir <- getwd()
    }

    if(substr(rep.dir, nchar(rep.dir), nchar(rep.dir)) != "/"){
      rep.dir <- paste0(rep.dir, "/")
    }

    if(!dir.exists(rep.dir)){
      stop(paste("Specified directory for storing replicates cannot be found!\n\n", getwd()))
    }
  }

  # For starters we need to combine species background points so that each model
  # is being built with the same background
  species.1$background.points <- rbind(species.1$background.points, species.2$background.points)
  species.2$background.points <- rbind(species.1$background.points, species.2$background.points)

  combined.presence.points <- rbind(species.1$presence.points, species.2$presence.points)

  # Clamping layers here so it's not done separately for every replicate
  # and setting replicate clmaping to FALSE
  if(clamp == TRUE){
    # Adding env (skipped for BC otherwise)
    this.df <- as.data.frame(extract(env, combined.presence.points))

    env <- clamp.env(this.df, env)
  }

  # Build models for empirical data
  message("\nBuilding empirical models...\n")
  if(type == "glm"){
    empirical.species.1.model <- enmtools.glm(species.1, env, f, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.glm(species.2, env, f, clamp = FALSE, ...)
  }

  if(type == "gam"){
    empirical.species.1.model <- enmtools.gam(species.1, env, f, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.gam(species.2, env, f, clamp = FALSE, ...)
  }

  if(type == "mx"){
    empirical.species.1.model <- enmtools.maxent(species.1, env, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.maxent(species.2, env, clamp = FALSE, ...)
  }

  if(type == "bc"){
    empirical.species.1.model <- enmtools.bc(species.1, env, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.bc(species.2, env, clamp = FALSE, ...)
  }

  if(type == "dm"){
    empirical.species.1.model <- enmtools.dm(species.1, env, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.dm(species.2, env, clamp = FALSE, ...)
  }

  if(type == "rf"){
    empirical.species.1.model <- enmtools.rf(species.1, env, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.rf(species.2, env, clamp = FALSE, ...)
  }


  empirical.overlap <- c(unlist(raster.overlap(empirical.species.1.model, empirical.species.2.model)),
                         unlist(env.overlap(empirical.species.1.model, empirical.species.2.model, env = env)[1:3]))
  reps.overlap <- empirical.overlap

  message("\nBuilding replicate models...\n")


  if (requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = " [:bar] :percent eta: :eta",
      total = nreps, clear = FALSE, width= 60)
  }


  for(i in 1:nreps){
    if(verbose == TRUE){message(paste("\nReplicate", i, "...\n"))}

    if (requireNamespace("progress", quietly = TRUE)) {
      pb$tick()
    }


    combined.presence.points <- combined.presence.points[sample(nrow(combined.presence.points)),]
    rep.species.1 <- species.1
    rep.species.2 <- species.2
    rep.species.1$presence.points <- combined.presence.points[1:nrow(species.1$presence.points),]
    rep.species.2$presence.points <- combined.presence.points[(nrow(species.1$presence.points) + 1):nrow(combined.presence.points),]

    # Building models for reps
    if(type == "glm"){
      rep.species.1.model <- enmtools.glm(rep.species.1, env, f, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.glm(rep.species.2, env, f, clamp = FALSE, ...)
    }

    if(type == "gam"){
      rep.species.1.model <- enmtools.gam(rep.species.1, env, f, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.gam(rep.species.2, env, f, clamp = FALSE, ...)
    }

    if(type == "mx"){
      rep.species.1.model <- enmtools.maxent(rep.species.1, env, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.maxent(rep.species.2, env, clamp = FALSE, ...)
    }

    if(type == "bc"){
      rep.species.1.model <- enmtools.bc(rep.species.1, env, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.bc(rep.species.2, env, clamp = FALSE, ...)
    }

    if(type == "dm"){
      rep.species.1.model <- enmtools.dm(rep.species.1, env, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.dm(rep.species.2, env, clamp = FALSE, ...)
    }

    if(type == "rf"){
      rep.species.1.model <- enmtools.rf(rep.species.1, env, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.rf(rep.species.2, env, clamp = FALSE, ...)
    }

    reps.overlap <- rbind(reps.overlap, c(unlist(raster.overlap(rep.species.1.model, rep.species.2.model)),
                                          unlist(env.overlap(rep.species.1.model, rep.species.2.model, env = env)[1:3])))

    # Appending models to replicates list
    if(low.memory == TRUE){
      path.1 <- paste0(rep.dir, species.1$species.name, ".rep.", i, ".Rda")
      path.2 <- paste0(rep.dir, species.2$species.name, ".rep.", i, ".Rda")
      save(rep.species.1.model, file = path.1)
      save(rep.species.2.model, file = path.2)
      replicate.models[[paste0(species.1$species.name, ".rep.", i)]] <- path.1
      replicate.models[[paste0(species.2$species.name, ".rep.", i)]] <- path.2

    } else {
      replicate.models[[paste0(species.1$species.name, ".rep.", i)]] <- rep.species.1.model
      replicate.models[[paste0(species.2$species.name, ".rep.", i)]] <- rep.species.2.model
    }
  }


  rownames(reps.overlap) <- c("empirical", paste("rep", 1:nreps))

  p.values <- apply(reps.overlap, 2, function(x) rank(x)[1]/length(x))

  d.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Identity test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  i.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Identity test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  cor.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"rank.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"rank.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Identity test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  env.d.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Identity test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  env.i.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Identity test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  env.cor.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"env.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"env.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Identity test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  # mean(id.dm[,1] > id.dm[1,1])

  output <- list(description = paste("\n\nIdentity test", species.1$species.name, "vs.", species.2$species.name),
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

  class(output) <- "enmtools.identity.test"

  return(output)

}

identity.precheck <- function(species.1, species.2, env, type, f, nreps){

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


summary.enmtools.identity.test <- function(object, ...){

  cat(paste("\n\n", object$description))

  cat("\n\nIdentity test p-values:\n")
  print(object$p.values)

  cat("\n\nReplicates:\n")
  print(kable(head(object$reps.overlap)))

  plot(object)

}

print.enmtools.identity.test <- function(x, ...){

  summary(x)

}

plot.enmtools.identity.test <- function(x, ...){
  grid.arrange(x$d.plot, x$env.d.plot,
               x$i.plot, x$env.i.plot,
               x$cor.plot, x$env.cor.plot, ncol = 2)  +
    theme(plot.title = element_text(hjust = 0.5))
}

