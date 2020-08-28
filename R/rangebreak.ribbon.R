#' rangebreak.ribbon Conduct a ribbon rangebreak test as described in Glor and Warren 2011.
#'
#'
#' @param species.1 An emtools.species object
#' @param species.2 An enmtools.species object
#' @param ribbon An enmtools.species object representing the region of marginal habtiat in the overlap between the species' ranges
#' @param env A RasterLayer or RasterStack object containing environmental data
#' @param type The type of model to construct, currently accepts "glm", "mx", "bc", or "dm"
#' @param f A function to use for model fitting.  Only required for GLM models at the moment.
#' @param width Width of the ribbon, in the same units as the occurrence points (e.g, decimal degrees)
#' @param nreps Number of replicates to perform
#' @param nback Number of background points for models
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param low.memory When set to TRUE, replicate models are written to disc instead of being stored in the output object.  Replicate models stored in the output object contain paths to the replicate models on disk instead of the rasters themselves.
#' @param rep.dir Directory for storing replicate models when low.memory is set to TRUE.  If not specified, the working directory will be used.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param clamp Controls whether empirical and replicate models should be clamped to the environment space used for training.
#' @param ... Additional arguments to be passed to model fitting functions.
#'
#' @return results A list containing models for the replicates, models for the empirical data, and summary statistics and plots.
#'
#' @keywords rangebreak biogeography barrier enmtools hypothesis-testing
#'
#' @examples
#' \donttest{
#' data(iberolacerta.clade)
#' data(euro.worldclim)
#' cyreni <- iberolacerta.clade$species$cyreni
#' aranica <- iberolacerta.clade$species$aranica
#'
#' # We're just going to fudge together occurrence data from a ribbon here
#' # from random points between the two species' ranges
#' p <- data.frame(Longitude = runif(50, -4, -2), Latitude = runif(50, 40, 43))
#' bg <- background.points.buffer(p, 100000, 100, euro.worldclim[[1]])
#' ribbon <- enmtools.species(species.name = "ribbon", presence.points = p, background.points = bg)
#'
#' rangebreak.ribbon(cyreni, aranica, ribbon = ribbon, env = euro.worldclim,
#' type = "glm", f= pres ~ bio1 + bio12, nreps = 10)
#' }

rangebreak.ribbon <- function(species.1, species.2, ribbon, env, type, f = NULL, width = 1, nreps = 99,  nback = 1000, bg.source = "default", low.memory = FALSE, rep.dir = NA, verbose = FALSE, clamp = TRUE, ...){

  species.1 <- check.bg(species.1, env, nback = nback, bg.source = bg.source, verbose = verbose)
  species.2 <- check.bg(species.2, env, nback = nback, bg.source = bg.source, verbose = verbose)
  ribbon <- check.bg(ribbon, env, nback = nback, bg.source = bg.source, verbose = verbose)

  # Making sure species 1 always has the most presence points
  if(nrow(species.1$presence.points) < nrow(species.2$presence.points)){
    temp.species <- species.1
    species.1 <- species.2
    species.2 <- temp.species
  }

  rangebreak.ribbon.precheck(species.1, species.2, ribbon, env, type, f, width, nreps)

  outside <- species.1
  outside$species.name <- "outside"
  outside$presence.points <- rbind(species.1$presence.points, species.2$presence.points)
  outside$background.points <- rbind(species.1$background.points, species.2$background.points, ribbon$background.points)

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
  combined.background <- rbind(species.1$background.points, species.2$background.points, ribbon$background.points)
  species.1$background.points <- combined.background
  species.2$background.points <- combined.background
  ribbon$background.points <- combined.background

  combined.presence.points <- rbind(species.1$presence.points, species.2$presence.points, ribbon$presence.points)

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
    empirical.ribbon.model <- enmtools.glm(ribbon, env, f, clamp = FALSE, ...)
    empirical.outside.model <- enmtools.glm(outside, env, f, clamp = FALSE, ...)
  }

  if(type == "gam"){
    empirical.species.1.model <- enmtools.gam(species.1, env, f, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.gam(species.2, env, f, clamp = FALSE, ...)
    empirical.ribbon.model <- enmtools.gam(ribbon, env, f, clamp = FALSE, ...)
    empirical.outside.model <- enmtools.gam(outside, env, f, clamp = FALSE, ...)
  }

  if(type == "mx"){
    empirical.species.1.model <- enmtools.maxent(species.1, env, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.maxent(species.2, env, clamp = FALSE, ...)
    empirical.ribbon.model <- enmtools.maxent(ribbon, env, clamp = FALSE, ...)
    empirical.outside.model <- enmtools.maxent(outside, env, clamp = FALSE, ...)
  }

  if(type == "bc"){
    empirical.species.1.model <- enmtools.bc(species.1, env, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.bc(species.2, env, clamp = FALSE, ...)
    empirical.ribbon.model <- enmtools.bc(ribbon, env, clamp = FALSE, ...)
    empirical.outside.model <- enmtools.bc(outside, env, clamp = FALSE, ...)
  }

  if(type == "dm"){
    empirical.species.1.model <- enmtools.dm(species.1, env, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.dm(species.2, env, clamp = FALSE, ...)
    empirical.ribbon.model <- enmtools.dm(ribbon, env, clamp = FALSE, ...)
    empirical.outside.model <- enmtools.dm(outside, env, clamp = FALSE, ...)
  }

  if(type == "rf"){
    empirical.species.1.model <- enmtools.rf(species.1, env, clamp = FALSE, ...)
    empirical.species.2.model <- enmtools.rf(species.2, env, clamp = FALSE, ...)
    empirical.ribbon.model <- enmtools.rf(ribbon, env, clamp = FALSE, ...)
    empirical.outside.model <- enmtools.rf(outside, env, clamp = FALSE, ...)
  }

  empirical.overlap.sp1.vs.sp2 <- c(unlist(raster.overlap(empirical.species.1.model, empirical.species.2.model)),
                                    unlist(env.overlap(empirical.species.1.model, empirical.species.2.model, env = env)[1:3]))
  reps.overlap.sp1.vs.sp2 <- empirical.overlap.sp1.vs.sp2

  empirical.overlap.sp1.vs.ribbon <- c(unlist(raster.overlap(empirical.species.1.model, empirical.ribbon.model)),
                                       unlist(env.overlap(empirical.species.1.model, empirical.ribbon.model, env = env)[1:3]))
  reps.overlap.sp1.vs.ribbon <- empirical.overlap.sp1.vs.ribbon

  empirical.overlap.sp2.vs.ribbon <- c(unlist(raster.overlap(empirical.species.2.model, empirical.ribbon.model)),
                                       unlist(env.overlap(empirical.species.2.model, empirical.ribbon.model, env = env)[1:3]))
  reps.overlap.sp2.vs.ribbon <- empirical.overlap.sp2.vs.ribbon

  empirical.overlap.outside.vs.ribbon <- c(unlist(raster.overlap(empirical.outside.model, empirical.ribbon.model)),
                                           unlist(env.overlap(empirical.outside.model, empirical.ribbon.model, env = env)[1:3]))
  reps.overlap.outside.vs.ribbon <- empirical.overlap.outside.vs.ribbon

  lines.df <- data.frame(slope = rep(NA, nreps), intercept = rep(NA, nreps), offset = rep(NA, nreps))

  message("\nBuilding replicate models...\n")

  # We'll use this to keep track of how many iterations were successful
  keepers <- 1

  if (requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = " [:bar] :percent eta: :eta",
      total = nreps, clear = FALSE, width= 60)
  }


  while(keepers <= nreps){
    if(verbose == TRUE){message(paste("\nReplicate", keepers, "...\n"))}

    if(requireNamespace("progress", quietly = TRUE)) {
      pb$tick()
    }

    rep.species.1 <- species.1
    rep.species.2 <- species.2
    rep.ribbon <- ribbon
    rep.outside <- outside

    angle <- runif(1, min=0, max=pi)
    slope <- sin(angle)/cos(angle)

    intercept.modifier <- (width/2)/cos(angle)

    if(slope < 0){
      intercept.modifier <- -(intercept.modifier)
    }

    # How far is each point from the line if intercept is set to zero
    part.points <- cbind(combined.presence.points, combined.presence.points[,2] - slope * combined.presence.points[,1])

    # Flip a coin to decide whether we're going from top to bottom or other way around
    if(rbinom(1,1,0.5) == 0){
      part.points <- part.points[order(part.points[,3]),]
    } else {
      part.points <- part.points[order(part.points[,3], decreasing = TRUE),]
    }

    # The intercept to split the two into the appropriate sizes should now be
    # the mean of the Nth and Nth + 1 values for column 3, where N is the sample size
    # of one of the species plus half the sample size of the ribbon
    N <- nrow(species.1$presence.points) + floor(nrow(ribbon$presence.points)/2)
    intercept <- mean(c(part.points[N, 3], part.points[N + 1, 3]))

    # Grab ribbon points, pull them out of part.points
    ribbon.points <- which(part.points[,3] > (intercept - intercept.modifier) & part.points[,3] < (intercept + intercept.modifier))
    rep.ribbon$presence.points <- part.points[ribbon.points,1:2]

    # Putting all remaining points in rep.outside
    rep.outside$presence.points <- part.points[-ribbon.points,1:2]

    # Splitting remaining points in proportion to the relative sample sizes of the empirical data
    rep.species.1$presence.points <- part.points[1:nrow(species.1$presence.points), 1:2]
    rep.species.2$presence.points <- part.points[(nrow(part.points) - nrow(species.2$presence.points)):nrow(part.points), 1:2]


    # Make sure we actually got some ribbon points.  If not, fail this round and try again.
    if(!nrow(rep.ribbon$presence.points) > 1){
      next
    }

    # Store the slope, intercept, and modifier for this round
    lines.df[keepers,] <- c(slope, intercept, intercept.modifier)

    # temp.list <- list(rep.species.1, rep.species.2, rep.ribbon, rep.outside)
    # return(temp.list)

    if(type == "glm"){
      rep.species.1.model <- enmtools.glm(rep.species.1, env, f, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.glm(rep.species.2, env, f, clamp = FALSE, ...)
      rep.ribbon.model <- enmtools.glm(rep.ribbon, env, f, clamp = FALSE, ...)
      rep.outside.model <- enmtools.glm(rep.outside, env, f, clamp = FALSE, ...)
    }

    if(type == "gam"){
      rep.species.1.model <- enmtools.gam(rep.species.1, env, f, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.gam(rep.species.2, env, f, clamp = FALSE, ...)
      rep.ribbon.model <- enmtools.gam(rep.ribbon, env, f, clamp = FALSE, ...)
      rep.outside.model <- enmtools.gam(rep.outside, env, f, clamp = FALSE, ...)
    }

    if(type == "mx"){
      rep.species.1.model <- enmtools.maxent(rep.species.1, env, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.maxent(rep.species.2, env, clamp = FALSE, ...)
      rep.ribbon.model <- enmtools.maxent(rep.ribbon, env, clamp = FALSE, ...)
      rep.outside.model <- enmtools.maxent(rep.outside, env, clamp = FALSE, ...)
    }

    if(type == "bc"){
      rep.species.1.model <- enmtools.bc(rep.species.1, env, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.bc(rep.species.2, env, clamp = FALSE, ...)
      rep.ribbon.model <- enmtools.bc(rep.ribbon, env, clamp = FALSE, ...)
      rep.outside.model <- enmtools.bc(rep.outside, env, clamp = FALSE, ...)
    }

    if(type == "dm"){
      rep.species.1.model <- enmtools.dm(rep.species.1, env, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.dm(rep.species.2, env, clamp = FALSE, ...)
      rep.ribbon.model <- enmtools.dm(rep.ribbon, env, clamp = FALSE, ...)
      rep.outside.model <- enmtools.dm(rep.outside, env, clamp = FALSE, ...)
    }

    if(type == "rf"){
      rep.species.1.model <- enmtools.rf(rep.species.1, env, clamp = FALSE, ...)
      rep.species.2.model <- enmtools.rf(rep.species.2, env, clamp = FALSE, ...)
      rep.ribbon.model <- enmtools.rf(rep.ribbon, env, clamp = FALSE, ...)
      rep.outside.model <- enmtools.rf(rep.outside, env, clamp = FALSE, ...)
    }



    # Appending models to replicates list
    if(low.memory == TRUE){
      path.1 <- paste0(rep.dir, species.1$species.name, ".rep.", keepers, ".Rda")
      path.2 <- paste0(rep.dir, species.2$species.name, ".rep.", keepers, ".Rda")
      path.ribbon <- paste0(rep.dir, "ribbon.rep.", keepers, ".Rda")
      path.outside <- paste0(rep.dir, "outside.rep.", keepers, ".Rda")
      save(rep.species.1.model, file = path.1)
      save(rep.species.2.model, file = path.2)
      save(rep.ribbon.model, file = path.ribbon)
      save(rep.outside.model, file = path.outside)
      replicate.models[[paste0(species.1$species.name, ".rep.", keepers)]] <- path.1
      replicate.models[[paste0(species.2$species.name, ".rep.", keepers)]] <- path.2
      replicate.models[[paste0("ribbon.rep.", keepers)]] <- path.ribbon
      replicate.models[[paste0("outside.rep.", keepers)]] <- path.outside

    } else {
      replicate.models[[paste0(species.1$species.name, ".rep.", keepers)]] <- rep.species.1.model
      replicate.models[[paste0(species.2$species.name, ".rep.", keepers)]] <- rep.species.2.model
      replicate.models[[paste0("ribbon.rep.", keepers)]] <- rep.ribbon.model
      replicate.models[[paste0("outside.rep.", keepers)]] <- rep.outside.model
    }


    # Measure overlaps
    this.overlap.sp1.vs.sp2  <- c(unlist(raster.overlap(rep.species.1.model, rep.species.2.model)),
                                  unlist(env.overlap(rep.species.1.model, rep.species.2.model, env = env)[1:3]))
    this.overlap.sp1.vs.ribbon <- c(unlist(raster.overlap(rep.species.1.model, rep.ribbon.model)),
                                    unlist(env.overlap(rep.species.1.model, rep.ribbon.model, env = env)[1:3]))
    this.overlap.sp2.vs.ribbon <- c(unlist(raster.overlap(rep.species.2.model, rep.ribbon.model)),
                                    unlist(env.overlap(rep.species.2.model, rep.ribbon.model, env = env)[1:3]))
    this.overlap.outside.vs.ribbon <- c(unlist(raster.overlap(rep.outside.model, rep.ribbon.model)),
                                        unlist(env.overlap(rep.outside.model, rep.ribbon.model, env = env)[1:3]))

#     print(this.overlap.sp1.vs.sp2)
#     print(this.overlap.sp1.vs.ribbon)
#     print(this.overlap.sp2.vs.ribbon)
#     print(this.overlap.outside.vs.ribbon)

    # If we can't get a good overlap value for this rep, chuck it and try again.
    if(any(is.na(this.overlap.sp1.vs.sp2)) | any(is.na(this.overlap.sp1.vs.ribbon)) |any(is.na(this.overlap.sp2.vs.ribbon)) |any(is.na(this.overlap.outside.vs.ribbon))){
      next
    }

    # No NAs, everything went right with this iteration
    keepers <- keepers + 1

    reps.overlap.sp1.vs.sp2 <- rbind(reps.overlap.sp1.vs.sp2, this.overlap.sp1.vs.sp2)
    reps.overlap.sp1.vs.ribbon <- rbind(reps.overlap.sp1.vs.ribbon, this.overlap.sp1.vs.ribbon)
    reps.overlap.sp2.vs.ribbon <- rbind(reps.overlap.sp2.vs.ribbon, this.overlap.sp2.vs.ribbon)
    reps.overlap.outside.vs.ribbon <- rbind(reps.overlap.outside.vs.ribbon, this.overlap.outside.vs.ribbon)


  }


  rownames(reps.overlap.sp1.vs.sp2) <- c("empirical", paste("rep", 1:nreps))
  rownames(reps.overlap.sp1.vs.ribbon) <- c("empirical", paste("rep", 1:nreps))
  rownames(reps.overlap.sp2.vs.ribbon) <- c("empirical", paste("rep", 1:nreps))
  rownames(reps.overlap.outside.vs.ribbon) <- c("empirical", paste("rep", 1:nreps))

  p.values.sp1.vs.sp2 <- apply(reps.overlap.sp1.vs.sp2, 2, function(x) min(rank(x)[1], rank(-x)[1])/length(x))
  p.values.sp1.vs.ribbon <- apply(reps.overlap.sp1.vs.ribbon, 2, function(x) min(rank(x)[1], rank(-x)[1])/length(x))
  p.values.sp2.vs.ribbon <- apply(reps.overlap.sp2.vs.ribbon, 2, function(x) min(rank(x)[1], rank(-x)[1])/length(x))
  p.values.outside.vs.ribbon <- apply(reps.overlap.outside.vs.ribbon, 2, function(x) min(rank(x)[1], rank(-x)[1])/length(x))


  ### Plots for sp1 vs sp2
  d.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  i.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  cor.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"rank.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"rank.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  env.d.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"env.D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"env.D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  env.i.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"env.I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"env.I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))

  env.cor.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"env.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"env.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs.", species.2$species.name)) +
    theme(plot.title = element_text(hjust = 0.5))


  ### Plots for sp1 vs ribbon
  d.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  i.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  cor.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"rank.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"rank.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.d.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"env.D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"env.D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.i.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"env.I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"env.I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.cor.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"env.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"env.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.1$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))



  ### Plots for sp2 vs ribbon
  d.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test:\n", species.2$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  i.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test:\n", species.2$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  cor.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"rank.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"rank.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test:\n", species.2$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.d.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"env.D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"env.D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.2$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.i.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"env.I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"env.I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.2$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.cor.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"env.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"env.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n", species.2$species.name, "vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))


  ### Plots for outside vs ribbon
  d.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test:\n outside vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  i.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test:\n outside vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  cor.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"rank.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"rank.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test:\n outside vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.d.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"env.D"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"env.D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n outside vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.i.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"env.I"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"env.I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n outside vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))

  env.cor.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"env.cor"], geom = "histogram", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"env.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test:\n outside vs. ribbon")) +
    theme(plot.title = element_text(hjust = 0.5))


  output <- list(description = paste("\n\nribbon rangebreak test", species.1$species.name, "vs.", species.2$species.name),
                 reps.overlap.sp1.vs.sp2 = reps.overlap.sp1.vs.sp2,
                 reps.overlap.sp1.vs.ribbon = reps.overlap.sp1.vs.ribbon,
                 reps.overlap.sp2.vs.ribbon = reps.overlap.sp2.vs.ribbon,
                 reps.overlap.outside.vs.ribbon = reps.overlap.outside.vs.ribbon,
                 p.values.sp1.vs.sp2 = p.values.sp1.vs.sp2,
                 p.values.sp1.vs.ribbon = p.values.sp1.vs.ribbon,
                 p.values.sp2.vs.ribbon = p.values.sp2.vs.ribbon,
                 p.values.outside.vs.ribbon = p.values.outside.vs.ribbon,
                 empirical.species.1.model = empirical.species.1.model,
                 empirical.species.2.model = empirical.species.2.model,
                 empirical.ribbon.model = empirical.ribbon.model,
                 empirical.outside.model = empirical.outside.model,
                 replicate.models = replicate.models,
                 lines.df = lines.df,
                 d.plot.sp1.vs.sp2 = d.plot.sp1.vs.sp2,
                 i.plot.sp1.vs.sp2 = i.plot.sp1.vs.sp2,
                 cor.plot.sp1.vs.sp2 = cor.plot.sp1.vs.sp2,
                 env.d.plot.sp1.vs.sp2 = env.d.plot.sp1.vs.sp2,
                 env.i.plot.sp1.vs.sp2 = env.i.plot.sp1.vs.sp2,
                 env.cor.plot.sp1.vs.sp2 = env.cor.plot.sp1.vs.sp2,
                 d.plot.sp1.vs.ribbon = d.plot.sp1.vs.ribbon,
                 i.plot.sp1.vs.ribbon = i.plot.sp1.vs.ribbon,
                 cor.plot.sp1.vs.ribbon = cor.plot.sp1.vs.ribbon,
                 env.d.plot.sp1.vs.ribbon = env.d.plot.sp1.vs.ribbon,
                 env.i.plot.sp1.vs.ribbon = env.i.plot.sp1.vs.ribbon,
                 env.cor.plot.sp1.vs.ribbon = env.cor.plot.sp1.vs.ribbon,
                 d.plot.sp2.vs.ribbon = d.plot.sp2.vs.ribbon,
                 i.plot.sp2.vs.ribbon = i.plot.sp2.vs.ribbon,
                 cor.plot.sp2.vs.ribbon = cor.plot.sp2.vs.ribbon,
                 env.d.plot.sp2.vs.ribbon = env.d.plot.sp2.vs.ribbon,
                 env.i.plot.sp2.vs.ribbon = env.i.plot.sp2.vs.ribbon,
                 env.cor.plot.sp2.vs.ribbon = env.cor.plot.sp2.vs.ribbon,
                 d.plot.outside.vs.ribbon = d.plot.outside.vs.ribbon,
                 i.plot.outside.vs.ribbon = i.plot.outside.vs.ribbon,
                 cor.plot.outside.vs.ribbon = cor.plot.outside.vs.ribbon,
                 env.d.plot.outside.vs.ribbon = env.d.plot.outside.vs.ribbon,
                 env.i.plot.outside.vs.ribbon = env.i.plot.outside.vs.ribbon,
                 env.cor.plot.outside.vs.ribbon = env.cor.plot.outside.vs.ribbon)

  class(output) <- "enmtools.rangebreak.ribbon"

  return(output)

}

rangebreak.ribbon.precheck <- function(species.1, species.2, ribbon, env, type, f, width, nreps){

  if(!inherits(species.1, "enmtools.species")){
    stop("Species.1 is not an enmtools.species object!")
  }

  if(!inherits(species.2, "enmtools.species")){
    stop("Species.2 is not an enmtools.species object!")
  }

  if(!inherits(ribbon, "enmtools.species")){
    stop("Ribbon is not an enmtools.species object!")
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

  check.species(ribbon)

  if(!inherits(ribbon$presence.points, "data.frame")){
    stop("Species 1 presence.points do not appear to be an object of class data.frame")
  }

  if(!inherits(ribbon$background.points, "data.frame")){
    stop("Species 1 background.points do not appear to be an object of class data.frame")
  }


  if(any(!colnames(species.1$background.points) %in% colnames(species.2$background.points))){
    stop("Column names for species background points do not match!")
  }

  if(any(!colnames(species.1$presence.points) %in% colnames(species.2$presence.points))){
    stop("Column names for species presence points do not match!")
  }

  if(any(!colnames(ribbon$presence.points) %in% colnames(species.2$presence.points))){
    stop("Column names for species presence points do not match!")
  }

  if(any(!colnames(ribbon$background.points) %in% colnames(species.2$background.points))){
    stop("Column names for species background points do not match!")
  }

  if(is.na(species.1$species.name)){
    stop("Species 1 does not have a species.name set!")
  }

  if(is.na(species.2$species.name)){
    stop("Species 2 does not have a species.name set!")
  }

  if(is.na(ribbon$species.name)){
    stop("Ribbon does not have a species.name set!  I suggest naming it 'ribbon' to avoid confusion.")
  }

  if(!inherits(width, "numeric")){
    stop("Barrier width is not numeric!")
  }

  if(!inherits(nreps, "numeric")){
    stop("Argument nreps is not numeric!")
  }
}


summary.enmtools.rangebreak.ribbon <- function(object, ...){

  cat(paste("\n\n", object$description))

  cat("\n\nrangebreak test p-values...\n")
  cat("\nSpecies 1 vs. Species 2:\n")
  print(object$p.values.sp1.vs.sp2)
  cat("\nSpecies 1 vs. Ribbon:\n")
  print(object$p.values.sp1.vs.ribbon)
  cat("\nSpecies 2 vs. Ribbon:\n")
  print(object$p.values.sp2.vs.ribbon)
  cat("\nOutside vs. Ribbon:\n")
  print(object$p.values.outside.vs.ribbon)

  cat("\n\nReplicates:\n")
  cat("\nSpecies 1 vs. Species 2:\n")
  print(object$reps.overlap.sp1.vs.sp2)
  cat("\nSpecies 1 vs. Ribbon:\n")
  print(object$reps.overlap.sp1.vs.ribbon)
  cat("\nSpecies 2 vs. Ribbon:\n")
  print(object$reps.overlap.sp2.vs.ribbon)
  cat("\nOutside vs. Ribbon:\n")
  print(object$reps.overlap.outside.vs.ribbon)

  plot(object)

}

print.enmtools.rangebreak.ribbon <- function(x, ...){

  summary(x)

}

plot.enmtools.rangebreak.ribbon <- function(x, ...){

  #   x.raster <- x$empirical.species.1.model$suitability
  #   x.raster[!is.na(x.raster)] <- 1
  #   plot(x.raster)
  #   for(i in 1:nrow(x$lines.df)){
  #     abline(x$lines.df[i,2], x$lines.df[i,1])
  #   }

  grid.arrange(x$d.plot.sp1.vs.sp2, x$env.d.plot.sp1.vs.sp2,
               x$i.plot.sp1.vs.sp2, x$env.i.plot.sp1.vs.sp2,
               x$cor.plot.sp1.vs.sp2, x$env.cor.plot.sp1.vs.sp2, ncol = 2) +
               theme(plot.title = element_text(hjust = 0.5))

  grid.arrange(x$d.plot.sp1.vs.ribbon, x$env.d.plot.sp1.vs.ribbon,
               x$i.plot.sp1.vs.ribbon, x$env.i.plot.sp1.vs.ribbon,
               x$cor.plot.sp1.vs.ribbon, x$env.cor.plot.sp1.vs.ribbon, ncol = 2) +
               theme(plot.title = element_text(hjust = 0.5))

  grid.arrange(x$d.plot.sp2.vs.ribbon, x$env.d.plot.sp2.vs.ribbon,
               x$i.plot.sp2.vs.ribbon, x$env.i.plot.sp2.vs.ribbon,
               x$cor.plot.sp2.vs.ribbon, x$env.cor.plot.sp2.vs.ribbon, ncol = 2) +
               theme(plot.title = element_text(hjust = 0.5))

  grid.arrange(x$d.plot.outside.vs.ribbon, x$env.d.plot.outside.vs.ribbon,
               x$i.plot.outside.vs.ribbon, x$env.i.plot.outside.vs.ribbon,
               x$cor.plot.outside.vs.ribbon, x$env.cor.plot.outside.vs.ribbon, ncol = 2) +
               theme(plot.title = element_text(hjust = 0.5))
}

