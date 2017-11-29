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
#' @param ... Additional arguments to be passed to model fitting functions.
#'
#' @return results A list containing a replicates, models for the empirical data, and summary statistics and plots.
#'
#' @keywords rangebreak, biogeography, barrier, enmtools, hypothesis testing
#'
#' @method print enmtools.rangebreak.ribbon
#' @method summary enmtools.rangebreak.ribbon
#' @method plot enmtools.rangebreak.ribbon
#' @export rangebreak.ribbon
#' @export rangebreak.ribbon.precheck
#'
#' @examples
#' rangebreak.ribbon(ahli, allogus, env, type = "glm", nreps = 10)
#'

rangebreak.ribbon <- function(species.1, species.2, ribbon, env, type, f = NULL, width = 1, nreps = 99,  nback = 1000, ...){

  species.1 <- check.bg(species.1, env, nback = nback, ...)
  species.2 <- check.bg(species.2, env, nback = nback, ...)
  ribbon <- check.bg(ribbon, env, nback = nback, ...)

  rangebreak.ribbon.precheck(species.1, species.2, ribbon, env, type, f, width, nreps)

  outside <- species.1
  outside$species.name <- "outside"
  outside$presence.points <- rbind(species.1$presence.points, species.2$presence.points)
  outside$background.points <- rbind(species.1$background.points, species.2$background.points, ribbon$background.points)

  # Initialize a list to store reps in
  replicate.models <- list()

  # For starters we need to combine species background points so that each model
  # is being built with the same background
  species.1$background.points <- rbind(species.1$background.points, species.2$background.points, ribbon$background.points)
  species.2$background.points <- rbind(species.1$background.points, species.2$background.points, ribbon$background.points)
  ribbon$background.points <- rbind(species.1$background.points, species.2$background.points, ribbon$background.points)

  combined.presence.points <- rbind(species.1$presence.points, species.2$presence.points, ribbon$presence.points)

  # Build models for empirical data
  cat("\nBuilding empirical models...\n")
  if(type == "glm"){
    empirical.species.1.model <- enmtools.glm(species.1, env, f, ...)
    empirical.species.2.model <- enmtools.glm(species.2, env, f, ...)
    empirical.ribbon.model <- enmtools.glm(ribbon, env, f, ...)
    empirical.outside.model <- enmtools.glm(outside, env, f, ...)
  }

  if(type == "gam"){
    empirical.species.1.model <- enmtools.gam(species.1, env, f, ...)
    empirical.species.2.model <- enmtools.gam(species.2, env, f, ...)
    empirical.ribbon.model <- enmtools.gam(ribbon, env, f, ...)
    empirical.outside.model <- enmtools.gam(outside, env, f, ...)
  }

  if(type == "mx"){
    empirical.species.1.model <- enmtools.maxent(species.1, env, ...)
    empirical.species.2.model <- enmtools.maxent(species.2, env, ...)
    empirical.ribbon.model <- enmtools.maxent(ribbon, env, ...)
    empirical.outside.model <- enmtools.maxent(outside, env, ...)
  }

  if(type == "bc"){
    empirical.species.1.model <- enmtools.bc(species.1, env, ...)
    empirical.species.2.model <- enmtools.bc(species.2, env, ...)
    empirical.ribbon.model <- enmtools.bc(ribbon, env, ...)
    empirical.outside.model <- enmtools.bc(outside, env, ...)
  }

  if(type == "dm"){
    empirical.species.1.model <- enmtools.dm(species.1, env, ...)
    empirical.species.2.model <- enmtools.dm(species.2, env, ...)
    empirical.ribbon.model <- enmtools.dm(ribbon, env, ...)
    empirical.outside.model <- enmtools.dm(outside, env, ...)
  }

  if(type == "rf"){
    empirical.species.1.model <- enmtools.rf(species.1, env, ...)
    empirical.species.2.model <- enmtools.rf(species.2, env, ...)
    empirical.ribbon.model <- enmtools.rf(ribbon, env, ...)
    empirical.outside.model <- enmtools.rf(outside, env, ...)
  }

  empirical.overlap.sp1.vs.sp2 <- c(unlist(raster.overlap(empirical.species.1.model, empirical.species.2.model)),
                                    unlist(env.overlap(empirical.species.1.model, empirical.species.2.model, env = env)))
  reps.overlap.sp1.vs.sp2 <- empirical.overlap.sp1.vs.sp2

  empirical.overlap.sp1.vs.ribbon <- c(unlist(raster.overlap(empirical.species.1.model, empirical.ribbon.model)),
                                       unlist(env.overlap(empirical.species.1.model, empirical.ribbon.model, env = env)))
  reps.overlap.sp1.vs.ribbon <- empirical.overlap.sp1.vs.ribbon

  empirical.overlap.sp2.vs.ribbon <- c(unlist(raster.overlap(empirical.species.2.model, empirical.ribbon.model)),
                                       unlist(env.overlap(empirical.species.2.model, empirical.ribbon.model, env = env)))
  reps.overlap.sp2.vs.ribbon <- empirical.overlap.sp2.vs.ribbon

  empirical.overlap.outside.vs.ribbon <- c(unlist(raster.overlap(empirical.outside.model, empirical.ribbon.model)),
                                           unlist(env.overlap(empirical.outside.model, empirical.ribbon.model, env = env)))
  reps.overlap.outside.vs.ribbon <- empirical.overlap.outside.vs.ribbon

  lines.df <- data.frame(slope = rep(NA, nreps), intercept = rep(NA, nreps), offset = rep(NA, nreps))

  cat("\nBuilding replicate models...\n")

  # We'll use this to keep track of how many iterations were successful
  keepers <- 1

  while(keepers <= nreps){
    cat(paste("\nReplicate", keepers, "...\n"))

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

    # Grab ribbon points, pull them out of part.points
    ribbon.points <- which(part.points[,3] > (intercept - intercept.modifier) & part.points[,3] < (intercept + intercept.modifier))
    rep.ribbon$presence.points <- part.points[ribbon.points,1:2]
    part.points <- part.points[-ribbon.points,]

    # Putting all remaining points in rep.outside
    rep.outside$presence.points <- part.points[,1:2]

    # Splitting remaining points in proportion to the relative sample sizes of the empirical data
    prop <- nrow(species.1$presence.points)/nrow(species.2$presence.points)
    rep.species.1$presence.points <- part.points[1:floor(prop * nrow(part.points)), 1:2]
    rep.species.2$presence.points <- part.points[(floor(prop * nrow(part.points)) + 1):nrow(part.points), 1:2]


    # Make sure we actually got some ribbon points.  If not, fail this round and try again.
    if(!nrow(rep.ribbon$presence.points) > 1){
      next
    }

    # Store the slope, intercept, and modifier for this round
    lines.df[keepers,] <- c(slope, intercept, intercept.modifier)


    if(type == "glm"){
      rep.species.1.model <- enmtools.glm(rep.species.1, env, f, ...)
      rep.species.2.model <- enmtools.glm(rep.species.2, env, f, ...)
      rep.ribbon.model <- enmtools.glm(rep.ribbon, env, f, ...)
      rep.outside.model <- enmtools.glm(rep.outside, env, f, ...)
    }

    if(type == "gam"){
      rep.species.1.model <- enmtools.gam(rep.species.1, env, f, ...)
      rep.species.2.model <- enmtools.gam(rep.species.2, env, f, ...)
      rep.ribbon.model <- enmtools.gam(rep.ribbon, env, f, ...)
      rep.outside.model <- enmtools.gam(rep.outside, env, f, ...)
    }

    if(type == "mx"){
      rep.species.1.model <- enmtools.maxent(rep.species.1, env, ...)
      rep.species.2.model <- enmtools.maxent(rep.species.2, env, ...)
      rep.ribbon.model <- enmtools.maxent(rep.ribbon, env, ...)
      rep.outside.model <- enmtools.maxent(rep.outside, env, ...)
    }

    if(type == "bc"){
      rep.species.1.model <- enmtools.bc(rep.species.1, env, ...)
      rep.species.2.model <- enmtools.bc(rep.species.2, env, ...)
      rep.ribbon.model <- enmtools.bc(rep.ribbon, env, ...)
      rep.outside.model <- enmtools.bc(rep.outside, env, ...)
    }

    if(type == "dm"){
      rep.species.1.model <- enmtools.dm(rep.species.1, env, ...)
      rep.species.2.model <- enmtools.dm(rep.species.2, env, ...)
      rep.ribbon.model <- enmtools.dm(rep.ribbon, env, ...)
      rep.outside.model <- enmtools.dm(rep.outside, env, ...)
    }

    if(type == "rf"){
      rep.species.1.model <- enmtools.rf(rep.species.1, env, ...)
      rep.species.2.model <- enmtools.rf(rep.species.2, env, ...)
      rep.ribbon.model <- enmtools.rf(rep.ribbon, env, ...)
      rep.outside.model <- enmtools.rf(rep.outside, env, ...)
    }

    # Appending models to replicates list
    replicate.models[[paste0(species.1$species.name, ".rep.", keepers)]] <- rep.species.1.model
    replicate.models[[paste0(species.2$species.name, ".rep.", keepers)]] <- rep.species.2.model
    replicate.models[[paste0("ribbon.rep.", keepers)]] <- rep.ribbon.model
    replicate.models[[paste0("outside.rep.", keepers)]] <- rep.outside.model

    # Measure overlaps
    this.overlap.sp1.vs.sp2  <- c(unlist(raster.overlap(rep.species.1.model, rep.species.2.model)),
                                  unlist(env.overlap(rep.species.1.model, rep.species.2.model, env = env)))
    this.overlap.sp1.vs.ribbon <- c(unlist(raster.overlap(rep.species.1.model, rep.ribbon.model)),
                                    unlist(env.overlap(rep.species.1.model, rep.ribbon.model, env = env)))
    this.overlap.sp2.vs.ribbon <- c(unlist(raster.overlap(rep.species.2.model, rep.ribbon.model)),
                                    unlist(env.overlap(rep.species.2.model, rep.ribbon.model, env = env)))
    this.overlap.outside.vs.ribbon <- c(unlist(raster.overlap(rep.outside.model, rep.ribbon.model)),
                                        unlist(env.overlap(rep.outside.model, rep.ribbon.model, env = env)))

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

  p.values.sp1.vs.sp2 <- apply(reps.overlap.sp1.vs.sp2, 2, function(x) 1 - mean(x > x[1], na.rm=FALSE))
  p.values.sp1.vs.ribbon <- apply(reps.overlap.sp1.vs.ribbon, 2, function(x) 1 - mean(x > x[1], na.rm=FALSE))
  p.values.sp2.vs.ribbon <- apply(reps.overlap.sp2.vs.ribbon, 2, function(x) 1 - mean(x > x[1], na.rm=FALSE))
  p.values.outside.vs.ribbon <- apply(reps.overlap.outside.vs.ribbon, 2, function(x) 1 - mean(x > x[1], na.rm=FALSE))


  ### Plots for sp1 vs sp2
  d.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs.", species.2$species.name))

  i.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs.", species.2$species.name))

  cor.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"rank.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"rank.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs.", species.2$species.name))

  env.d.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"env.D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"env.D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs.", species.2$species.name))

  env.i.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"env.I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"env.I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs.", species.2$species.name))

  env.cor.plot.sp1.vs.sp2 <- qplot(reps.overlap.sp1.vs.sp2[2:nrow(reps.overlap.sp1.vs.sp2),"env.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.sp2[1,"env.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs.", species.2$species.name))


  ### Plots for sp1 vs ribbon
  d.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs. ribbon"))

  i.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs. ribbon"))

  cor.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"rank.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"rank.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs. ribbon"))

  env.d.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"env.D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"env.D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs. ribbon"))

  env.i.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"env.I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"env.I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs. ribbon"))

  env.cor.plot.sp1.vs.ribbon <- qplot(reps.overlap.sp1.vs.ribbon[2:nrow(reps.overlap.sp1.vs.ribbon),"env.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp1.vs.ribbon[1,"env.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.1$species.name, "vs. ribbon"))



  ### Plots for sp2 vs ribbon
  d.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test:", species.2$species.name, "vs. ribbon"))

  i.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test:", species.2$species.name, "vs. ribbon"))

  cor.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"rank.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"rank.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test:", species.2$species.name, "vs. ribbon"))

  env.d.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"env.D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"env.D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.2$species.name, "vs. ribbon"))

  env.i.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"env.I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"env.I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.2$species.name, "vs. ribbon"))

  env.cor.plot.sp2.vs.ribbon <- qplot(reps.overlap.sp2.vs.ribbon[2:nrow(reps.overlap.sp2.vs.ribbon),"env.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.sp2.vs.ribbon[1,"env.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test:", species.2$species.name, "vs. ribbon"))


  ### Plots for outside vs ribbon
  d.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
    ggtitle(paste("Rangebreak test: outside vs. ribbon"))

  i.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
    ggtitle(paste("Rangebreak test: outside vs. ribbon"))

  cor.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"rank.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"rank.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation") +
    ggtitle(paste("Rangebreak test: outside vs. ribbon"))

  env.d.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"env.D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"env.D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D, Environmental Space") +
    ggtitle(paste("Rangebreak test: outside vs. ribbon"))

  env.i.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"env.I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"env.I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I, Environmental Space") +
    ggtitle(paste("Rangebreak test: outside vs. ribbon"))

  env.cor.plot.outside.vs.ribbon <- qplot(reps.overlap.outside.vs.ribbon[2:nrow(reps.overlap.outside.vs.ribbon),"env.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap.outside.vs.ribbon[1,"env.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation, Environmental Space") +
    ggtitle(paste("Rangebreak test: outside vs. ribbon"))


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
               x$cor.plot.sp1.vs.sp2, x$env.cor.plot.sp1.vs.sp2, ncol = 2)

  grid.arrange(x$d.plot.sp1.vs.ribbon, x$env.d.plot.sp1.vs.ribbon,
               x$i.plot.sp1.vs.ribbon, x$env.i.plot.sp1.vs.ribbon,
               x$cor.plot.sp1.vs.ribbon, x$env.cor.plot.sp1.vs.ribbon, ncol = 2)

  grid.arrange(x$d.plot.sp2.vs.ribbon, x$env.d.plot.sp2.vs.ribbon,
               x$i.plot.sp2.vs.ribbon, x$env.i.plot.sp2.vs.ribbon,
               x$cor.plot.sp2.vs.ribbon, x$env.cor.plot.sp2.vs.ribbon, ncol = 2)

  grid.arrange(x$d.plot.outside.vs.ribbon, x$env.d.plot.outside.vs.ribbon,
               x$i.plot.outside.vs.ribbon, x$env.i.plot.outside.vs.ribbon,
               x$cor.plot.outside.vs.ribbon, x$env.cor.plot.outside.vs.ribbon, ncol = 2)
}

