#' Takes an overlap matrix and a tree and runs permutation tests to
#' determine the statistical significance of the relationship between
#' overlap and time
#'
#' @param clade An enmtools.clade object containing species data and a phylogeny
#' @param env Environmental layers for use when overlap is calculated using niche models.
#' @param overlap.source The source of the overlaps to calculate.  Choices are "range", "point", "matrix", or a model type: "glm", "gam", "bc", "dm", "rf", "rf.ranger", "maxnet", "mx", or "tidy" (see `model`).
#' @param nreps A number of reps to do
#' @param f Optional.  A formula or [recipes::recipe()] specifying the predictors to use for ENM-based overlaps.  If NULL (the default), each model uses the default for its type: every layer in `env` is used as a predictor, as linear terms for GLMs and smooth terms for GAMs.  See the documentation for each modeling function (e.g. [enmtools.glm()]) for details.  Not used for Maxent models (overlap.source = "mx").
#' @param model When overlap.source = "tidy", a parsnip model specification with mode "classification", passed to [enmtools.tidy()].
#' @param model.args When overlap.source = "tidy", a list of arguments for the model specification or its engine, passed to [enmtools.tidy()].
#' @param clamp Passed to the modeling functions for ENM-based overlaps.  If NULL (the default), each model type's own default is used: presence-only envelope models (Bioclim, DOMAIN, and hypervolumes) are not clamped, and other models are.
#' @param overlap.matrix A matrix of overlaps to use, for option overlap.source = "matrix"
#' @param metric The overlap metric to use. For ENM sources, this can be any combination of "D", "I", "cor", "env.D", "env.I", and "env.cor".
#' for range and point overlap this argument is ignored.
#' @param ... Arguments to be passed to modeling functions for ENM-based overlaps.
#'
#' @return A list containing a data frame of coefficients from the empirical regression of overlap on time along with the coefficients from all Monte Carlo replicates, along with plots and p values for the accompanying statistical tests.

enmtools.aoc <- function(clade, env = NULL,  overlap.source, nreps = 100, f = NULL, model = NULL, model.args = list(), clamp = NULL, overlap.matrix = NULL, metric = "D", ...){

  description <- "Age-Overlap Correlation from Monte Carlo Test"

  clade <- check.clade(clade)

  # Make sure the data's okay
  enmtools.aoc.precheck(clade, nreps, overlap.source, env, f, model, overlap.matrix, metric)

  # Generate empirical overlaps

  # ENM-based overlaps
  if(overlap.source %in% model.types){
    empirical.models <- lapply(clade$species, function(x) enmtools.model.by.type(overlap.source, x, env, f = f, model = model, model.args = model.args, clamp = clamp, ...))
  }

  if(is.na(match(overlap.source, c("range", "points", "matrix")))){
    if(grepl(pattern = "^env", metric)){
      overlap <- sapply(empirical.models, function(x) sapply(empirical.models, function(y) env.overlap(x,y, env=env)[metric]))
    } else {
      overlap <- sapply(empirical.models, function(x) sapply(empirical.models, function(y) raster.overlap(x,y)[metric]))
    }
  }



  # Range rasters
  if(overlap.source == "range"){

    # Do pairwise for all species
    overlap <- sapply(clade$species, function(x) sapply(clade$species, function(y) geog.range.overlap(x,y)))
  }

  # Presence points
  if(overlap.source == "points"){
    overlap <- sapply(clade$species, function(x) sapply(clade$species, function(y) point.overlap(x,y)))
  }

  if(overlap.source == "matrix"){
    overlap = overlap.matrix
  }

  tree <- clade$tree
  tree$node.label <- NULL

  # sapply is renaming rows, gotta change tnem back
  rownames(overlap) <- colnames(overlap)

  # Scale empirical overlaps for phylogeny
  empirical.df <- node.overlap(overlap, tree)

  # Build an empirical lm
  empirical.model <- lm(empirical.df$overlap ~ empirical.df$age)

  # Define a function for each rep so we can try to apply it
  do.rep <- function(inds) {
    tree$tip.label <- tree$tip.label[inds]
    rep.df <- node.overlap(overlap, tree)
    return(list(rep.df = rep.df,
                rep.lm = lm(rep.df$overlap ~ rep.df$age)))
  }

  reps <- list()

  if (requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = " [:bar] :percent eta: :eta",
      total = nreps, clear = FALSE, width= 60)
  }

  for(i in 1:nreps){
    if (requireNamespace("progress", quietly = TRUE)) {
      pb$tick()
    }
    this.rep <- sample(nrow(overlap))
    reps[[paste0("rep.", i)]] <- do.rep(sample(length(tree$tip.label)))$rep.lm
  }

  reps.aoc <- rbind(empirical.model$coefficients,
                    do.call(rbind, lapply(reps, function(x) x$coefficients)))

  rownames(reps.aoc) <- c("empirical", paste("rep", 1:nreps))
  colnames(reps.aoc) <- c("Intercept", "Age")

  # Modified for two-tailed test
  p.values <- apply(reps.aoc, 2, function(x) min(rank(x)[1], rank(-x)[1])/length(x))

  reps.aoc <- as.data.frame(reps.aoc)

  intercept.plot <- ggplot(reps.aoc[2:nrow(reps.aoc),], aes(x = .data$Intercept, fill = "density", alpha = 0.5)) +
    geom_histogram(bins = 20) +
    geom_vline(xintercept = reps.aoc[1,"Intercept"], linetype = "longdash") +
    guides(fill = "none", alpha = "none") + xlab("Intercept") + ggtitle(description) +
    theme(plot.title = element_text(hjust = 0.5))

  slope.plot <- ggplot(reps.aoc[2:nrow(reps.aoc),], aes(x = .data$Age, fill = "density", alpha = 0.5)) +
    geom_histogram(bins = 20) +
    geom_vline(xintercept = reps.aoc[1,"Age"], linetype = "longdash") +
    guides(fill = "none", alpha = "none") + xlab("Slope") + ggtitle(description) +
    theme(plot.title = element_text(hjust = 0.5))



  regressions.plot <- ggplot(data = empirical.df, aes(x = .data$age, y = .data$overlap)) +
    theme_bw() + geom_point()
  for(i in 2:min(100, nrow(reps.aoc))){
    regressions.plot <- regressions.plot + geom_abline(slope=reps.aoc[i,2],
                                                       intercept=reps.aoc[i,1],
                                                       color="grey86")
  }
  regressions.plot <- regressions.plot + geom_abline(slope = reps.aoc[1,2], intercept = reps.aoc[1,1]) +
    geom_point() + ylim(0, 1) + xlim(0, 1.1 * max(empirical.df$age)) +
    theme(plot.title = element_text(hjust = 0.5)) + xlab("Age") + ylab("Overlap")

  output <- list(coefficients = reps.aoc,
                 p.values = p.values,
                 intercept.plot = intercept.plot,
                 slope.plot = slope.plot,
                 regressions.plot = regressions.plot,
                 tree = tree,
                 empirical.overlap = overlap,
                 empirical.df = empirical.df,
                 empirical.model = empirical.model,
                 reps = reps)
  class(output) <- "enmtools.aoc"

  return(output)
}

#' @exportS3Method
summary.enmtools.aoc <- function(object, ...){

  cat("\n\nAge-Overlap Correlation test\n\n")
  cat(paste(length(object$reps), "replicates", "\n\n"))

  cat("p values:\n")
  print(object$p.values)

  plot(object)

}

#' @exportS3Method
print.enmtools.aoc <- function(x, ...){
  summary(x)
}

#' @exportS3Method
plot.enmtools.aoc <- function(x, ...){

  assert.extras.this.fun()

  plot(x$tree, no.margin=TRUE, edge.width=2, cex=1)
  ape::nodelabels(format(x$empirical.df$overlap, digits = 1, nsmall = 2))

  grid.arrange(x$regressions.plot, x$intercept.plot,
               x$slope.plot, ncol = 2) +
    theme(plot.title = element_text(hjust = 0.5))

}

enmtools.aoc.precheck <- function(clade, nreps, overlap.source, env, f, model, overlap.matrix, metric){

  if(!inherits(clade$tree, "phylo")){
    stop("Tree is not a phylo object!")
  }

  if(is.null(clade$tree$edge.length)){
    stop("Tree does not have branch lengths!")
  }

  # Check to make sure env data is good
  if(!overlap.source %in% c(model.types, "range", "point", "points", "matrix")){
    stop(paste0("overlap.source ", overlap.source, " not understood! Select one of: ", paste(c(model.types, "range", "point", "matrix"), collapse = ", "), "."))
  }

  if(overlap.source %in% model.types){
    check.model.type(overlap.source, f, model)
  }

  if(overlap.source %in% model.types){
    if(!inherits(env, c("SpatRaster"))){
      stop("No environmental rasters were supplied!")
    }
  }

  if(overlap.source == "range"){
    if(any(is.na(lapply(clade$species, function(x) x$range)))){

      stop(paste("Overlap source set to range, but some species are missing range rasters: ",
                 paste(names(clade$species)[which(is.na(lapply(clade$species, function(x) x$range)))], collapse = ", ")))
    }
  }

}
