#' Takes an overlap matrix and a tree and runs permutation tests to
#' determine the statistical significance of the relationship between
#' overlap and time
#'
#' @param overlap An overlap matrix
#' @param tree A tree
#' @param nreps A number of reps to do
#'
#' @export age.overlap.correlation

age.overlap.correlation <- function(overlap, tree, nreps){

  description <- "Age-Overlap Correlation from Monte Carlo Test"

  # de-triangularize matrix
  if(is.na(overlap[1, ncol(overlap)])){
    overlap[upper.tri(overlap)] <- t(overlap)[upper.tri(overlap)]
  }

  if(is.na(overlap[nrow(overlap),1])){
    overlap[lower.tri(overlap)] <- t(overlap)[lower.tri(overlap)]
  }

  # Make sure the data's okay
  age.overlap.correlation.precheck(overlap, tree, nreps)

  # Get overlaps for empirical data
  empirical.df <- node.overlap(overlap, tree)

  # Build an empirical lm
  empirical.model <- lm(overlap ~ age, data = empirical.df)

  # Define a function for each rep so we can try to apply it
  do.rep <- function(inds) {
    tree$tip.label <- tree$tip.label[inds]
    rep.df <- node.overlap(overlap, tree)
    return(list(rep.df = rep.df,
                rep.lm = lm(overlap ~ age, data = rep.df)))
  }

  reps <- list()

  for(i in 1:nreps){
     this.rep <- sample(nrow(overlap))
     reps[[paste0("rep.", i)]] <- do.rep(sample(length(tree$tip.label)))$rep.lm
  }

  reps.aoc <- rbind(empirical.model$coefficients,
                    do.call(rbind, lapply(reps, function(x) x$coefficients)))

  rownames(reps.aoc) <- c("empirical", paste("rep", 1:nreps))

  p.values <- apply(reps.aoc, 2, function(x) 1 - mean(x > x[1]))

  intercept.plot <- qplot(reps.aoc[2:nrow(reps.aoc),"(Intercept)"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.aoc[1,"(Intercept)"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Intercept") + ggtitle(description)

  slope.plot <- qplot(reps.aoc[2:nrow(reps.aoc),"age"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.aoc[1,"age"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Slope") + ggtitle(description)

  regressions.plot <- qplot(age, overlap, data = empirical.df) + theme_bw()
  for(i in 2:nrow(reps.aoc)){
    regressions.plot <- regressions.plot + geom_abline(slope=reps.aoc[i,2],
                                       intercept=reps.aoc[i,1],
                                       color="grey86")
  }
  regressions.plot <- regressions.plot + geom_abline(slope = reps.aoc[1,2], intercept = reps.aoc[1,1]) +
    geom_point()

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

summary.enmtools.aoc <- function(this.aoc){
  cat("\n\nAge-Overlap Correlation test\n\n")
  cat(paste(length(this.aoc$reps), "replicates", "\n"))
}

print.enmtools.aoc <- function(this.aoc){
  summary(this.aoc)
}

plot.enmtools.aoc <- function(this.aoc){

}

age.overlap.correlation.precheck <- function(overlap, tree, nreps){
  # Test to make sure our data aren't crap
  if(any(!rownames(overlap) %in% colnames(overlap))){
    stop("Row and column names do not match!")
  }

  if(!inherits(tree, "phylo")){
    stop("Tree is not a phylo object!")
  }

  if(any(!rownames(overlap) %in% tree$tip.label)){
    stop("Overlap matrix names and tip labels do not match!")
  }

  if(is.null(tree$edge.length)){
    stop("Tree does not have branch lengths!")
  }

  if(any(is.na(overlap))){
    stop("Overlap matrix contains NAs!")
  }
}
