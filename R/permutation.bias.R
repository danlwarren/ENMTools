#' permutation.bias
#'
#' Implements a permutation test for the effects of sampling bias on ENM
#' projections, as described in Warren et al. (2021).  The empirical model is
#' projected to a transfer environment and a suite of summary statistics are
#' calculated for that projection.  A null distribution for each statistic is
#' then built by repeatedly resampling occurrence records at random from the
#' training region, refitting the model with identical settings, projecting to
#' the transfer environment, and recalculating the statistics.  When the
#' training and transfer regions overlap, change statistics (mean suitability
#' change and cells gained/lost using an MSS threshold) are also computed.
#' The function additionally returns rasters of the mean and variance of
#' per-cell suitability change across replicates, showing where models are
#' systematically biased toward predicting increasing or decreasing suitability
#' and how sensitive those predictions are to the input occurrence data.
#'
#' @param species An enmtools.species object
#' @param model A fitted enmtools.model object
#' @param training.env A SpatRaster of the training environment
#' @param transfer.env A SpatRaster of the transfer environment
#' @param nreps Number of replicate models to build. Defaults to 100.
#' @param bias Optional SpatRaster. If provided, random occurrence and
#'   background points are drawn with probability proportional to the
#'   estimated sampling intensity in each grid cell.
#' @param buffer.width Optional numeric. If provided, background points for
#'   each replicate are drawn from circular buffers of this radius around the
#'   resampled presence points, intersected with the bias layer (if provided).
#'   Units match the CRS of training.env.
#' @param pa.threshold Controls which MSS threshold is used to binarize replicate projections. "empirical" (default) applies the threshold from the fitted empirical model to all replicates, matching the approach in Warren et al. (2021). "replicate" calculates a separate threshold for each replicate from its own training evaluation.
#' @param verbose Controls printing of progress messages. Defaults to FALSE.
#' @param ... Additional arguments passed to the model fitting function when
#'   building replicate models (e.g., f, test.prop, clamp).
#'
#' @return An enmtools.permutation.bias object containing empirical and
#'   replicate statistics, mean and variance change rasters, and summary plots.
#'
#' @export
#'
#' @examples
#' \donttest{
#' monticola.glm <- enmtools.glm(iberolacerta.clade$species$monticola,
#'                               training.env = euro.worldclim,
#'                               f = pres ~ bio1 + bio12,
#'                               test.prop = 0.2)
#' pb <- permutation.bias(iberolacerta.clade$species$monticola,
#'                        monticola.glm,
#'                        training.env = euro.worldclim,
#'                        transfer.training.env = euro.worldclim,
#'                        nreps = 10)
#' }

permutation.bias <- function(species, model, training.env, transfer.env, nreps = 100,
                              bias = NULL, buffer.width = NULL,
                              pa.threshold = "empirical", verbose = FALSE, ...) {

  # --- Input validation and coercion ---
  species <- check.species(species)

  if(!inherits(training.env, "SpatRaster")){
    training.env <- terra::rast(training.env)
  }

  if(!inherits(transfer.env, "SpatRaster")){
    transfer.env <- terra::rast(transfer.env)
  }

  if(!is.null(bias) && !inherits(bias, "SpatRaster")){
    bias <- terra::rast(bias)
  }

  # Determine model type
  if(inherits(model, "enmtools.glm")){
    type <- "glm"
  } else if(inherits(model, "enmtools.gam")){
    type <- "gam"
  } else if(inherits(model, "enmtools.rf")){
    type <- "rf"
  } else if(inherits(model, "enmtools.rf.ranger")){
    type <- "rf.ranger"
  } else if(inherits(model, "enmtools.maxent")){
    type <- "mx"
  } else if(inherits(model, "enmtools.bc")){
    type <- "bc"
  } else if(inherits(model, "enmtools.dm")){
    type <- "dm"
  } else {
    stop("Unrecognized model type.")
  }

  n.pres <- length(model$training.evaluation@presence)
  n.bg   <- length(model$training.evaluation@absence)

  # Raster used to define valid sampling area (non-NA cells of training.env)
  env.mask <- training.env[[1]]

  # Bias raster masked to training environment extent
  if(!is.null(bias)){
    bias.masked <- terra::mask(terra::crop(bias, terra::ext(training.env)), env.mask)
  }

  # --- Empirical projection ---
  empirical.transfer <- pb.transfer.predict(model, transfer.env, type)
  mss.threshold      <- dismo::threshold(model$training.evaluation, "spec_sens")

  
  empirical.mean.suitability <- terra::global(empirical.transfer, mean, na.rm = TRUE)[[1]]
  empirical.n.suitable       <- terra::global(empirical.transfer >= mss.threshold, sum, na.rm = TRUE)[[1]]

  # Detect overlap between training and transfer extents
  has.overlap <- tryCatch({
    ov.ext <- terra::intersect(terra::ext(training.env), terra::ext(transfer.env))
    terra::ncell(terra::crop(env.mask, ov.ext)) > 0
  }, error = function(e) FALSE)

  empirical.mean.change <- NA_real_
  empirical.n.gained    <- NA_real_
  empirical.n.lost      <- NA_real_

  if(has.overlap){
    ov.ext <- terra::intersect(terra::ext(training.env), terra::ext(transfer.env))
    train.crop    <- terra::crop(model$suitability, ov.ext)
    transfer.crop <- terra::resample(terra::crop(empirical.transfer, ov.ext), train.crop)
    empirical.mean.change <- terra::global(transfer.crop - train.crop, mean, na.rm = TRUE)[[1]]

    train.bin    <- train.crop    >= mss.threshold
    transfer.bin <- transfer.crop >= mss.threshold
    empirical.n.gained <- terra::global((transfer.bin - train.bin) > 0,  sum, na.rm = TRUE)[[1]]
    empirical.n.lost   <- terra::global((train.bin - transfer.bin) > 0,  sum, na.rm = TRUE)[[1]]
  }

  # --- Build reusable argument list from the empirical model's stored call ---
  # Evaluate each argument in the caller's environment so that variable
  # references (e.g. f = my.formula) are resolved correctly.  species and env
  # are excluded here and injected fresh each iteration.
  calling.env <- parent.frame()
  mc <- model$call
  fn.name <- as.character(mc[[1]])
  base.args <- lapply(
    as.list(mc)[setdiff(names(mc), c("", "species", "env"))],
    eval, envir = calling.env
  )

  # --- Replicate loop ---
  reps.mean.suitability <- numeric(nreps)
  reps.n.suitable       <- numeric(nreps)
  reps.mean.change      <- rep(NA_real_, nreps)
  reps.n.gained         <- rep(NA_real_, nreps)
  reps.n.lost           <- rep(NA_real_, nreps)
  rep.training.rasters  <- list()
  rep.transfer.rasters  <- list()

  for(i in seq_len(nreps)){

    if(verbose){
      message("Building replicate ", i, " of ", nreps)
    }

    # Draw random presence points
    if(is.null(bias)){
      rep.pres.vect <- terra::spatSample(env.mask, size = n.pres, method = "random",
                                          na.rm = TRUE, as.points = TRUE)
    } else {
      rep.pres.vect <- terra::spatSample(bias.masked, size = n.pres, method = "weights",
                                          na.rm = TRUE, as.points = TRUE)
    }

    # Coerce to named data frame matching ENMTools conventions, then back to SpatVector
    rep.pres.df <- as.data.frame(terra::crds(rep.pres.vect))
    colnames(rep.pres.df) <- c("Longitude", "Latitude")
    rep.pres <- terra::vect(rep.pres.df, geom = c("Longitude", "Latitude"),
                             crs = terra::crs(training.env))

    # Draw background points
    if(!is.null(buffer.width)){
      buf.raster <- background.buffer(points = rep.pres, buffer.width = buffer.width,
                                       mask = training.env, return.type = "raster")
      if(!is.null(bias)){
        sample.raster <- terra::mask(bias, buf.raster)
      } else {
        sample.raster <- buf.raster
      }
      rep.bg.vect <- terra::spatSample(sample.raster, size = n.bg, method = "weights",
                                        na.rm = TRUE, as.points = TRUE)
    } else if(!is.null(bias)){
      rep.bg.vect <- terra::spatSample(bias.masked, size = n.bg, method = "weights",
                                        na.rm = TRUE, as.points = TRUE)
    } else {
      rep.bg.vect <- terra::spatSample(env.mask, size = n.bg, method = "random",
                                        na.rm = TRUE, as.points = TRUE)
    }

    rep.bg.df <- as.data.frame(terra::crds(rep.bg.vect))
    colnames(rep.bg.df) <- c("Longitude", "Latitude")
    rep.bg <- terra::vect(rep.bg.df, geom = c("Longitude", "Latitude"),
                           crs = terra::crs(training.env))

    # Build replicate species object
    rep.species                   <- enmtools.species()
    rep.species$presence.points   <- rep.pres
    rep.species$background.points <- rep.bg
    rep.species$species.name <- paste0("rep", i)

    # Fit replicate model using the same arguments as the empirical model
    rep.model <- do.call(fn.name,
                         c(list(species = rep.species, env = training.env), base.args))

    # Project to transfer environment
    rep.transfer <- pb.transfer.predict(rep.model, transfer.env, type)
    rep.training.rasters[[i]] <- rep.model$suitability
    rep.transfer.rasters[[i]] <- rep.transfer

    # Collect statistics
    if(pa.threshold == "replicate"){
      rep.mss <- dismo::threshold(rep.model$training.evaluation, "spec_sens")
    } else {
      rep.mss <- mss.threshold
    }
    reps.mean.suitability[i] <- terra::global(rep.transfer, mean, na.rm = TRUE)[[1]]
    reps.n.suitable[i]       <- terra::global(rep.transfer >= rep.mss, sum, na.rm = TRUE)[[1]]

    if(has.overlap){
      ov.ext <- terra::intersect(terra::ext(training.env), terra::ext(transfer.env))
      rep.train.crop    <- terra::crop(rep.model$suitability, ov.ext)
      rep.transfer.crop <- terra::resample(terra::crop(rep.transfer, ov.ext), rep.train.crop)
      reps.mean.change[i] <- terra::global(rep.transfer.crop - rep.train.crop, mean, na.rm = TRUE)[[1]]

      rep.train.bin    <- rep.train.crop    >= rep.mss
      rep.transfer.bin <- rep.transfer.crop >= rep.mss
      reps.n.gained[i] <- terra::global((rep.transfer.bin - rep.train.bin) > 0, sum, na.rm = TRUE)[[1]]
      reps.n.lost[i]   <- terra::global((rep.train.bin - rep.transfer.bin) > 0, sum, na.rm = TRUE)[[1]]
    }
  }

  # --- Mean and variance rasters of per-cell suitability change ---
  train.stack             <- terra::rast(rep.training.rasters)
  transfer.stack          <- terra::rast(rep.transfer.rasters)
  change.stack       <- transfer.stack - train.stack
  mean.change.raster <- terra::app(change.stack, mean)
  var.change.raster  <- terra::app(change.stack, var)

  # --- Assemble combined stats table (row 1 = empirical, rows 2:n+1 = replicates) ---
  combined.stats <- rbind(
    data.frame(mean.suitability = empirical.mean.suitability,
               n.suitable       = empirical.n.suitable,
               mean.change      = empirical.mean.change,
               n.gained         = empirical.n.gained,
               n.lost           = empirical.n.lost),
    data.frame(mean.suitability = reps.mean.suitability,
               n.suitable       = reps.n.suitable,
               mean.change      = reps.mean.change,
               n.gained         = reps.n.gained,
               n.lost           = reps.n.lost)
  )
  rownames(combined.stats) <- c("empirical", paste0("rep.", seq(1,nreps), sep = ""))

  # --- Plots ---
  plots <- list()

  plots[["mean.suitability"]] <- pb.histogram(combined.stats, "mean.suitability",
                                               xlab = "Mean suitability",
                                               title = "Mean suitability in transfer region")

  plots[["n.suitable"]] <- pb.histogram(combined.stats, "n.suitable",
                                         xlab = "Number of suitable cells",
                                         title = "Suitable cells in transfer region")

  if(has.overlap){
    plots[["mean.change"]] <- pb.histogram(combined.stats, "mean.change",
                                            xlab = "Mean suitability change",
                                            title = "Mean change in suitability across overlap region")

    plots[["n.gained"]] <- pb.histogram(combined.stats, "n.gained",
                                         xlab = "Cells gained",
                                         title = "Cells gained in transfer region")

    plots[["n.lost"]] <- pb.histogram(combined.stats, "n.lost",
                                       xlab = "Cells lost",
                                       title = "Cells lost from training region")
  }

  mean.change.points <- data.frame(rasterToPoints2(mean.change.raster))
  colnames(mean.change.points) <- c("x", "y", "MeanChange")

  plots[["average.estimated.bias"]] <- ggplot(data = mean.change.points,
                                               aes(y = .data$y, x = .data$x)) +
    geom_raster(aes(fill = .data$MeanChange)) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Mean change")) +
    coord_fixed() +
    theme_classic() +
    ggtitle("Average estimated bias") +
    theme(plot.title = element_text(hjust = 0.5))

  var.change.points <- data.frame(rasterToPoints2(var.change.raster))
  colnames(var.change.points) <- c("x", "y", "Leverage")

  plots[["leverage"]] <- ggplot(data = var.change.points,
                                 aes(y = .data$y, x = .data$x)) +
    geom_raster(aes(fill = .data$Leverage)) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Leverage")) +
    coord_fixed() +
    theme_classic() +
    ggtitle("Leverage") +
    theme(plot.title = element_text(hjust = 0.5))

  # --- Build description string ---
  if(!is.null(bias)){
    bias.str <- "Bias layer provided\n"
  } else {
    bias.str <- "No bias layer\n"
  }

  if(!is.null(buffer.width)){
    buffer.str <- paste0("Buffer width: ", buffer.width, "\n")
  } else {
    buffer.str <- ""
  }

  # --- Build output ---
  output <- list(
    description = paste0("Permutation bias test\n",
                         nreps, " replicates\n",
                         "Model type: ", type, "\n",
                         bias.str,
                         buffer.str),
    empirical.model                = model,
    empirical.transfer.suitability = empirical.transfer,
    combined.stats                 = combined.stats,
    mean.change.raster             = mean.change.raster,
    var.change.raster              = var.change.raster,
    plots                          = plots
  )

  class(output) <- "enmtools.permutation.bias"
  return(output)
}


# --- Internal helpers ---

pb.transfer.predict <- function(model, transfer.env, type) {

  if(type %in% c("glm", "gam")){
    return(terra::predict(transfer.env, model$model, type = "response", na.rm = TRUE))
  }

  if(type == "rf"){
    return(terra::predict(transfer.env, model$model, type = "prob", na.rm = TRUE)[["1"]])
  }

  if(type == "rf.ranger"){
    pred.fun <- function(model, data, ...) {
      predict(model, data = data, type = "response")$predictions[, 2]
    }
    return(terra::predict(transfer.env, model$model, fun = pred.fun, na.rm = TRUE))
  }

  if(type %in% c("mx", "bc", "dm")){
    return(terra::predict(transfer.env, model$model, na.rm = TRUE))
  }

}

pb.histogram <- function(combined.stats, col, xlab, title) {
  ggplot(combined.stats[2:nrow(combined.stats), , drop = FALSE],
         aes(x = .data[[col]], fill = "density", alpha = 0.5)) +
    geom_histogram(bins = 20) +
    geom_vline(xintercept = combined.stats[1, col], linetype = "longdash") +
    guides(fill = "none", alpha = "none") +
    xlab(xlab) +
    ylab("Count") +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5))
}


# --- S3 methods ---

#' @export
summary.enmtools.permutation.bias <- function(object, ...) {
  cat("\n\nPermutation Bias Analysis Results\n\n")
  cat(object$description)
  cat("\nEmpirical statistics:\n")
  print(object$combined.stats[1, , drop = FALSE])
  cat("\nReplicate statistics summary:\n")
  print(summary(object$combined.stats[2:nrow(object$combined.stats), ]))
}

#' @export
print.enmtools.permutation.bias <- function(x, ...) {
  summary(x, ...)
}

#' @export
plot.enmtools.permutation.bias <- function(x, ...) {
  n.plots <- length(x$plots)
  do.call(gridExtra::grid.arrange, c(x$plots, list(ncol = min(2, n.plots))))
}
