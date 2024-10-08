#' Takes an enmtools.species object with presence and background points, and builds a GLM
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data.
#' @param f Standard GLM formula
#' @param test.prop Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.
#' @param eval Determines whether model evaluation should be done.  Turned on by default, but moses turns it off to speed things up.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param env.nback Number of points to draw from environment space for environment space discrimination metrics.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param rts.reps The number of replicates to do for a Raes and ter Steege-style test of significance
#' @param weights If this is set to "equal", presences and background data will be assigned weights so that the sum of all presence points weights equals the sum of all background point weights.  Otherwise, weights are not provided to the model.
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param clamp When set to TRUE, clamps the environmental layers so that predictions made outside the min/max of the training data for each predictor are set to the value for the min/max for that predictor. Prevents the model from extrapolating beyond the min/max bounds of the predictor space the model was trained in, although there could still be projections outside the multivariate training space if predictors are strongly correlated.
#' @param corner An integer from 1 to 4.  Selects which corner to use for "block" test data.  By default the corner is selected randomly.
#' @param bias An optional raster estimating relative sampling effort per grid cell.  Will be used for drawing background data.
#' @param step Logical determining whether to do stepwise model selection or not
#' @param ... Arguments to be passed to glm()
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @examples
#' enmtools.glm(iberolacerta.clade$species$monticola, env = euro.worldclim, f = pres ~ bio1 + bio9)



enmtools.glm <- function(species, env, f = NULL, test.prop = 0, eval = TRUE, nback = 1000, env.nback = 10000, report = NULL, overwrite = FALSE, rts.reps = 0, weights = "equal", bg.source = "default",  verbose = FALSE, clamp = TRUE, corner = NA, bias = NA, step = FALSE, ...){

  notes <- NULL

  env <- check.raster(env, "env")

  species <- check.bg(species, env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias)

  # Builds a default formula using all env
  if(is.null(f)){
    f <- as.formula(paste("presence", paste(c(names(env)), collapse = " + "), sep = " ~ "))
    notes <- c(notes, "No formula was provided, so a GLM formula was built automatically.")
  }

  glm.precheck(f, species, env)

  # Declaring some NAs in case we skip evaluation
  test.data <- NA
  model.evaluation <- NA
  env.model.evaluation <- NA
  test.evaluation <- NA
  env.test.evaluation <- NA
  rts.test <- NA

  # Code for randomly withheld test data
  if(is.numeric(test.prop)){
    if(test.prop > 0 & test.prop < 1){
      test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
      test.data <- species$presence.points[test.inds,]
      species$presence.points <- species$presence.points[-test.inds,]
    }
  }

  # Code for spatially structured test data
  if(is.character(test.prop)){
    if(test.prop == "block"){
      if(is.na(corner)){
        corner <- ceiling(runif(1, 0, 4))
      } else if(corner < 1 | corner > 4){
        stop("corner should be an integer from 1 to 4!")
      }
      test.inds <- get.block(terra::crds(species$presence.points), terra::crds(species$background.points))
      test.bg.inds <- which(test.inds$bg.grp == corner)
      test.inds <- which(test.inds$occs.grp == corner)
      test.data <- species$presence.points[test.inds,]
      test.bg <- species$background.points[test.bg.inds,]
      species$presence.points <- species$presence.points[-test.inds,]
      species$background.points <- species$background.points[-test.bg.inds,]
    }
  }


  # Sample code for ENMeval
  # Checkerboard 1
  # test.inds <- get.checkerboard1(analysis.df[analysis.df$presence == 1,], env,
  #                          analysis.df[analysis.df$presence == 0,], 2)

  ### Add env data
  species <- add.env(species, env, verbose = verbose)

  # Recast this formula so that the response variable is named "presence"
  # regardless of what was passed.
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"), response = "presence")

  analysis.df <- make_analysis.df(species)
  # pres <- species$presence.points
  # abs <- species$background.points
  # pres$presence <- 1
  # abs$presence <- 0
  # analysis.df <- terra::as.data.frame(rbind(pres, abs), geom = "XY")
  # analysis.df <- analysis.df[ , c("x", "y", colnames(analysis.df)[!colnames(analysis.df) %in% c("x", "y")])]

  if(weights == "equal"){
    weights <- c(rep(1, nrow(species$presence.points)),
                 rep(nrow(species$presence.points)/nrow(species$background.points),
                     nrow(species$background.points)))
  } else {
    weights <- rep(1, nrow(species$presence.points) + nrow(species$background.points))
  }

  this.glm <- glm(f, analysis.df[,-c(1,2)], family="binomial", weights = weights, ...)
  if(step == TRUE){
    if(verbose == TRUE){
      this.glm <- step(this.glm)
    } else {
      invisible(capture.output(this.glm <- step(this.glm)))
    }
  }


  if(as.integer(this.glm$aic) == 2 * length(this.glm$coefficients)){
    notes <- c(notes, "AIC is 2x number of coefficients, indicating an uninformative model.  This often indicates that you have too many predictors for your number of data points.")
  }

  suitability <- terra::predict(env, this.glm, type = "response", na.rm = TRUE)

  # Clamping and getting a diff layer
  clamping.strength <- NA
  if(clamp == TRUE){
    env <- clamp.env(analysis.df, env)
    clamped.suitability <- terra::predict(env, this.glm, type = "response", na.rm = TRUE)
    clamping.strength <- clamped.suitability - suitability
    suitability <- clamped.suitability
  }

  if(eval == TRUE){

    # This is a very weird hack that has to be done because dismo's evaluate function
    # fails if the stack only has one layer.
    if(length(names(env)) == 1){
      oldname <- names(env)
      env <- c(env, env)
      names(env) <- c(oldname, "dummyvar")
      notes <- c(notes, "Only one predictor was provided, so a dummy variable was created in order to be compatible with dismo's prediction function.")
    }

    model.evaluation <-dismo::evaluate(species$presence.points, species$background.points,
                                       this.glm, env, na.rm = TRUE)
    env.model.evaluation <- env.evaluate(species, this.glm, env, n.background = env.nback)

    # Test eval for randomly withheld data
    if(is.numeric(test.prop)){
      if(test.prop > 0 & test.prop < 1){
        test.check <- terra::extract(env, test.data, ID = FALSE)
        test.data <- test.data[complete.cases(test.check),]
        test.evaluation <-dismo::evaluate(test.data, species$background.points,
                                          this.glm, env, na.rm = TRUE)
        temp.sp <- species
        temp.sp$presence.points <- test.data
        env.test.evaluation <- env.evaluate(temp.sp, this.glm, env, n.background = env.nback)
      }
    }

    # Test eval for spatially structured data
    if(is.character(test.prop)){
      if(test.prop == "block"){
        test.check <- terra::extract(env, test.data, ID = FALSE)
        test.data <- test.data[complete.cases(test.check),]
        test.evaluation <-dismo::evaluate(test.data, test.bg,
                                          this.glm, env, na.rm = TRUE)
        temp.sp <- species
        temp.sp$presence.points <- test.data
        temp.sp$background.points <- test.bg
        env.test.evaluation <- env.evaluate(temp.sp, this.glm, env, n.background = env.nback)
      }
    }


    # Do Raes and ter Steege test for significance.  Turned off if eval == FALSE
    if(rts.reps > 0 & eval == TRUE){

      message("\nBuilding RTS replicate models...\n")

      # Die if we're not doing randomly withheld test data and RTS reps > 0
      if(!is.numeric(test.prop)){
        stop(paste("RTS test can only be conducted with randomly withheld data, and test.prop is set to", test.prop))
      }

      rts.models <- list()

      rts.geog.training <- c()
      rts.geog.test <- c()
      rts.env.training <- c()
      rts.env.test <- c()

      if (requireNamespace("progress", quietly = TRUE)) {
        pb <- progress::progress_bar$new(
          format = " [:bar] :percent eta: :eta",
          total = rts.reps, clear = FALSE, width= 60)
      }

      for(i in 1:rts.reps){

        if (requireNamespace("progress", quietly = TRUE)) {
          pb$tick()
        }

        if(verbose == TRUE){message(paste("Replicate", i, "of", rts.reps))}

        # Repeating analysis with scrambled pa points and then evaluating models
        rep.species <- species

        # Mix the points all together
        if(test.prop > 0) {
          test <- as.data.frame(test.data, geom = "XY")[ , c("x", "y")]
        } else {
          test <- NULL
        }
        allpoints <- rbind(test,
                           as.data.frame(species$background.points, geom = "XY")[ , c("x", "y")],
                           as.data.frame(species$presence.points, geom = "XY")[ , c("x", "y")])

        # Sample presence points from pool and remove from pool
        rep.rows <- sample(nrow(allpoints), nrow(species$presence.points))
        rep.species$presence.points <- terra::vect(allpoints[rep.rows,], geom=c("x", "y"), crs = terra::crs(species$presence.points))
        allpoints <- allpoints[-rep.rows,]

        # Do the same for test points
        if(test.prop > 0){
          test.rows <- sample(nrow(allpoints), nrow(test.data))
          rep.test.data <- allpoints[test.rows,]
          allpoints <- allpoints[-test.rows,]
        }

        # Everything else goes back to the background
        rep.species$background.points <- terra::vect(allpoints, geom=c("x", "y"), crs = terra::crs(species$presence.points))

        rep.species <- add.env(rep.species, env, verbose = verbose)

        rts.df <- make_analysis.df(rep.species)
        thisrep.glm <- glm(f, rts.df[,-c(1,2)], family="binomial", ...)
        if(step == TRUE){
          if(verbose == TRUE){
            this.glm <- step(this.glm)
          } else {
            invisible(capture.output(this.glm <- step(this.glm)))
          }
        }

        thisrep.model.evaluation <-dismo::evaluate(rep.species$presence.points, species$background.points,
                                                   thisrep.glm, env, na.rm = TRUE)
        thisrep.env.model.evaluation <- env.evaluate(rep.species, thisrep.glm, env, n.background = env.nback)

        rts.geog.training[i] <- thisrep.model.evaluation@auc
        rts.env.training[i] <- thisrep.env.model.evaluation@auc

        if(test.prop > 0 & test.prop < 1){
          thisrep.test.evaluation <-dismo::evaluate(rep.test.data, rep.species$background.points,
                                                    thisrep.glm, env, na.rm = TRUE)
          temp.sp <- rep.species
          temp.sp$presence.points <- terra::vect(rep.test.data, geom = c("x", "y"), crs = terra::crs(species$presence.points))
          thisrep.env.test.evaluation <- env.evaluate(temp.sp, thisrep.glm, env, n.background = env.nback)

          rts.geog.test[i] <- thisrep.test.evaluation@auc
          rts.env.test[i] <- thisrep.env.test.evaluation@auc

          rts.models[[paste0("rep.",i)]] <- list(model = thisrep.glm,
                                                 training.evaluation = thisrep.model.evaluation,
                                                 env.training.evaluation = thisrep.env.model.evaluation,
                                                 test.evaluation = thisrep.test.evaluation,
                                                 env.test.evaluation = thisrep.env.test.evaluation)
        } else {
          rts.models[[paste0("rep.",i)]] <- list(model = thisrep.glm,
                                                 training.evaluation = thisrep.model.evaluation,
                                                 env.training.evaluation = thisrep.env.model.evaluation,
                                                 test.evaluation = NA,
                                                 env.test.evaluation = NA)
        }

      }

      # Reps are all run now, time to package it all up

      # Calculating p values
      rts.geog.training.pvalue = mean(rts.geog.training > model.evaluation@auc)
      rts.env.training.pvalue = mean(rts.env.training > env.model.evaluation@auc)
      if(test.prop > 0){
        rts.geog.test.pvalue <- mean(rts.geog.test > test.evaluation@auc)
        rts.env.test.pvalue <- mean(rts.env.test > env.test.evaluation@auc)
      } else {
        rts.geog.test.pvalue <- NA
        rts.env.test.pvalue <- NA
      }

      rts.geog.training <- data.frame(AUC = rts.geog.training)
      rts.env.training <- data.frame(AUC = rts.env.training)
      rts.geog.test <- data.frame(AUC = rts.geog.test)
      rts.env.test <- data.frame(AUC = rts.env.test)

      # Making plots
      training.plot <- ggplot(rts.geog.training, aes(x = .data$AUC, fill = "density", alpha = 0.5)) +
        geom_histogram(binwidth = 0.05) +
        geom_vline(xintercept = model.evaluation@auc, linetype = "longdash") +
        xlim(-0.05,1.05) + guides(fill = "none", alpha = "none") + xlab("AUC") +
        ggtitle(paste("Model performance in geographic space on training data")) +
        theme(plot.title = element_text(hjust = 0.5))

      env.training.plot <- ggplot(rts.env.training, aes(x = .data$AUC, fill = "density", alpha = 0.5)) +
        geom_histogram(binwidth = 0.05) +
        geom_vline(xintercept = env.model.evaluation@auc, linetype = "longdash") +
        xlim(-0.05,1.05) + guides(fill = "none", alpha = "none") + xlab("AUC") +
        ggtitle(paste("Model performance in environment space on training data")) +
        theme(plot.title = element_text(hjust = 0.5))

      # Make plots for test AUC distributions
      if(test.prop > 0){
        test.plot <- ggplot(rts.geog.test, aes(x = .data$AUC, fill = "density", alpha = 0.5)) +
          geom_histogram(binwidth = 0.05) +
          geom_vline(xintercept = test.evaluation@auc, linetype = "longdash") +
          xlim(-0.05,1.05) + guides(fill = "none", alpha = "none") + xlab("AUC") +
          ggtitle(paste("Model performance in geographic space on test data")) +
          theme(plot.title = element_text(hjust = 0.5))

        env.test.plot <- ggplot(rts.env.test, aes(x = .data$AUC, fill = "density", alpha = 0.5)) +
          geom_histogram(binwidth = 0.05) +
          geom_vline(xintercept = env.test.evaluation@auc, linetype = "longdash") +
          xlim(-0.05,1.05) + guides(fill = "none", alpha = "none") + xlab("AUC") +
          ggtitle(paste("Model performance in environment space on test data")) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        test.plot <- NA
        env.test.plot <- NA
      }

      rts.pvalues = list(rts.geog.training.pvalue = rts.geog.training.pvalue,
                         rts.env.training.pvalue = rts.env.training.pvalue,
                         rts.geog.test.pvalue = rts.geog.test.pvalue,
                         rts.env.test.pvalue = rts.env.test.pvalue)
      rts.distributions = list(rts.geog.training = rts.geog.training,
                               rts.env.training = rts.env.training,
                               rts.geog.test = rts.geog.test,
                               rts.env.test = rts.env.test)
      rts.plots = list(geog.training.plot = training.plot,
                       env.training.plot = env.training.plot,
                       geog.test.plot = test.plot,
                       env.test.plot = env.test.plot)

      rts.test <- list(rts.models = rts.models,
                       rts.pvalues = rts.pvalues,
                       rts.distributions = rts.distributions,
                       rts.plots = rts.plots,
                       rts.nreps = rts.reps)
    }

  }

  output <- list(species.name = species$species.name,
                 formula = f,
                 analysis.df = analysis.df,
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.glm,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 rts.test = rts.test,
                 suitability = suitability,
                 clamping.strength = clamping.strength,
                 call = sys.call(),
                 notes = notes)

  class(output) <- c("enmtools.glm", "enmtools.model")

  # Doing response plots for each variable.  Doing this bit after creating
  # the output object because marginal.plots expects an enmtools.model object
  response.plots <- list()

  plot.vars <- all.vars(formula(this.glm))

  for(i in 2:length(plot.vars)){
    this.var <-plot.vars[i]
    if(this.var %in% names(env)){
      response.plots[[this.var]] <- marginal.plots(output, env, this.var)
    }
  }

  output[["response.plots"]] <- response.plots

  if(!is.null(report)){
    if(file.exists(report) & overwrite == FALSE){
      stop("Report file exists, and overwrite is set to FALSE!")
    } else {
      # message("\n\nGenerating html report...\n")
      message("This function not enabled yet.  Check back soon!")
      # makereport(output, outfile = report)
    }
  }

  return(output)

}

# Summary for objects of class enmtools.glm
summary.enmtools.glm <- function(object, plot = TRUE, ...){

  cat("\n\nFormula:  ")
  cat(deparse(object$formula))

  cat("\n\nData table (top ten lines): ")
  print(kable(head(object$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(summary(object$model))

  cat("\n\nModel fit (training data):  ")
  print(object$training.evaluation)

  cat("\n\nEnvironment space model fit (training data):  ")
  print(object$env.training.evaluation)

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(object$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(object$test.evaluation)

  cat("\n\nEnvironment space model fit (test data):  ")
  print(object$env.test.evaluation)

  cat("\n\nSuitability:  \n")
  print(object$suitability)

  cat("\n\nNotes:  \n")
  object$notes

  if(plot) {
    plot(object)
  }

}

# Print method for objects of class enmtools.glm
print.enmtools.glm <- function(x, ...){

  print(summary(x, ...))

}


# Plot method for objects of class enmtools.glm
plot.enmtools.glm <- function(x, ...){


  suit.points <- data.frame(rasterToPoints2(x$suitability))
  colnames(suit.points) <- c("x", "y", "Suitability")
  test <- terra::as.data.frame(x$test.data, geom = "XY")

  suit.plot <- ggplot(data = suit.points,  aes(y = .data$y, x = .data$x)) +
    geom_raster(aes(fill = .data$Suitability)) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = x$analysis.df[x$analysis.df$presence == 1,],  aes(y = .data$y, x = .data$x),
               pch = 21, fill = "white", color = "black", size = 2)


  if(inherits(x$test.data, "SpatVector")){
    suit.plot <- suit.plot + geom_point(data = test,  aes(y = .data$y, x = .data$x),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }

  if(!is.na(x$species.name)){
    title <- paste("GLM model for", x$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }


  return(suit.plot)

}


# Predict method for models of class enmtools.glm
predict.enmtools.glm <- function(object, env, maxpts = 1000, clamp = TRUE, ...){

  # Make a plot of habitat suitability in the new region
  suitability <- terra::predict(env, object$model, type = "response", na.rm = TRUE)

  # Clamping and getting a diff layer
  clamping.strength <- NA
  if(clamp == TRUE){
    env <- clamp.env(object$analysis.df, env)
    clamped.suitability <- terra::predict(env, object$model, type = "response", na.rm = TRUE)
    clamping.strength <- clamped.suitability - suitability
    suitability <- clamped.suitability
  }

  suit.points <- data.frame(rasterToPoints2(suitability))
  colnames(suit.points) <- c("x", "y", "Suitability")

  suit.plot <- ggplot(data = suit.points,  aes(y = .data$y, x = .data$x)) +
    geom_raster(aes(fill = .data$Suitability)) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic()

  clamp.points <- data.frame(rasterToPoints2(clamping.strength))
  colnames(clamp.points) <- c("x", "y", "Clamping")

  clamp.plot <- ggplot(data = clamp.points,  aes(y = .data$y, x = .data$x)) +
    geom_raster(aes_string(fill = "Clamping")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic()

  if(!is.na(object$species.name)){
    title <- paste("GLM model projection for", object$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }

  this.threespace = threespace.plot(object, env, maxpts)

  output <- list(suitability.plot = suit.plot,
                 clamping.strength = clamping.strength,
                 suitability = suitability,
                 clamp.plot = clamp.plot,
                 threespace.plot = this.threespace)
  return(output)
}


# Function for checking data prior to running enmtools.glm
glm.precheck <- function(f, species, env){

  # Check to see if the function is the right class
  if(!inherits(f, "formula")){
    stop("Argument \'formula\' must contain an R formula object!")
  }

  ### Check to make sure the data we need is there
  if(!inherits(species, "enmtools.species")){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  check.species(species)

  if(!inherits(species$presence.points, "SpatVector")){
    stop("Species presence.points do not appear to be an object of class SpatVector")
  }

  if(!inherits(species$background.points, "SpatVector")){
    stop("Species background.points do not appear to be an object of class SpatVector")
  }

  if(!inherits(env, c("SpatRaster"))){
    stop("No environmental rasters were supplied!")
  }

}
