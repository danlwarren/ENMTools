#' Takes an emtools.species object with presence and background points, and builds a Point Process Model (with Lasso penalty)
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param f Standard R formula
#' @param test.prop Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.
#' @param eval Determines whether model evaluation should be done.  Turned on by default, but moses turns it off to speed things up.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param env.nback Number of points to draw from environment space for environment space discrimination metrics.
#' @param normalise Should the suitability of the model be normalised? If FALSE (the default), suitability is returned as the predicted number of presence points in each grid cell (occurrence density). If TRUE, occurrence densities are divided by the total predicted density, to give a value ranging from 0 to 1, which represents the proportion of the predicted density for a species that occurs in each grid cell.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param rts.reps The number of replicates to do for a Raes and ter Steege-style test of significance
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param verbose Controls printing of various messages progress reports.  Defaults to FALSE.
#' @param clamp When set to TRUE, clamps the environmental layers so that predictions made outside the min/max of the training data for each predictor are set to the value for the min/max for that predictor. Prevents the model from extrapolating beyond the min/max bounds of the predictor space the model was trained in, although there could still be projections outside the multivariate training space if predictors are strongly correlated.
#' @param ... Arguments to be passed to ppmlasso()
#'
#' @details This runs a \code{ppmlasso} model of a species' distribution. It is generally recommended that background points should be on a grid for this method, as the background points are considered 'quadrature' points, used to estimate an integral. If background points are not provided, the function will generate them on a grid, rather than randomly, as is more usual for other SDM methods.
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @examples
#' \dontrun{
#' install.extras(repos='http://cran.us.r-project.org')
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' enmtools.ppmlasso(iberolacerta.clade$species$monticola, env = euro.worldclim[[1:3]])
#' }


enmtools.ppmlasso <- function(species, env, f = NULL, test.prop = 0, eval = TRUE, nback = 1000, env.nback = 10000, normalise = FALSE, report = NULL, overwrite = FALSE, rts.reps = 0, bg.source = "default",  verbose = FALSE, clamp = TRUE, ...){

  check.packages("ppmlasso")

  notes <- NULL

  species <- check.bg(species, env, nback = nback, bg.source = bg.source, verbose = verbose)

  # Builds a default formula using all env
  if(is.null(f)){
    f <- as.formula(paste0("presence ~ poly(", paste(c(names(env)), collapse = ", "), ", degree = 2, raw = TRUE)"))
    notes <- c(notes, "No formula was provided, so a ppmlasso formula was built automatically.")
  }

  ppmlasso.precheck(f, species, env)

  # Declaring some NAs in case we skip evaluation
  test.data <- NA
  model.evaluation <- NA
  env.model.evaluation <- NA
  test.evaluation <- NA
  env.test.evaluation <- NA
  rts.test <- NA


  ### Add env data
  species <- add.env(species, env, verbose = verbose)

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
      corner <- ceiling(runif(1, 0, 4))
      test.inds <- ENMeval::get.block(species$presence.points, species$background.points)
      test.bg.inds <- which(test.inds$bg.grp == corner)
      test.inds <- which(test.inds$occ.grp == corner)
      test.data <- species$presence.points[test.inds,]
      test.bg <- species$background.points[test.bg.inds,]
      species$presence.points <- species$presence.points[-test.inds,]
      species$background.points <- species$presence.points[-test.bg.inds,]
    }
  }

  # Recast this formula so that the response variable is blank for ppmlasso function
  # regardless of what was passed
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"))

  analysis.df <- rbind(cbind(species$presence.points, Pres = 1),
                       cbind(species$background.points, Pres = 0))
  wts <- ppmlasso_weights(species$presence.points, species$background.points,
                          c("Longitude", "Latitude"))
  analysis.df <- cbind(analysis.df, wt = wts)

  #this.ppmlasso <- ppmlasso(f, coord = c("Longitude", "Latitude"), data = analysis.df)
  this.ppmlasso <- ppmlasso::ppmlasso(f, coord = c("Longitude", "Latitude"), data = analysis.df, ...)

  env_cell_area <- prod(res(env))
  p.fun <- function(object, newdata, ...) {
    ppmlasso::predict.ppmlasso(object, newdata = newdata, ...)*env_cell_area
  }

  suitability <- predict(env, this.ppmlasso, fun = p.fun)

  # Clamping and getting a diff layer
  clamping.strength <- NA
  if(clamp == TRUE){
    env <- clamp.env(analysis.df, env)
    clamped.suitability <- predict(env, this.ppmlasso, fun = p.fun)
    clamping.strength <- clamped.suitability - suitability
    suitability <- clamped.suitability
  }

  if(normalise) {
     suitability <- raster.standardize(suitability)
  }

  if(eval == TRUE){

    # This is a very weird hack that has to be done because dismo's evaluate function
    # fails if the stack only has one layer.
    if(length(names(env)) == 1){
      oldname <- names(env)
      env <- stack(env, env)
      names(env) <- c(oldname, "dummyvar")
      notes <- c(notes, "Only one predictor was provided, so a dummy variable was created in order to be compatible with dismo's prediction function.")
    }

    model.evaluation <- dismo::evaluate(ppmlasso::predict.ppmlasso(this.ppmlasso,
                                                         newdata = species$presence.points)[ , 1, drop = TRUE],
                                        ppmlasso::predict.ppmlasso(this.ppmlasso,
                                                         newdata = species$background.points)[ , 1, drop = TRUE])

    env.model.evaluation <- env.evaluate(species, this.ppmlasso, env, n.background = env.nback)

    # Test eval for randomly withheld data
    if(is.numeric(test.prop)){
      if(test.prop > 0 & test.prop < 1){
        test.check <- raster::extract(env, test.data[,1:2])
        test.data <- test.data[complete.cases(test.check),]
        test.evaluation <- dismo::evaluate(ppmlasso::predict.ppmlasso(this.ppmlasso,
                                                            newdata = test.data)[ , 1, drop = TRUE],
                                           ppmlasso::predict.ppmlasso(this.ppmlasso,
                                                            newdata = species$background.points)[ , 1, drop = TRUE])
        temp.sp <- species
        temp.sp$presence.points <- test.data
        env.test.evaluation <- env.evaluate(temp.sp, this.ppmlasso, env, n.background = env.nback)
      }
    }

    # Test eval for spatially structured data
    if(is.character(test.prop)){
      if(test.prop == "block"){
        test.check <- raster::extract(env, test.data[,1:2])
        test.data <- test.data[complete.cases(test.check),]
        test.evaluation <- dismo::evaluate(ppmlasso::predict.ppmlasso(this.ppmlasso,
                                                            newdata = test.data)[ , 1, drop = TRUE],
                                           ppmlasso::predict.ppmlasso(this.ppmlasso,
                                                            newdata = test.bg)[ , 1, drop = TRUE])
        temp.sp <- species
        temp.sp$presence.points <- test.data
        temp.sp$background.points <- test.bg
        env.test.evaluation <- env.evaluate(temp.sp, this.ppmlasso, env, n.background = env.nback)
      }
    }

    # Do Raes and ter Steege test for significance.  Turned off if eval == FALSE
    if(rts.reps > 0){

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
        allpoints <- rbind(test.data[,1:2], species$background.points[,1:2], species$presence.points[,1:2])

        # Sample presence points from pool and remove from pool
        rep.rows <- sample(nrow(allpoints), nrow(species$presence.points))
        rep.species$presence.points <- allpoints[rep.rows,]
        allpoints <- allpoints[-rep.rows,]

        # Do the same for test points
        if(test.prop > 0){
          test.rows <- sample(nrow(allpoints), nrow(species$presence.points))
          rep.test.data <- allpoints[test.rows,]
          allpoints <- allpoints[-test.rows,]
        }

        # Everything else goes back to the background
        rep.species$background.points <- allpoints

        rep.species <- add.env(rep.species, env, verbose = verbose)

        rts.df <- rbind(cbind(rep.species$presence.points, Pres = 1),
                             cbind(rep.species$background.points, Pres = 0))
        wts <- ppmlasso_weights(rep.species$presence.points, rep.species$background.points,
                                c("Longitude", "Latitude"))

        rts.df$wt <- wts
        capture.output(
          thisrep.ppmlasso <- ppmlasso::ppmlasso(f, coord = c("Longitude", "Latitude"), data = rts.df, ...)
        )

        # capture.output(
        #   thisrep.ppmlasso <- ppmlasso(f, coord = c("Longitude", "Latitude"), data = rts.df, ...)
        # )

        p.fun <- function(object, newdata, ...) {
          ppmlasso::predict.ppmlasso(object, newdata = newdata, ...)*env_cell_area
        }

        thisrep.model.evaluation <- dismo::evaluate(ppmlasso::predict.ppmlasso(thisrep.ppmlasso,
                                                                    newdata = rep.species$presence.points)[ , 1, drop = TRUE],
                                                   ppmlasso::predict.ppmlasso(thisrep.ppmlasso,
                                                                    newdata = rep.species$background.points)[ , 1, drop = TRUE])
        thisrep.env.model.evaluation <- env.evaluate(rep.species, thisrep.ppmlasso, env, n.background = env.nback)

        rts.geog.training[i] <- thisrep.model.evaluation@auc
        rts.env.training[i] <- thisrep.env.model.evaluation@auc

        # I need to double check whether RTS tested models on same test data as empirical
        # model, or whether they drew new holdouts for replicates.  Currently I'm just
        # using the same test data for each rep.
        if(test.prop > 0 & test.prop < 1){
          names <- c(colnames(rep.test.data), names(env))
          rep.test.data <- cbind(rep.test.data, extract(env, rep.test.data[,1:2]))
          colnames(rep.test.data) <- names
          rep.test.data <- rep.test.data[complete.cases(rep.test.data),]

          thisrep.test.evaluation <- dismo::evaluate(ppmlasso::predict.ppmlasso(thisrep.ppmlasso,
                                                                      newdata = rep.test.data)[ , 1, drop = TRUE],
                                                     ppmlasso::predict.ppmlasso(thisrep.ppmlasso,
                                                                      newdata = rep.species$background.points)[ , 1, drop = TRUE])
          temp.sp <- rep.species
          temp.sp$presence.points <- test.data
          thisrep.env.test.evaluation <- env.evaluate(temp.sp, thisrep.ppmlasso, env, n.background = env.nback)

          rts.geog.test[i] <- thisrep.test.evaluation@auc
          rts.env.test[i] <- thisrep.env.test.evaluation@auc
        }
        rts.models[[paste0("rep.",i)]] <- list(model = thisrep.ppmlasso,
                                               training.evaluation = thisrep.model.evaluation,
                                               env.training.evaluation = thisrep.env.model.evaluation,
                                               test.evaluation = thisrep.test.evaluation,
                                               env.test.evaluation = thisrep.env.test.evaluation)
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

      # Making plots
      training.plot <- qplot(rts.geog.training, geom = "histogram", fill = "density", alpha = 0.5) +
        geom_vline(xintercept = model.evaluation@auc, linetype = "longdash") +
        xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("AUC") +
        ggtitle(paste("Model performance in geographic space on training data")) +
        theme(plot.title = element_text(hjust = 0.5))

      env.training.plot <- qplot(rts.env.training, geom = "histogram", fill = "density", alpha = 0.5) +
        geom_vline(xintercept = env.model.evaluation@auc, linetype = "longdash") +
        xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("AUC") +
        ggtitle(paste("Model performance in environmental space on training data")) +
        theme(plot.title = element_text(hjust = 0.5))

      # Make plots for test AUC distributions
      if(test.prop > 0){
        test.plot <- qplot(rts.geog.test, geom = "histogram", fill = "density", alpha = 0.5) +
          geom_vline(xintercept = test.evaluation@auc, linetype = "longdash") +
          xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("AUC") +
          ggtitle(paste("Model performance in geographic space on test data")) +
          theme(plot.title = element_text(hjust = 0.5))

        env.test.plot <- qplot(rts.env.test, geom = "histogram", fill = "density", alpha = 0.5) +
          geom_vline(xintercept = env.test.evaluation@auc, linetype = "longdash") +
          xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("AUC") +
          ggtitle(paste("Model performance in environmental space on test data")) +
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

  ## rename Pres to presence for compatability with other enmtools functions
  colnames(analysis.df)[colnames(analysis.df) == "Pres"] <- "presence"

  output <- list(species.name = species$species.name,
                 formula = f,
                 analysis.df = analysis.df,
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.ppmlasso,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 rts.test = rts.test,
                 suitability = suitability,
                 clamping.strength = clamping.strength,
                 call = sys.call(),
                 notes = notes)

  class(output) <- c("enmtools.ppmlasso", "enmtools.model")

  # Doing response plots for each variable.  Doing this bit after creating
  # the output object because marginal.plots expects an enmtools.model object
  response.plots <- list()

  plot.vars <- all.vars(formula(this.ppmlasso))

  for(i in 1:length(plot.vars)){
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
summary.enmtools.ppmlasso <- function(object, ...){

  cat("\n\nFormula:  ")
  print(object$formula)

  cat("\n\nData table (top ten lines): ")
  print(kable(head(object$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(summary(object$model))

  # ppmlasso doesn't really have a pretty summary at the moment. Might have to come up with something ourselves
  # cat("\n\nModel fit (training data):  ")
  # print(object$training.evaluation)

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

  plot(object)

}

# Print method for objects of class enmtools.ppmlasso
print.enmtools.ppmlasso <- function(x, ...){

  print(summary(x))

}

# Plot method for objects of class enmtools.ppmlasso
plot.enmtools.ppmlasso <- function(x, trans_col = NULL, ...){

  suit.points <- data.frame(rasterToPoints(x$suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points,  aes_string(y = "Latitude", x = "Longitude")) +
    geom_raster(aes_string(fill = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = x$analysis.df[x$analysis.df$presence == 1,],  aes_string(y = "Latitude", x = "Longitude"),
               pch = 21, fill = "white", color = "black", size = 2)

  if(!(all(is.na(x$test.data)))){
    suit.plot <- suit.plot + geom_point(data = x$test.data,  aes_string(y = "Latitude", x = "Longitude"),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }
  if(!is.null(trans_col)) {
    suit.plot <- suit.plot + scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability"), trans = trans_col)
  } else {
    suit.plot <- suit.plot + scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability"))
  }

  if(!is.na(x$species.name)){
    title <- paste("PPM lasso model for", x$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }

  return(suit.plot)
}


# Predict method for models of class enmtools.ppmlasso
predict.enmtools.ppmlasso <- function(object, env, maxpts = 1000, clamp = TRUE, ...){

  env_cell_area <- prod(res(env))

  p.fun <- function(object, newdata, ...) {
    ppmlasso::predict.ppmlasso(object, newdata = newdata, ...)*env_cell_area
  }

  # Make a plot of habitat suitability in the new region
  suitability <- predict(env, object$model, fun = p.fun)

  # Clamping and getting a diff layer
  clamping.strength <- NA
  if(clamp == TRUE){
    env <- clamp.env(object$analysis.df, env)
    clamped.suitability <- predict(env, object$model, fun = p.fun)
    clamping.strength <- clamped.suitability - suitability
    suitability <- clamped.suitability
  }


  suit.points <- data.frame(rasterToPoints(suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points,  aes_string(y = "Latitude", x = "Longitude")) +
    geom_raster(aes_string(fill = "Suitability")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic()

  clamp.points <- data.frame(rasterToPoints(clamping.strength))
  colnames(clamp.points) <- c("Longitude", "Latitude", "Clamping")

  clamp.plot <- ggplot(data = clamp.points,  aes_string(y = "Latitude", x = "Longitude")) +
    geom_raster(aes_string(fill = "Clamping")) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic()

  if(!is.na(object$species.name)){
    title <- paste("ppmlasso model projection for", object$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }

  this.threespace = threespace.plot(object, env, maxpts)

  output <- list(suitability.plot = suit.plot,
                 suitability = suitability,
                 clamping.strength = clamping.strength,
                 clamp.plot = clamp.plot,
                 threespace.plot = this.threespace)
  return(output)
}


# Function for checking data prior to running enmtools.ppmlasso
ppmlasso.precheck <- function(f, species, env){

  # Check to see if the function is the right class
  if(!inherits(f, "formula")){
    stop("Argument \'formula\' must contain an R formula object!")
  }

  ### Check to make sure the data we need is there
  if(!inherits(species, "enmtools.species")){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  check.species(species)

  if(!inherits(species$presence.points, "data.frame")){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }

  if(!inherits(species$background.points, "data.frame")){
    stop("Species background.points do not appear to be an object of class data.frame")
  }

  if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
    stop("No environmental rasters were supplied!")
  }

  if(ncol(species$presence.points) != 2){
    stop("Species presence points do not contain longitude and latitude data!")
  }

  if(ncol(species$background.points) != 2){
    stop("Species background points do not contain longitude and latitude data!")
  }
}

# Helper function copied from ppmlasso (from which it was unexported) with permission of Author:
# Ian Renner
ppmlasso_weights <- function (sp.xy, quad.xy, coord = c("X", "Y"))
{
  sp.col = c(which(names(sp.xy) == coord[1]), which(names(sp.xy) ==
                                                      coord[2]))
  quad.col = c(which(names(quad.xy) == coord[1]), which(names(quad.xy) ==
                                                          coord[2]))
  X.inc = sort(unique(quad.xy[, quad.col[1]]))[2] - sort(unique(quad.xy[,
                                                                        quad.col[1]]))[1]
  Y.inc = sort(unique(quad.xy[, quad.col[2]]))[2] - sort(unique(quad.xy[,
                                                                        quad.col[2]]))[1]
  quad.0X = min(quad.xy[, quad.col[1]]) - floor(min(quad.xy[,
                                                            quad.col[1]])/X.inc) * X.inc
  quad.0Y = min(quad.xy[, quad.col[2]]) - floor(min(quad.xy[,
                                                            quad.col[2]])/Y.inc) * Y.inc
  X = c(sp.xy[, quad.col[1]], quad.xy[, quad.col[1]])
  Y = c(sp.xy[, quad.col[2]], quad.xy[, quad.col[2]])
  round.X = round((X - quad.0X)/X.inc) * X.inc
  round.Y = round((Y - quad.0Y)/Y.inc) * Y.inc
  round.id = paste(round.X, round.Y)
  round.table = table(round.id)
  wt = X.inc * Y.inc/as.numeric(round.table[match(round.id,
                                                  names(round.table))])
  wt
}

