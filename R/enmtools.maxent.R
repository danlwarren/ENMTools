#' Takes an emtools.species object with presence and background points, and builds a maxent model
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold randomly for model evaluation, or "block" for spatially structured evaluation.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param rts.reps The number of replicates to do for a Raes and ter Steege-style test of significance
#' @param bg.source Source for drawing background points.  If "points", it just uses the background points that are already in the species object.  If "range", it uses the range raster.  If "env", it draws points at randome from the entire study area outlined by the first environmental layer.
#' @param ... Arguments to be passed to maxent()
#'
#' @export enmtools.maxent
#'
#' @examples
#' \dontrun{
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' enmtools.maxent(iberolacerta.clade$species$monticola, env = euro.worldclim)
#' }


enmtools.maxent <- function(species, env, test.prop = 0, nback = 1000, report = NULL, overwrite = FALSE, rts.reps = 0,  bg.source = "default", ...){

  notes <- NULL

  species <- check.bg(species, env, nback = nback, bg.source = bg.source)

  maxent.precheck(f, species, env)

  test.data <- NA
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
      corner <- ceiling(runif(1, 0, 4))
      test.inds <- get.block(species$presence.points, species$background.points)
      test.bg.inds <- which(test.inds$bg.grp == corner)
      test.inds <- which(test.inds$occ.grp == corner)
      test.data <- species$presence.points[test.inds,]
      test.bg <- species$background.points[test.bg.inds,]
      species$presence.points <- species$presence.points[-test.inds,]
      species$background.points <- species$presence.points[-test.bg.inds,]
    }
  }

  analysis.df <- rbind(species$presence.points, species$background.points)
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))

  # This is a very weird hack that has to be done because dismo's evaluate and maxent function
  # fail if the stack only has one layer.
  if(length(names(env)) == 1){
    oldname <- names(env)
    env <- stack(env, env)
    env[[2]][!is.na(env[[2]])] <- 0
    names(env) <- c(oldname, "dummyvar")
    notes <- c(notes, "Only one predictor was provided, so a dummy variable was created in order to be compatible with dismo's prediction function.")
  }

  this.mx <- dismo::maxent(env, p = analysis.df[analysis.df$presence == 1,1:2], a = analysis.df[analysis.df$presence == 0,1:2], ...)


  model.evaluation <-dismo::evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                               this.mx, env)
  env.model.evaluation <- env.evaluate(species, this.mx, env)

  # Test eval for randomly withheld data
  if(is.numeric(test.prop)){
    if(test.prop > 0 & test.prop < 1){
      test.evaluation <-dismo::evaluate(test.data, species$background.points[,1:2],
                                        this.mx, env)
      temp.sp <- species
      temp.sp$presence.points <- test.data
      env.test.evaluation <- env.evaluate(temp.sp, this.mx, env)
    }
  }

  # Test eval for spatially structured data
  if(is.character(test.prop)){
    if(test.prop == "block"){
      test.evaluation <-dismo::evaluate(test.data, test.bg,
                                        this.mx, env)
      temp.sp <- species
      temp.sp$presence.points <- test.data
      temp.sp$background.points <- test.bg
      env.test.evaluation <- env.evaluate(temp.sp, this.mx, env)
    }
  }

  # Do Raes and ter Steege test for significance.  Turned off if eval == FALSE
  if(rts.reps > 0){

    # Die if we're not doing randomly withheld test data and RTS reps > 0
    if(!is.numeric(test.prop)){
      stop(paste("RTS test can only be conducted with randomly withheld data, and test.prop is set to", test.prop))
    }

    rts.models <- list()

    rts.geog.training <- c()
    rts.geog.test <- c()
    rts.env.training <- c()
    rts.env.test <- c()

    for(i in 1:rts.reps){
      print(paste("Replicate", i, "of", rts.reps))

      # Repeating analysis with scrambled pa points and then evaluating models
      rep.species <- species

      # Mix the points all together
      allpoints <- rbind(test.data, species$background.points[,1:2], species$presence.points[,1:2])

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

      rep.species <- add.env(rep.species, env, verbose = FALSE)

      rts.df <- rbind(rep.species$presence.points, rep.species$background.points)
      rts.df$presence <- c(rep(1, nrow(rep.species$presence.points)), rep(0, nrow(rep.species$background.points)))

      thisrep.mx <- dismo::maxent(env, p = rts.df[rts.df$presence == 1,1:2], a = rts.df[rts.df$presence == 0,1:2], ...)

      thisrep.model.evaluation <-dismo::evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                                                 thisrep.mx, env)
      thisrep.env.model.evaluation <- env.evaluate(species, thisrep.mx, env)

      rts.geog.training[i] <- thisrep.model.evaluation@auc
      rts.env.training[i] <- thisrep.env.model.evaluation@auc

      if(test.prop > 0 & test.prop < 1){
        thisrep.test.evaluation <-dismo::evaluate(rep.test.data, rep.species$background.points[,1:2],
                                                  thisrep.mx, env)
        temp.sp <- rep.species
        temp.sp$presence.points <- test.data
        thisrep.env.test.evaluation <- env.evaluate(temp.sp, thisrep.mx, env)

        rts.geog.test[i] <- thisrep.test.evaluation@auc
        rts.env.test[i] <- thisrep.env.test.evaluation@auc
      }
      rts.models[[paste0("rep.",i)]] <- list(model = thisrep.mx,
                                             training.evaluation = model.evaluation,
                                             env.training.evaluation = env.model.evaluation,
                                             test.evaluation = test.evaluation,
                                             env.test.evaluation = env.test.evaluation)
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


  suitability <- predict(env, this.mx, type = "response")


  output <- list(species.name = species$species.name,
                 analysis.df = analysis.df,
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.mx,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 rts.test = rts.test,
                 suitability = suitability,
                 notes = notes)

  class(output) <- c("enmtools.maxent", "enmtools.model")

  # Doing response plots for each variable.  Doing this bit after creating
  # the output object because marginal.plots expects an enmtools.model object
  response.plots <- list()

  for(i in names(env)){
    response.plots[[i]] <- marginal.plots(output, env, i)
  }

  output[["response.plots"]] <- response.plots

  if(!is.null(report)){
    if(file.exists(report) & overwrite == FALSE){
      stop("Report file exists, and overwrite is set to FALSE!")
    } else {
      # cat("\n\nGenerating html report...\n")
print("This function not enabled yet.  Check back soon!")
      # makereport(output, outfile = report)
    }
  }

  return(output)

}

# Summary for objects of class enmtools.maxent
summary.enmtools.maxent <- function(object, ...){

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
  print(object$notes)

  plot(object)

}

# Print method for objects of class enmtools.maxent
print.enmtools.maxent <- function(x, ...){

  summary(x)

}

# Plot method for objects of class enmtools.maxent
plot.enmtools.maxent <- function(x, ...){


  suit.points <- data.frame(rasterToPoints(x$suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points, aes_string(y = "Latitude", x = "Longitude")) +
    geom_raster(aes_string(fill = "Suitability")) +
    scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = x$analysis.df[x$analysis.df$presence ==1,],  aes_string(y = "Latitude", x = "Longitude"),
               pch = 21, fill = "white", color = "black", size = 2)

  if(!(all(is.na(x$test.data)))){
    suit.plot <- suit.plot + geom_point(data = x$test.data,  aes_string(y = "Latitude", x = "Longitude"),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }

  if(!is.na(x$species.name)){
    title <- paste("Maxent model for", x$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }


  return(suit.plot)

}


# Predict method for models of class enmtools.maxent
predict.enmtools.maxent <- function(object, env, maxpts = 1000, ...){

  # Make a plot of habitat suitability in the new region
  suitability <- raster::predict(env, object$model)
  suit.points <- data.frame(rasterToPoints(suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points,  aes_string(y = "Latitude", x = "Longitude")) +
    geom_raster(aes_string(fill = "Suitability")) +
    scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic()

  if(!is.na(object$species.name)){
    title <- paste("Maxent model projection for", object$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }

  this.threespace = threespace.plot(object, env, maxpts)

  output <- list(suitability = suit.plot,
                 threespace.plot = this.threespace)
  return(output)
}


# Function for checking data prior to running enmtools.maxent
maxent.precheck <- function(f, species, env){

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

