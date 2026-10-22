#' Takes an enmtools.species object with presence and background points, and builds
#' a fitted `tidymodels` workflow.
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data.
#' @param f Optional.  A formula or [recipes::recipe()] object specifying the model.  If NULL (the default), every layer in `env` is used as a predictor (as smooth terms when model = "gam").  For Bioclim, DOMAIN, and hypervolume models, a formula or recipe is used only to select predictors.
#' @param model A character string specifying the desired model, or a `parsnip`
#' model definition for any model with `mode = "classification"`. Default is
#' "glm". If a character string, choices are the standard ENMTools models:
#' `c("glm", "bc", "dm", "dm.dismo", "gam", "rf", "rf.ranger", "maxnet", "hypervolume")`
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
#' @param clamp Whether to clamp the environmental layers so that predictions made outside the min/max of the training data for each predictor are set to the value for the min/max for that predictor. Prevents the model from extrapolating beyond the min/max bounds of the predictor space the model was trained in, although there could still be projections outside the multivariate training space if predictors are strongly correlated.  Defaults to TRUE, except for presence-only envelope models (Bioclim, DOMAIN, and hypervolumes), whose suitability already declines outside the range of the presence points; these are not clamped unless clamp = TRUE, in which case they are clamped to the range of the presence points.
#' @param corner An integer from 1 to 4.  Selects which corner to use for "block" test data.  By default the corner is selected randomly.
#' @param needs.formula Some `parsnip` models require a formula to specify the structure of the data. If this is the case, set this to `TRUE` so that ENMTools knows to pass the original formula to the `tidymodels` workflow. You can safely ignore this if `model` is specified as a character string (e.g. a 'classic' ENMTools model)
#' @param bias An optional raster estimating relative sampling effort per grid cell.  Will be used for drawing background data.
#' @param model.args Arguments to be passed to the `parsnip` model specification. For 'classic' ENMTools models specified with a character string argument to `model`, these can be found using `get_parsnip_model(model)`. Arguments to the underlying engine used by the `parsnip` specification can also be specified here. These can be found under the individual model specifications for parsnip engines.
#' @param ... Additional arguments for future extensions. Not currently used.
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @export enmtools.tidy
#'
#' @examples
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' enmtools.tidy(iberolacerta.clade$species$monticola, env = euro.worldclim, f = pres ~ bio1 + bio9)



enmtools.tidy <- function(species, env, f = NULL, model = "glm", test.prop = 0, eval = TRUE, nback = 1000, env.nback = 10000, report = NULL, overwrite = FALSE, rts.reps = 0, weights = "equal", bg.source = "default",  verbose = FALSE, clamp = NULL, corner = NA, bias = NA, needs.formula = is.character(model) && model %in% c("gam"), model.args = list(), ...){

  notes <- NULL

  if("legacy" %in% c(names(list(...)), names(model.args))){
    stop("The legacy argument was removed in ENMTools 2.0.  All models are now fitted through tidymodels.", call. = FALSE)
  }

  if("step" %in% names(model.args)){
    warning("The step argument is deprecated: stepwise model selection is no longer available, and step is ignored.", call. = FALSE)
    model.args$step <- NULL
  }

  mod <- choose.model(model, model.args)

  if(is.null(clamp)){
    clamp <- default.clamp(mod)
  }

  if(is.envelope.model(mod) && !is.null(f)){
    f <- envelope.formula(f)
  }

  if(!case.weights.check(mod)) {
   weights <- "none"
  }

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
      test.inds <- get.block(species$presence.points, species$background.points)
      test.bg.inds <- which(test.inds$bg.grp == corner)
      test.inds <- which(test.inds$occs.grp == corner)
      test.data <- species$presence.points[test.inds,]
      test.bg <- species$background.points[test.bg.inds,]
      species$presence.points <- species$presence.points[-test.inds,]
      species$background.points <- species$background.points[-test.bg.inds,]
    }
  }

  wf <- workflows::workflow()
  ## check formula and construct tidymodels objects
  if(!is.null(f)) {
    if(inherits(f, "recipe")) {
      rec <- f
      wf <- workflows::add_recipe(wf, rec)
    } else {
      # Recast this formula so that the response variable is named "presence"
      # regardless of what was passed.
      f <- reformulate(attr(delete.response(terms(f)), "term.labels"), response = "presence")
      if(class(mod)[1] == "gen_additive_mod") {
        rec <- recipe(species, formula = f, env = env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias,
                      weights = weights)
        wf <- workflows::add_recipe(wf, rec)
      } else {
        wf <- workflows::add_formula(wf, f)
      }
    }
  } else {
    rec <- recipe(species, env = env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias,
                  weights = weights)
    wf <- workflows::add_recipe(wf, rec)
  }
  preps <- enmtools.prep(species, env = env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias, weights = weights)

  analysis.df <- preps$data
  species <- preps$species

  wf <- workflows::add_model(wf, mod)

  if(weights == "equal"){
    wf <- workflows::add_case_weights(wf, "weights")
  }

  if(needs.formula) {
    if(is.null(f)) {
      f <- make.formula(model, env, ...)
    }
    wf <- workflows::update_model(wf, mod, formula = f)
   }

  this.fit <- parsnip::fit(wf, data = analysis.df)

  suitability <- terra::predict(env, this.fit, type = "prob", na.rm = TRUE)$.pred_1

    # Clamping and getting a diff layer
  clamping.strength <- NA
  if(clamp == TRUE){
    env <- clamp.env(clamp.data(analysis.df, this.fit), env)
    clamped.suitability <- terra::predict(env, this.fit, type = "prob", na.rm = TRUE)$.pred_1
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

    model.evaluation <- dismo::evaluate(as.numeric(unlist(predict(this.fit, new_data = analysis.df[analysis.df$presence == 1, ], type = "prob")$.pred_1)),
                                        as.numeric(unlist(predict(this.fit, new_data = analysis.df[analysis.df$presence == 0, ], type = "prob")$.pred_1)))
    env.model.evaluation <- env.evaluate(species, this.fit, env, n.background = env.nback)

    # Test eval for randomly withheld data
    if(is.numeric(test.prop)){
      if(test.prop > 0 & test.prop < 1){
        test.data.check <- terra::extract(env, test.data, ID = FALSE)
        test.data.check <- test.data.check[complete.cases(test.data.check),]
        test.bg.check <- terra::extract(env, species$background.points, ID = FALSE)
        test.bg.check <- test.bg.check[complete.cases(test.bg.check),]
        test.evaluation <- dismo::evaluate(as.numeric(unlist(predict(this.fit, new_data = test.data.check, type = "prob")$.pred_1)),
                                           as.numeric(unlist(predict(this.fit, new_data = test.bg.check, type = "prob")$.pred_1)))
        temp.sp <- species
        temp.sp$presence.points <- test.data
        env.test.evaluation <- env.evaluate(temp.sp, this.fit, env, n.background = env.nback)
      }
    }

    # Test eval for spatially structured data
    if(is.character(test.prop)){
      if(test.prop == "block"){
        test.data.check <- terra::extract(env, test.data, ID = FALSE)
        test.data.check <- test.data.check[complete.cases(test.data.check),]
        test.bg.check <- terra::extract(env, test.bg, ID = FALSE)
        test.bg.check <- test.bg.check[complete.cases(test.bg.check),]
        test.evaluation <- dismo::evaluate(as.numeric(unlist(predict(this.fit, new_data = test.data.check, type = "prob")$.pred_1)),
                                           as.numeric(unlist(predict(this.fit, new_data = test.bg.check, type = "prob")$.pred_1)))

        temp.sp <- species
        temp.sp$presence.points <- test.data
        temp.sp$background.points <- test.bg
        env.test.evaluation <- env.evaluate(temp.sp, this.fit, env, n.background = env.nback)
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
          test <- cbind(test.data, test.data.check)
          allpoints <- rbind(test,
                             species$background.points,
                             species$presence.points)
        } else {
          allpoints <- rbind(species$background.points,
                             species$presence.points)
        }

        # Sample presence points from pool and remove from pool
        rep.rows <- sample(nrow(allpoints), nrow(species$presence.points))
        rep.species$presence.points <- allpoints[rep.rows,]
        allpoints <- allpoints[-rep.rows,]

        # Do the same for test points
        if(test.prop > 0){
          test.rows <- sample(nrow(allpoints), nrow(test.data))
          rep.test.data <- allpoints[test.rows,]
          allpoints <- allpoints[-test.rows,]
        }

        # Everything else goes back to the background
        rep.species$background.points <- allpoints

        rts.prep <- enmtools.prep(rep.species, nback = 0, weights = weights)
        rts.df <- rts.prep$data
        rep.species <- rts.prep$species

        thisrep.tidy <- parsnip::fit(wf, data = rts.df)

        thisrep.model.evaluation <-dismo::evaluate(as.numeric(unlist(predict(thisrep.tidy, new_data = rts.df[rts.df$presence == 1, ], type = "prob")$.pred_1)),
                                                    as.numeric(unlist(predict(thisrep.tidy, new_data = rts.df[rts.df$presence == 0, ], type = "prob")$.pred_1)))

        thisrep.env.model.evaluation <- env.evaluate(rep.species, thisrep.tidy, env, n.background = env.nback)

        rts.geog.training[i] <- thisrep.model.evaluation@auc
        rts.env.training[i] <- thisrep.env.model.evaluation@auc

        if(test.prop > 0 & test.prop < 1){
          temp.sp <- rep.species
          temp.sp$presence.points <- rep.test.data
          temp.sp.prep <- enmtools.prep(temp.sp, nback = 0, weights = weights)
          rep.test.data2 <- temp.sp.prep$data
          #temp.sp <- temp.sp.prep$species

          thisrep.test.evaluation <- dismo::evaluate(as.numeric(unlist(predict(thisrep.tidy, new_data = rep.test.data2[rep.test.data2$presence == 1, ], type = "prob")$.pred_1)),
                                                     as.numeric(unlist(predict(thisrep.tidy, new_data = rep.test.data2[rep.test.data2$presence == 0, ], type = "prob")$.pred_1)))

          thisrep.env.test.evaluation <- env.evaluate(temp.sp, thisrep.tidy, env, n.background = env.nback)

          rts.geog.test[i] <- thisrep.test.evaluation@auc
          rts.env.test[i] <- thisrep.env.test.evaluation@auc

          rts.models[[paste0("rep.",i)]] <- list(model = thisrep.tidy,
                                                 training.evaluation = thisrep.model.evaluation,
                                                 env.training.evaluation = thisrep.env.model.evaluation,
                                                 test.evaluation = thisrep.test.evaluation,
                                                 env.test.evaluation = thisrep.env.test.evaluation)
        } else {
          rts.models[[paste0("rep.",i)]] <- list(model = thisrep.tidy,
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
        geom_vline(xintercept = model.evaluation@auc, linetype = "longdash") +
        xlim(-0.05,1.05) + guides(fill = "none", alpha = "none") + xlab("AUC") +
        ggtitle(paste("Model performance in environment space on training data")) +
        theme(plot.title = element_text(hjust = 0.5))

      # Make plots for test AUC distributions
      if(test.prop > 0){
        test.plot <- ggplot(rts.geog.test, aes(x = .data$AUC, fill = "density", alpha = 0.5)) +
          geom_histogram(binwidth = 0.05) +
          geom_vline(xintercept = model.evaluation@auc, linetype = "longdash") +
          xlim(-0.05,1.05) + guides(fill = "none", alpha = "none") + xlab("AUC") +
          ggtitle(paste("Model performance in geographic space on test data")) +
          theme(plot.title = element_text(hjust = 0.5))

        env.test.plot <- ggplot(rts.env.test, aes(x = .data$AUC, fill = "density", alpha = 0.5)) +
          geom_histogram(binwidth = 0.05) +
          geom_vline(xintercept = model.evaluation@auc, linetype = "longdash") +
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
                 model = this.fit,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 rts.test = rts.test,
                 suitability = suitability,
                 clamping.strength = clamping.strength,
                 clamp = clamp,
                 call = sys.call(),
                 notes = notes)

  # Determine model-specific class based on model parameter
  model.class <- if (is.character(model)) {
    switch(model,
           maxnet = "enmtools.maxnet",
           hypervolume = "enmtools.hypervolume",
           hv = "enmtools.hypervolume",
           NULL)
  } else {
    NULL
  }

  # For hypervolume models, extract the hypervolume object for special methods
  if (!is.null(model.class) && model.class == "enmtools.hypervolume") {
    output$hv <- workflows::extract_fit_parsnip(this.fit)$fit
  }

  if (!is.null(model.class)) {
    class(output) <- c(model.class, "enmtools.tidy", "enmtools.model")
  } else {
    class(output) <- c("enmtools.tidy", "enmtools.model")
  }

  # Doing response plots for each variable.  Doing this bit after creating
  # the output object because marginal.plots expects an enmtools.model object
  response.plots <- list()

  form <- workflows::extract_preprocessor(this.fit)
  if(!inherits(form, "formula")) {
    form <- formula(recipes::prep(form))
  }
  plot.vars <- all.vars(form)

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

  #list(fit = fit, suitability = suitability)
}

#' Build a recipe from an `enmtools.species` object
#'
#' A [recipe][recipes::recipe()] is a description of the steps to be applied to a data set in order to prepare it for data analysis.
#' This function builds one for an `enmtools.species` object. Once built, any recipe steps can be added to it.
#' The recipe returned assumes a data.frame in the form used by ENMTools internally, which includes an
#' outcome variable called `"presence"`, specifying whether the data point is a presence point or a 'background' or
#' 'pseudo-absence' point. It also includes as predictor variables all environmental variables contained in
#' `env` unless a reduced set is specified in `formula`, in which case only the reduced set are available as predictors.
#' If you are unsure what variables are accessible for recipe steps, use [`enmtools.prep(x)$data`][enmtools.prep()], which returns the prepared
#' data used by ENMTools. The only exception are the variables "x" and "y", which will be present in the prepared data
#' but are currently not useable by recipe steps (these are the spatial coordinates of points).
#'
#' @param x An `enmtools.species` object
#' @param formula An R formula
#' @param ... Additional arguments. Not currently used.
#' @inheritParams enmtools.glm
#' @inheritParams recipes::recipe
#'
#' @inherit recipes::recipe return details
#' @export
#'
#' @examples
#' recipe(iberolacerta.clade$species$monticola, env = euro.worldclim)
recipe.enmtools.species <- function (x, formula = NULL, env, nback = 1000, bg.source = "default", verbose = FALSE, bias = NA, weights = "equal", ..., vars = NULL, roles = NULL) {
  x <- enmtools.prep(x, env = env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias, weights = weights)$data
  if(is.null(formula)) {
    vars <- colnames(x)
    vars <- vars[-which(vars %in% c("x", "y"))]
  } else {
    if(length(formula) > 2) {
      formula <- formula[-2]
    }
    vars <- c("presence", all.vars(formula), colnames(x)[colnames(x) == "weights"])
  }
  roles <- rep("predictor", length(vars))
  roles[vars == "presence"] <- "outcome"
  roles[vars == "weights"] <- "case_weights"
  recipes::recipe(x, vars = vars, roles = roles)
}

#' Prepare data for ENMTools models
#'
#' This is mostly used internally by ENMTools but we export it for users who want to
#' see how the sausage is made.
#'
#' @param x An `enmtools.species` object
#' @inheritParams enmtools.glm
#'
#' @return A list containing two elements: `data` and `species`. `data` contains a data.frame with formatted data suitable for modelling. `species` contains an updated `enmtools.species` object with background point information filled in.
#' @export
#'
#' @examples
#' enmtools.prep(iberolacerta.clade$species$monticola, euro.worldclim)
enmtools.prep <- function(x, env = NULL, nback = 1000, bg.source = "default", verbose = FALSE, bias = NA, weights = "none") {
  if(nback > 0) {
    species <- check.bg(x, env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias)
  } else {
    species <- x
  }
  if(!is.null(env)) {
    species <- add.env(species, env = env, verbose = verbose)
  }
  x <- make.analysis.df(species)
  if(weights == "equal"){
    weights <- c(rep(1, nrow(species$presence.points)),
                 rep(nrow(species$presence.points)/nrow(species$background.points),
                     nrow(species$background.points)))
    weights <- parsnip::importance_weights(weights)
    x$weights <- weights
  }
  x$presence <- as.factor(x$presence)
  list(data = x, species = species)
}

choose.model <- function(model, args = list(), ...) {
  if(inherits(model, "model_spec")) {
    if(length(args) > 0) {
      return(parsnip::set_args(model, !!!args))
    } else {
      return(model)
    }
  }
  m <- switch(model,
         glm = parsnip::logistic_reg(),
         gam = parsnip::gen_additive_mod(mode = "classification"),
         rf = parsnip::rand_forest(mode = "classification", engine = "randomForest"),
         `rf.ranger` = parsnip::rand_forest(mode = "classification"),
         bc = pres.only.sdm(),
         dm = pres.only.sdm(engine = "domain"),
         dm.dismo = pres.only.sdm(engine = "dismo.domain"),
         hv = pres.only.sdm(engine = "hypervolume"),
         hypervolume = pres.only.sdm(engine = "hypervolume"),
         maxnet = maxnet.sdm())
  if(length(args) > 0) {
    m <- parsnip::set_args(m, !!!args)
  }
  m
}

make.formula <- function(model, env, k = 4, ...) {
  if(!inherits(model, "model_spec")) {
    f <- switch(model,
           gam = as.formula(paste("presence", paste(unlist(lapply(names(env), FUN = function(x) paste0("s(", x, ", k = ", k, ")"))), collapse = " + "), sep = " ~ ")),
           NULL)
  } else {
    f <- NULL
  }
  f
}

#' Wrapper function for `dismo::bioclim()`
#'
#' Wraps [`dismo::bioclim()`] for use in a `parsnip` model specification.
#' Mostly for internal use. Exported so it works properly with parallel computation in `tidymodels`
#'
#' @param x Matrix or data.frame of environmental variables at points
#' @param y Single column matrix or data.frame containing a two-level factor, specifying whether each point is a 'presence' point or a 'background' point.
#'
#' @return The result of calling [`dismo::bioclim()`]
#' @export
#'
#' @examples
#' dat <- enmtools.prep(iberolacerta.clade$species$monticola, euro.worldclim)$data
#' bioclim.bridge(dat[ , c("bio1", "bio9")], dat[, "presence", drop = FALSE])
bioclim.bridge <- function(x, y) {
  dat <- check.pres.only(x, y)
  dismo::bioclim(dat)
}

#' Fit a DOMAIN model
#'
#' Fits a DOMAIN model as described by Carpenter et al. (1993), for use in a `parsnip` model specification.
#' The model stores the presence points and the range of each variable among them.  Suitability for a
#' site is its maximum similarity to any presence point, where similarity is one minus the Gower
#' distance: the range-standardized absolute difference, averaged across variables.  This differs from
#' [`dismo::domain()`], which averages the distance to all presence points for each variable separately
#' and then takes the minimum across variables; that version is available via [dismo.domain.bridge()].
#' Variables that do not vary among the presence points are not used, as in Gower (1971).
#' Mostly for internal use. Exported so it works properly with parallel computation in `tidymodels`
#'
#' @param x Matrix or data.frame of environmental variables at points
#' @param y Single column matrix or data.frame containing a two-level factor, specifying whether each point is a 'presence' point or a 'background' point.
#'
#' @return An object of class `enmtools.domain`
#' @references Carpenter, G., Gillison, A. N., and Winter, J. (1993) DOMAIN: a flexible modelling procedure for mapping potential distributions of plants and animals. Biodiversity and Conservation 2: 667-680.
#'
#' Gower, J. C. (1971) A general coefficient of similarity and some of its properties. Biometrics 27: 857-871.
#' @export
#'
#' @examples
#' dat <- enmtools.prep(iberolacerta.clade$species$monticola, euro.worldclim)$data
#' domain.bridge(dat[ , c("bio1", "bio9")], dat[, "presence", drop = FALSE])
domain.bridge <- function(x, y) {
  dat <- check.pres.only(x, y)
  var.range <- apply(dat, 2, function(v) diff(range(v, na.rm = TRUE)))
  if(any(var.range == 0)){
    warning(paste("These variables do not vary among presence points and will not be used by the DOMAIN model:",
                  paste(colnames(dat)[var.range == 0], collapse = ", ")))
  }
  keep <- var.range > 0
  output <- list(presence = dat[ , keep, drop = FALSE],
                 range = var.range[keep])
  class(output) <- "enmtools.domain"
  output
}

#' Predict from a DOMAIN model
#'
#' Calculates DOMAIN suitability (Carpenter et al. 1993) for new data: the maximum, over all presence
#' points, of one minus the range-standardized absolute difference averaged across variables.  Values
#' below zero, which occur for sites far outside the range of the presence points, are set to zero.
#' Mostly for internal use. Exported so it works properly with parallel computation in `tidymodels`
#'
#' @param object An object of class `enmtools.domain`, from [domain.bridge()]
#' @param newdata Matrix or data.frame of environmental variables at points
#'
#' @return Numeric vector of suitability scores between 0 and 1
#' @export
domain.predict <- function(object, newdata) {
  X <- as.matrix(newdata[ , colnames(object$presence), drop = FALSE])
  X <- sweep(X, 2, object$range, "/")
  pres <- sweep(object$presence, 2, object$range, "/")

  similarity <- rep(-Inf, nrow(X))
  for(i in 1:nrow(pres)){
    distance <- rowMeans(abs(sweep(X, 2, pres[i, ])))
    similarity <- pmax(similarity, 1 - distance)
  }

  pmax(similarity, 0)
}

#' Wrapper function for `dismo::domain()`
#'
#' Wraps [`dismo::domain()`] for use in a `parsnip` model specification.  Note that dismo's version
#' differs from the DOMAIN algorithm described by Carpenter et al. (1993); see [domain.bridge()].
#' Mostly for internal use. Exported so it works properly with parallel computation in `tidymodels`
#'
#' @param x Matrix or data.frame of environmental variables at points
#' @param y Single column matrix or data.frame containing a two-level factor, specifying whether each point is a 'presence' point or a 'background' point.
#'
#' @return The result of calling [`dismo::domain()`]
#' @export
#'
#' @examples
#' dat <- enmtools.prep(iberolacerta.clade$species$monticola, euro.worldclim)$data
#' dismo.domain.bridge(dat[ , c("bio1", "bio9")], dat[, "presence", drop = FALSE])
dismo.domain.bridge <- function(x, y) {
  dat <- check.pres.only(x, y)
  dismo::domain(dat)
}

#' Wrapper function for `maxnet::maxnet()`
#'
#' Wraps [`maxnet::maxnet()`] for use in a `parsnip` model specification.
#' Mostly for internal use. Exported so it works properly with parallel computation in `tidymodels`
#'
#' @param x Matrix or data.frame of environmental variables at points
#' @param y Single column matrix or data.frame containing a two-level factor
#' @param regmult Regularization multiplier passed to maxnet. Default 1.
#' @param classes Feature classes for maxnet formula. Default "default".
#' @param ... Additional arguments passed to maxnet::maxnet()
#'
#' @return The result of calling [`maxnet::maxnet()`]
#' @export
maxnet.bridge <- function(x, y, regmult = 1, classes = "default", ...) {
  # Convert factor to 0/1 vector
  # Level 2 is presence (coded as "1"), level 1 is background (coded as "0")
  # Handle both vector and single-column data.frame/matrix cases
  if (is.data.frame(y) || is.matrix(y)) {
    p <- as.integer(y[[1]]) - 1L
  } else {
    p <- as.integer(y) - 1L
  }

  # Ensure x is a data.frame
  data <- as.data.frame(x)

  # Build the maxnet formula
  f <- maxnet::maxnet.formula(p, data, classes = classes)

  # Fit the model
  maxnet::maxnet(p = p, data = data, f = f, regmult = regmult, ...)
}

#' Wrapper function for `hypervolume::hypervolume_gaussian()`
#'
#' Wraps [`hypervolume::hypervolume_gaussian()`] for use in a `parsnip` model specification.
#' Mostly for internal use. Exported so it works properly with parallel computation in `tidymodels`
#'
#' @param x Matrix or data.frame of environmental variables at points
#' @param y Single column matrix or data.frame containing a two-level factor
#' @param method Method for constructing hypervolumes: "gaussian" (default) or "svm".
#' @param samples.per.point Number of random samples per point for hypervolume estimation.
#' @param reduction.factor Value between 0 and 1 for prediction speed. Default 0.5.
#' @param ... Additional arguments passed to hypervolume construction function.
#'
#' @return A Hypervolume object with scaling attributes attached
#' @export
hypervolume.bridge <- function(x, y, method = "gaussian", samples.per.point = 1000,
                               reduction.factor = 0.5, ...) {
  # Extract presence-only data (same as bioclim/domain)
  dat <- check.pres.only(x, y)

  # Standardize the data (hypervolume works better with standardized data)
  dat.scaled <- scale(dat)

  # Store scaling parameters as attributes for prediction
  center <- attr(dat.scaled, "scaled:center")
  scale.sd <- attr(dat.scaled, "scaled:scale")

  # Build the hypervolume
  if (method == "gaussian") {
    hv <- hypervolume::hypervolume_gaussian(
      dat.scaled,
      samples.per.point = samples.per.point,
      verbose = FALSE,
      ...
    )
  } else if (method == "svm") {
    hv <- hypervolume::hypervolume_svm(
      dat.scaled,
      samples.per.point = samples.per.point,
      verbose = FALSE,
      ...
    )
  } else {
    stop("method must be 'gaussian' or 'svm'")
  }

  # Attach scaling info and prediction settings for later use
  attr(hv, "scale.center") <- center
  attr(hv, "scale.scale") <- scale.sd
  attr(hv, "reduction.factor") <- reduction.factor

  hv
}

#' Prediction helper for hypervolume objects
#'
#' Helper function for predicting from hypervolume objects in the tidymodels framework.
#' Uses hypervolume_estimate_probability for continuous suitability values.
#'
#' @param object A Hypervolume object from hypervolume package
#' @param newdata Data frame or matrix of new points to predict
#'
#' @return Numeric vector of probability density estimates (normalized to max = 1)
#' @export
hypervolume.predict <- function(object, newdata) {
  # Get scaling parameters
  center <- attr(object, "scale.center")
  scale.sd <- attr(object, "scale.scale")
  reduction.factor <- attr(object, "reduction.factor")
  if (is.null(reduction.factor)) reduction.factor <- 0.5  # Default

  # Scale the new data using stored parameters
  newdata.scaled <- scale(as.matrix(newdata), center = center, scale = scale.sd)

  # Use probability estimation for continuous values
  result <- hypervolume::hypervolume_estimate_probability(
    hv = object,
    points = newdata.scaled,
    reduction.factor = reduction.factor,
    verbose = FALSE
  )

  # Normalize to [0, 1] range by dividing by max (if any non-zero values)
  max.val <- max(result, na.rm = TRUE)
  if (max.val > 0) {
    result <- result / max.val
  }

  result
}

check.pres.only <- function(x, y) {
  # if(ncol(y) > 1) {
  #   stop("pres.only.sdm can only accept an outcome with a single variable")
  # }
  # parsnip passes the outcome as a vector; it may also arrive as a one-column data frame
  if(is.data.frame(y)) {
    y <- y[[1]]
  }
  if(!is.factor(y) || nlevels(y) != 2) {
    stop("pres.only.sdm outcome must be a factor with a exactly two levels")
  }
  dat <- as.matrix(x[y == levels(y)[2], , drop = FALSE])
  dat
}

make.pres.only.sdm <- function() {
  parsnip::set_new_model("pres.only.sdm")
  parsnip::set_model_mode(model = "pres.only.sdm", mode = "classification")
  parsnip::set_model_engine(
    "pres.only.sdm",
    mode = "classification",
    eng = "bioclim"
  )
  parsnip::set_model_engine(
    "pres.only.sdm",
    mode = "classification",
    eng = "domain"
  )
  parsnip::set_model_engine(
    "pres.only.sdm",
    mode = "classification",
    eng = "dismo.domain"
  )
  parsnip::set_model_engine(
    "pres.only.sdm",
    mode = "classification",
    eng = "hypervolume"
  )
  parsnip::set_dependency("pres.only.sdm", eng = "bioclim", pkg = "dismo")
  parsnip::set_dependency("pres.only.sdm", eng = "dismo.domain", pkg = "dismo")
  parsnip::set_dependency("pres.only.sdm", eng = "hypervolume", pkg = "hypervolume")
  parsnip::set_dependency("pres.only.sdm", eng = "bioclim", pkg = "ENMTools")
  parsnip::set_dependency("pres.only.sdm", eng = "domain", pkg = "ENMTools")
  parsnip::set_dependency("pres.only.sdm", eng = "dismo.domain", pkg = "ENMTools")
  parsnip::set_dependency("pres.only.sdm", eng = "hypervolume", pkg = "ENMTools")

  parsnip::set_fit(
    model = "pres.only.sdm",
    eng = "bioclim",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "bioclim.bridge"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "pres.only.sdm",
    eng = "domain",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "domain.bridge"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "pres.only.sdm",
    eng = "dismo.domain",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "dismo.domain.bridge"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "pres.only.sdm",
    eng = "hypervolume",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "hypervolume.bridge"),
      defaults = list(method = "gaussian", samples.per.point = 1000, reduction.factor = 0.5)
    )
  )

  parsnip::set_encoding(
    model = "pres.only.sdm",
    eng = "bioclim",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_encoding(
    model = "pres.only.sdm",
    eng = "domain",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_encoding(
    model = "pres.only.sdm",
    eng = "dismo.domain",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_encoding(
    model = "pres.only.sdm",
    eng = "hypervolume",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  prob.info <-
    list(
      pre = NULL,
      post = function(x, object) {
        cnames <- paste0(".pred_", object$lvl)
        res <- data.frame(pred_0 = 1 - x, pred_1 = x)
        colnames(res) <- cnames
        res
      },
      func = c(pkg = "dismo", fun = "predict"),
      args =
        # These lists should be of the form:
        # {predict.class argument name} = {values provided from parsnip objects}
        list(
          # We don't want the first two arguments evaluated right now
          # since they don't exist yet. `type` is a simple object that
          # doesn't need to have its evaluation deferred.
          object = quote(object$fit),
          x = quote(new_data)
        )
    )

  parsnip::set_pred(
    model = "pres.only.sdm",
    eng = "bioclim",
    mode = "classification",
    type = "prob",
    value = prob.info
  )

  parsnip::set_pred(
    model = "pres.only.sdm",
    eng = "dismo.domain",
    mode = "classification",
    type = "prob",
    value = prob.info
  )

  # DOMAIN as described by Carpenter et al. (1993) uses its own predict function
  domain.prob.info <-
    list(
      pre = NULL,
      post = function(x, object) {
        cnames <- paste0(".pred_", object$lvl)
        res <- data.frame(pred_0 = 1 - x, pred_1 = x)
        colnames(res) <- cnames
        res
      },
      func = c(pkg = "ENMTools", fun = "domain.predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )

  parsnip::set_pred(
    model = "pres.only.sdm",
    eng = "domain",
    mode = "classification",
    type = "prob",
    value = domain.prob.info
  )

  # Hypervolume uses a custom predict function
  hypervolume.prob.info <-
    list(
      pre = NULL,
      post = function(x, object) {
        cnames <- paste0(".pred_", object$lvl)
        probs <- as.numeric(x)
        res <- data.frame(pred_0 = 1 - probs, pred_1 = probs)
        colnames(res) <- cnames
        res
      },
      func = c(pkg = "ENMTools", fun = "hypervolume.predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )

  parsnip::set_pred(
    model = "pres.only.sdm",
    eng = "hypervolume",
    mode = "classification",
    type = "prob",
    value = hypervolume.prob.info
  )

}

#' `parsnip` Model specification for presence-only species distribution models
#'
#' @details Engines available:
#' - [`bioclim`][dismo::bioclim()]
#' - [`domain`][dismo::domain()]
#' - [`hypervolume`][hypervolume::hypervolume()]
#'
#' @param mode A single character string for the type of model. The only possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine to use for fitting. Possible engines are listed below. The default for this model is "bioclim".
#' @inherit parsnip::logistic_reg return
#' @export
#'
#' @examples
#' pres.only.sdm()
pres.only.sdm <- function(mode = "classification", engine = "bioclim") {
    # Check for correct mode
    if (mode  != "classification") {
      stop("`mode` should be 'classification'")
    }


    # Save some empty slots for future parts of the specification
    new_model_spec(
      "pres.only.sdm",
      args = NULL,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
}

#' `parsnip` Model specification for maxnet species distribution models
#'
#' @details maxnet fits Maxent models using the glmnet package, providing a
#'   modern implementation of the Maxent algorithm without requiring Java.
#'
#' @param mode A single character string for the type of model.
#'   The only possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The only engine available is "maxnet".
#'
#' @inherit parsnip::logistic_reg return
#' @export
#'
#' @examples
#' maxnet.sdm()
maxnet.sdm <- function(mode = "classification", engine = "maxnet") {
  if (mode != "classification") {
    stop("`mode` should be 'classification'")
  }

  parsnip::new_model_spec(
    "maxnet.sdm",
    args = NULL,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

make.maxnet.sdm <- function() {
  parsnip::set_new_model("maxnet.sdm")
  parsnip::set_model_mode(model = "maxnet.sdm", mode = "classification")
  parsnip::set_model_engine(
    "maxnet.sdm",
    mode = "classification",
    eng = "maxnet"
  )

  parsnip::set_dependency("maxnet.sdm", eng = "maxnet", pkg = "maxnet")
  parsnip::set_dependency("maxnet.sdm", eng = "maxnet", pkg = "ENMTools")

  parsnip::set_fit(
    model = "maxnet.sdm",
    eng = "maxnet",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "maxnet.bridge"),
      defaults = list(regmult = 1, classes = "default")
    )
  )

  parsnip::set_encoding(
    model = "maxnet.sdm",
    eng = "maxnet",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # Prediction - returns probability from cloglog transformation
  # Note: maxnet has a predict.maxnet S3 method, so we use stats::predict
  maxnet.prob.info <-
    list(
      pre = NULL,
      post = function(x, object) {
        cnames <- paste0(".pred_", object$lvl)
        res <- data.frame(pred_0 = 1 - x, pred_1 = x)
        colnames(res) <- cnames
        res
      },
      func = c(pkg = "stats", fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(as.data.frame(new_data)),
        type = "cloglog",
        clamp = TRUE
      )
    )

  parsnip::set_pred(
    model = "maxnet.sdm",
    eng = "maxnet",
    mode = "classification",
    type = "prob",
    value = maxnet.prob.info
  )
}

#' Reexported functions from other packages
#'
#' [recipes::recipe()]
#'
#' @name recipe
#' @rdname reexports
NULL


#' @exportS3Method
summary.enmtools.tidy <- function(object, plot = TRUE, ...){

  cat("\n\nFormula or recipe:  ")
  print(object$formula)

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


#' @exportS3Method
print.enmtools.tidy <- function(x, ...){

  print(summary(x, ...))

}

#' @exportS3Method
plot.enmtools.tidy <- function(x, ...){

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
    title <- paste(model.label(x$model), "model for", x$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }


  return(suit.plot)

}


# Short name of the model type in a fitted workflow, for plot titles
model.label <- function(model){
  spec <- workflows::extract_spec_parsnip(model)
  engine <- spec$engine
  label <- switch(class(spec)[1],
                  pres.only.sdm = switch(engine,
                                         bioclim = "Bioclim",
                                         domain = "DOMAIN",
                                         dismo.domain = "DOMAIN (dismo)",
                                         hypervolume = "Hypervolume",
                                         engine),
                  maxnet.sdm = "Maxnet",
                  logistic_reg = if(engine == "glm") "GLM" else paste0("GLM (", engine, ")"),
                  gen_additive_mod = "GAM",
                  rand_forest = if(engine == "ranger") "Random forest (ranger)" else "Random forest",
                  NULL)
  if(is.null(label)){
    label <- paste0(class(spec)[1], " (", engine, ")")
  }
  label
}


# Predict method for models of class enmtools.tidy
predict.enmtools.tidy <- function(object, env, maxpts = 1000, clamp = NULL, ...){

  # Use the clamping setting the model was fitted with, unless told otherwise
  if(is.null(clamp)){
    clamp <- if(!is.null(object[["clamp"]])) object[["clamp"]] else default.clamp(object$model)
  }

  # Make a plot of habitat suitability in the new region
  suitability <- terra::predict(env, object$model, type = "prob", na.rm = TRUE)$.pred_1

  # Clamping and getting a diff layer
  clamping.strength <- NA
  if(clamp == TRUE){
    env <- clamp.env(clamp.data(object$analysis.df, object$model), env)
    clamped.suitability <- terra::predict(env, object$model, type = "prob", na.rm = TRUE)$.pred_1
    clamping.strength <- clamped.suitability - suitability
    suitability <- clamped.suitability
  }

  suit.points <- data.frame(rasterToPoints2(suitability))
  colnames(suit.points) <- c("x", "y", "Suitability")

  suit.plot <- ggplot(data = suit.points,  aes(y = .data$y, x = .data$x)) +
    geom_raster(aes(fill = .data$Suitability)) +
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic()

  clamp.plot <- NA
  if(clamp == TRUE){
    clamp.points <- data.frame(rasterToPoints2(clamping.strength))
    colnames(clamp.points) <- c("x", "y", "Clamping")

    clamp.plot <- ggplot(data = clamp.points,  aes(y = .data$y, x = .data$x)) +
      geom_raster(aes(fill = .data$Clamping)) +
      scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
      coord_fixed() + theme_classic()
  }

  if(!is.na(object$species.name)){
    title <- paste(model.label(object$model), "model projection for", object$species.name)
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


# Presence-only envelope models (Bioclim, DOMAIN, and hypervolumes), whose
# suitability already declines outside the range of the presence points
is.envelope.model <- function(model){
  spec <- if(inherits(model, "workflow")) workflows::extract_spec_parsnip(model) else model
  inherits(spec, "pres.only.sdm")
}

# Envelope models use a formula or recipe only to select predictors, so
# transformations, interactions, and recipe steps are dropped
envelope.formula <- function(f){
  if(inherits(f, "recipe")){
    info <- summary(f)
    vars <- info$variable[info$role == "predictor"]
    transformed <- length(f$steps) > 0
  } else {
    labels <- attr(terms(f), "term.labels")
    vars <- all.vars(delete.response(terms(f)))
    transformed <- !setequal(labels, vars)
  }
  if(transformed){
    warning(paste("Bioclim, DOMAIN, and hypervolume models use the formula or recipe only to select predictors; transformations, interactions, and recipe steps are ignored.  Predictors used:",
                  paste(vars, collapse = ", ")), call. = FALSE)
  }
  reformulate(vars, response = "presence")
}

# Envelope models are not clamped by default; other models are
default.clamp <- function(model){
  !is.envelope.model(model)
}

# Data used to set clamping limits.  Envelope models are trained on presence
# points alone, so they are clamped to the range of the presences; other models
# are clamped to the range of presences and background.
clamp.data <- function(analysis.df, model){
  if(is.envelope.model(model)){
    return(analysis.df[analysis.df$presence == 1, , drop = FALSE])
  }
  analysis.df
}
