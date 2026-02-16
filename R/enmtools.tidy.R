#' Takes an enmtools.species object with presence and background points, and builds
#' a fitted `tidymodels` workflow.
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data.
#' @param f A formula or tidymodels recipe
#' @param model A character string specifying the desired model, or a `parsnip`
#' model definition for any model with `mode = "classification"`. Default is
#' "glm". If a character string, choices are the standard ENMTools models:
#' `c("glm", "bc", "dm", "gam", "rf", "rf.ranger", "maxent", "tabpfn")`
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
#' @param needs_formula Some `parsnip` models require a formula to specify the structure of the data. If this is the case, set this to `TRUE` so that ENMTools knows to pass the original formula to the `tidymodels` workflow. You can safely ignore this if `model` is specified as a character string (e.g. a 'classic' ENMTools model)
#' @param bias An optional raster estimating relative sampling effort per grid cell.  Will be used for drawing background data.
#' @param model_args Arguments to be passed to the `parsnip` model specification. For 'classic' ENMTools models specified with a character string argument to `model`, these can be found using `get_parsnip_model(model)`. Arguments to the underlying engine used by the `parsnip` specification can also be specified here. These can be found under the individual model specifications for parsnip engines.
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



enmtools.tidy <- function(species, env, f = NULL, model = "glm", test.prop = 0, eval = TRUE, nback = 1000, env.nback = 10000, report = NULL, overwrite = FALSE, rts.reps = 0, weights = "equal", bg.source = "default",  verbose = FALSE, clamp = TRUE, corner = NA, bias = NA, needs_formula = is.character(model) && model %in% c("gam"), model_args = list(), ...){

  notes <- NULL

  mod <- choose_model(model, model_args)

  if(!case_weights_check(mod)) {
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

  if(needs_formula) {
    if(is.null(f)) {
      f <- make_formula(model, env, ...)
    }
    wf <- workflows::update_model(wf, mod, formula = f)
   }

  this.fit <- parsnip::fit(wf, data = analysis.df)

  suitability <- terra::predict(env, this.fit, type = "prob", na.rm = TRUE)$.pred_1

    # Clamping and getting a diff layer
  clamping.strength <- NA
  if(clamp == TRUE){
    env <- clamp.env(analysis.df, env)
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
                 call = sys.call(),
                 notes = notes)

  # Determine model-specific class based on model parameter
  model_class <- if (is.character(model)) {
    switch(model,
           maxnet = "enmtools.maxnet",
           hypervolume = "enmtools.hypervolume",
           hv = "enmtools.hypervolume",
           tabpfn = "enmtools.tabpfn",
           NULL)
  } else {
    NULL
  }

  # For hypervolume models, extract the hypervolume object for special methods
  if (!is.null(model_class) && model_class == "enmtools.hypervolume") {
    output$hv <- workflows::extract_fit_parsnip(this.fit)$fit
  }

  if (!is.null(model_class)) {
    class(output) <- c(model_class, "enmtools.tidy", "enmtools.model")
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
  x <- make_analysis.df(species)
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

choose_model <- function(model, args = list(), ...) {
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
         bc = pres_only_sdm(),
         dm = pres_only_sdm(engine = "domain"),
         hv = pres_only_sdm(engine = "hypervolume"),
         hypervolume = pres_only_sdm(engine = "hypervolume"),
         maxnet = maxnet_sdm(),
         tabpfn = tabpfn_sdm())
  if(length(args) > 0) {
    m <- parsnip::set_args(m, !!!args)
  }
  m
}

make_formula <- function(model, env, k = 4, ...) {
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
#' bioclim_bridge(dat[ , c("bio1", "bio9")], dat[, "presence", drop = FALSE])
bioclim_bridge <- function(x, y) {
  dat <- check_pres_only(x, y)
  dismo::bioclim(dat)
}

#' Wrapper function for `dismo::domain()`
#'
#' Wraps [`dismo::domain()`] for use in a `parsnip` model specification.
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
#' domain_bridge(dat[ , c("bio1", "bio9")], dat[, "presence", drop = FALSE])
domain_bridge <- function(x, y) {
  dat <- check_pres_only(x, y)
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
maxnet_bridge <- function(x, y, regmult = 1, classes = "default", ...) {
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
hypervolume_bridge <- function(x, y, method = "gaussian", samples.per.point = 1000,
                               reduction.factor = 0.5, ...) {
  # Extract presence-only data (same as bioclim/domain)
  dat <- check_pres_only(x, y)

  # Standardize the data (hypervolume works better with standardized data)
  dat_scaled <- scale(dat)

  # Store scaling parameters as attributes for prediction
  center <- attr(dat_scaled, "scaled:center")
  scale_sd <- attr(dat_scaled, "scaled:scale")

  # Build the hypervolume
  if (method == "gaussian") {
    hv <- hypervolume::hypervolume_gaussian(
      dat_scaled,
      samples.per.point = samples.per.point,
      verbose = FALSE,
      ...
    )
  } else if (method == "svm") {
    hv <- hypervolume::hypervolume_svm(
      dat_scaled,
      samples.per.point = samples.per.point,
      verbose = FALSE,
      ...
    )
  } else {
    stop("method must be 'gaussian' or 'svm'")
  }

  # Attach scaling info and prediction settings for later use
  attr(hv, "scale_center") <- center
  attr(hv, "scale_scale") <- scale_sd
  attr(hv, "reduction_factor") <- reduction.factor

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
hypervolume_predict <- function(object, newdata) {
  # Get scaling parameters
  center <- attr(object, "scale_center")
  scale_sd <- attr(object, "scale_scale")
  reduction_factor <- attr(object, "reduction_factor")
  if (is.null(reduction_factor)) reduction_factor <- 0.5  # Default

  # Scale the new data using stored parameters
  newdata_scaled <- scale(as.matrix(newdata), center = center, scale = scale_sd)

  # Use probability estimation for continuous values
  result <- hypervolume::hypervolume_estimate_probability(
    hv = object,
    points = newdata_scaled,
    reduction.factor = reduction_factor,
    verbose = FALSE
  )

  # Normalize to [0, 1] range by dividing by max (if any non-zero values)
  max_val <- max(result, na.rm = TRUE)
  if (max_val > 0) {
    result <- result / max_val
  }

  result
}

check_pres_only <- function(x, y) {
  # if(ncol(y) > 1) {
  #   stop("pres_only_sdm can only accept an outcome with a single variable")
  # }
  if(!is.factor(y[[1]]) || nlevels(y[[1]]) != 2) {
    stop("pres_only_sdm outcome must be a factor with a exactly two levels")
  }
  dat <- as.matrix(x[y[[1]] == levels(y[[1]])[2], ])
  dat
}

make_pres_only_sdm <- function() {
  parsnip::set_new_model("pres_only_sdm")
  parsnip::set_model_mode(model = "pres_only_sdm", mode = "classification")
  parsnip::set_model_engine(
    "pres_only_sdm",
    mode = "classification",
    eng = "bioclim"
  )
  parsnip::set_model_engine(
    "pres_only_sdm",
    mode = "classification",
    eng = "domain"
  )
  parsnip::set_model_engine(
    "pres_only_sdm",
    mode = "classification",
    eng = "hypervolume"
  )
  parsnip::set_dependency("pres_only_sdm", eng = "bioclim", pkg = "dismo")
  parsnip::set_dependency("pres_only_sdm", eng = "domain", pkg = "dismo")
  parsnip::set_dependency("pres_only_sdm", eng = "hypervolume", pkg = "hypervolume")
  parsnip::set_dependency("pres_only_sdm", eng = "bioclim", pkg = "ENMTools")
  parsnip::set_dependency("pres_only_sdm", eng = "domain", pkg = "ENMTools")
  parsnip::set_dependency("pres_only_sdm", eng = "hypervolume", pkg = "ENMTools")

  parsnip::set_fit(
    model = "pres_only_sdm",
    eng = "bioclim",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "bioclim_bridge"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "pres_only_sdm",
    eng = "domain",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "domain_bridge"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "pres_only_sdm",
    eng = "hypervolume",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "hypervolume_bridge"),
      defaults = list(method = "gaussian", samples.per.point = 1000, reduction.factor = 0.5)
    )
  )

  parsnip::set_encoding(
    model = "pres_only_sdm",
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
    model = "pres_only_sdm",
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
    model = "pres_only_sdm",
    eng = "hypervolume",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  prob_info <-
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
    model = "pres_only_sdm",
    eng = "bioclim",
    mode = "classification",
    type = "prob",
    value = prob_info
  )

  parsnip::set_pred(
    model = "pres_only_sdm",
    eng = "domain",
    mode = "classification",
    type = "prob",
    value = prob_info
  )

  # Hypervolume uses a custom predict function
  hypervolume_prob_info <-
    list(
      pre = NULL,
      post = function(x, object) {
        cnames <- paste0(".pred_", object$lvl)
        probs <- as.numeric(x)
        res <- data.frame(pred_0 = 1 - probs, pred_1 = probs)
        colnames(res) <- cnames
        res
      },
      func = c(pkg = "ENMTools", fun = "hypervolume_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )

  parsnip::set_pred(
    model = "pres_only_sdm",
    eng = "hypervolume",
    mode = "classification",
    type = "prob",
    value = hypervolume_prob_info
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
#' pres_only_sdm()
pres_only_sdm <- function(mode = "classification", engine = "bioclim") {
    # Check for correct mode
    if (mode  != "classification") {
      stop("`mode` should be 'classification'")
    }


    # Save some empty slots for future parts of the specification
    new_model_spec(
      "pres_only_sdm",
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
#' maxnet_sdm()
maxnet_sdm <- function(mode = "classification", engine = "maxnet") {
  if (mode != "classification") {
    stop("`mode` should be 'classification'")
  }

  parsnip::new_model_spec(
    "maxnet_sdm",
    args = NULL,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

make_maxnet_sdm <- function() {
  parsnip::set_new_model("maxnet_sdm")
  parsnip::set_model_mode(model = "maxnet_sdm", mode = "classification")
  parsnip::set_model_engine(
    "maxnet_sdm",
    mode = "classification",
    eng = "maxnet"
  )

  parsnip::set_dependency("maxnet_sdm", eng = "maxnet", pkg = "maxnet")
  parsnip::set_dependency("maxnet_sdm", eng = "maxnet", pkg = "ENMTools")

  parsnip::set_fit(
    model = "maxnet_sdm",
    eng = "maxnet",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "maxnet_bridge"),
      defaults = list(regmult = 1, classes = "default")
    )
  )

  parsnip::set_encoding(
    model = "maxnet_sdm",
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
  maxnet_prob_info <-
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
    model = "maxnet_sdm",
    eng = "maxnet",
    mode = "classification",
    type = "prob",
    value = maxnet_prob_info
  )
}

#' Wrapper function for TabPFN classifier
#'
#' Wraps the TabPFN Python package for use in a `parsnip` model specification.
#' Supports three backends: local pretrained, local finetuned, and cloud API.
#' Mostly for internal use. Exported so it works properly with parallel computation in `tidymodels`.
#'
#' @param x Matrix or data.frame of environmental variables at points
#' @param y Single column matrix or data.frame containing a two-level factor
#' @param backend Character. Either "local" or "api".
#' @param model_path Character. Model path: "auto" (default pretrained), "real" (real-data pretrained),
#'   a finetuned model name (e.g. "sdm-finetuned-nonspatial"), or a file path to a .pt checkpoint.
#' @param device Character. Device for computation: "auto", "cuda", or "cpu".
#' @param n_estimators Integer. Number of ensemble estimators.
#' @param softmax_temperature Numeric. Softmax temperature for predictions.
#' @param balance_probabilities Logical. Whether to balance class probabilities.
#' @param average_before_softmax Logical. Whether to average before softmax.
#' @param ensemble_subsamples Integer or NULL. Number of subsamples for API manual ensembling.
#' @param ... Additional arguments (ignored).
#'
#' @return A list with class "tabpfn_fit" containing the fitted model and metadata.
#' @export
tabpfn_bridge <- function(x, y, backend = "local", model_path = "auto",
                           device = "auto", n_estimators = 8L,
                           softmax_temperature = 0.9,
                           balance_probabilities = FALSE,
                           average_before_softmax = FALSE,
                           ensemble_subsamples = NULL, ...) {

  # Convert factor to 0/1 integer
  if (is.data.frame(y) || is.matrix(y)) {
    y_vec <- y[[1]]
  } else {
    y_vec <- y
  }
  y_int <- as.integer(y_vec) - 1L
  X <- as.matrix(x)

  # Determine if this is a finetuned model
  is_finetuned <- (!is.null(model_path) &&
                     (grepl("\\.pt$", model_path) ||
                        model_path %in% names(.tabpfn_models)))

  if (backend == "local") {
    reticulate::py_require("tabpfn")
    np <- reticulate::import("numpy")

    if (is_finetuned) {
      # Finetuned model path: use Python helper
      if (model_path %in% names(.tabpfn_models)) {
        model_path <- tabpfn_model_path(model_path)
      }

      py_helper <- reticulate::import_from_path(
        "tabpfn_sdm_predict",
        system.file("python", package = "ENMTools")
      )
      clf <- py_helper$load_finetuned_model(
        model_path,
        device = device,
        n_estimators = as.integer(n_estimators)
      )

      result <- list(
        clf = clf,
        py_helper = py_helper,
        X_train = X,
        y_train = y_int,
        backend = "finetuned"
      )

    } else {
      # Standard pretrained model
      tabpfn <- reticulate::import("tabpfn")

      clf_args <- list(
        device = device,
        n_estimators = as.integer(n_estimators),
        softmax_temperature = softmax_temperature,
        balance_probabilities = balance_probabilities,
        average_before_softmax = average_before_softmax,
        ignore_pretraining_limits = TRUE,
        random_state = 42L
      )

      if (model_path != "auto") {
        clf_args$model_path <- model_path
      }

      clf <- do.call(tabpfn$TabPFNClassifier, clf_args)
      clf$fit(reticulate::r_to_py(X), np$array(y_int))

      result <- list(
        clf = clf,
        backend = "local"
      )
    }

  } else if (backend == "api") {
    reticulate::py_require("tabpfn_client")
    tabpfn_client <- reticulate::import("tabpfn_client")
    np <- reticulate::import("numpy")

    token <- Sys.getenv("TABPFN_ACCESS_TOKEN")
    if (token == "") {
      stop("TabPFN API token not found. Set your token by adding to your .Renviron file:\n",
           "  TABPFN_ACCESS_TOKEN=your_token_here\n\n",
           "Get a token at: https://tabpfn.com",
           call. = FALSE)
    }
    tabpfn_client$config$set_access_token(token)

    clf <- tabpfn_client$TabPFNClassifier(
      model_path = "v2.5_large-samples",
      ignore_pretraining_limits = TRUE,
      random_state = 42L
    )

    if (!is.null(ensemble_subsamples)) {
      # Store training data for ensemble prediction
      result <- list(
        clf = clf,
        tabpfn_client = tabpfn_client,
        X_train = X,
        y_train = y_int,
        ensemble_subsamples = as.integer(ensemble_subsamples),
        backend = "api_ensemble"
      )
    } else {
      clf$fit(reticulate::r_to_py(X), np$array(y_int))
      result <- list(
        clf = clf,
        backend = "api"
      )
    }

  } else {
    stop("Unknown TabPFN backend: '", backend, "'. Use 'local' or 'api'.",
         call. = FALSE)
  }

  class(result) <- "tabpfn_fit"
  result
}

#' Prediction helper for TabPFN objects
#'
#' Helper function for predicting from TabPFN fit objects in the tidymodels framework.
#'
#' @param object A tabpfn_fit object
#' @param newdata Data frame or matrix of new points to predict
#'
#' @return Numeric vector of predicted presence probabilities.
#' @export
tabpfn_predict <- function(object, newdata) {
  np <- reticulate::import("numpy")
  newdata_mat <- as.matrix(newdata)

  if (object$backend == "local") {
    # Standard pretrained: direct predict_proba
    proba <- object$clf$predict_proba(reticulate::r_to_py(newdata_mat))
    proba <- as.matrix(proba)
    return(proba[, 2])

  } else if (object$backend == "finetuned") {
    # Finetuned: use Python helper with training data
    preds <- object$py_helper$predict_with_finetuned(
      object$clf,
      object$X_train,
      object$y_train,
      newdata_mat
    )
    return(as.numeric(preds))

  } else if (object$backend == "api") {
    # API without ensemble: direct predict_proba
    proba <- object$clf$predict_proba(reticulate::r_to_py(newdata_mat))
    proba <- as.matrix(proba)
    return(proba[, 2])

  } else if (object$backend == "api_ensemble") {
    # API with manual ensembling
    n_subs <- object$ensemble_subsamples
    X_train <- object$X_train
    y_train <- object$y_train

    pres_idx <- which(y_train == 1L)
    abs_idx <- which(y_train == 0L)

    all_preds <- matrix(0, nrow = nrow(newdata_mat), ncol = n_subs)

    for (i in seq_len(n_subs)) {
      # Subsample: all presences + random subset of absences
      sub_abs <- sample(abs_idx, min(length(abs_idx), length(pres_idx)), replace = FALSE)
      sub_idx <- c(pres_idx, sub_abs)

      sub_clf <- object$tabpfn_client$TabPFNClassifier(
        model_path = "v2.5_large-samples",
        ignore_pretraining_limits = TRUE,
        random_state = as.integer(42L + i)
      )
      sub_clf$fit(
        reticulate::r_to_py(X_train[sub_idx, , drop = FALSE]),
        np$array(y_train[sub_idx])
      )
      proba <- as.matrix(sub_clf$predict_proba(reticulate::r_to_py(newdata_mat)))
      all_preds[, i] <- proba[, 2]
    }

    return(rowMeans(all_preds))

  } else {
    stop("Unknown TabPFN backend in predict: '", object$backend, "'",
         call. = FALSE)
  }
}

#' `parsnip` Model specification for TabPFN species distribution models
#'
#' @details TabPFN is a pre-trained tabular foundation model that can be used
#'   for species distribution modeling. Supports local pretrained, local
#'   finetuned, and cloud API backends.
#'
#' @param mode A single character string for the type of model.
#'   The only possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The only engine available is "tabpfn".
#'
#' @inherit parsnip::logistic_reg return
#' @export
tabpfn_sdm <- function(mode = "classification", engine = "tabpfn") {
  if (mode != "classification") {
    stop("`mode` should be 'classification'")
  }

  parsnip::new_model_spec(
    "tabpfn_sdm",
    args = NULL,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

make_tabpfn_sdm <- function() {
  parsnip::set_new_model("tabpfn_sdm")
  parsnip::set_model_mode(model = "tabpfn_sdm", mode = "classification")
  parsnip::set_model_engine(
    "tabpfn_sdm",
    mode = "classification",
    eng = "tabpfn"
  )

  parsnip::set_dependency("tabpfn_sdm", eng = "tabpfn", pkg = "reticulate")
  parsnip::set_dependency("tabpfn_sdm", eng = "tabpfn", pkg = "ENMTools")

  parsnip::set_fit(
    model = "tabpfn_sdm",
    eng = "tabpfn",
    mode = "classification",
    value = list(
      interface = "matrix",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ENMTools", fun = "tabpfn_bridge"),
      defaults = list(
        backend = "local",
        model_path = "auto",
        device = "auto",
        n_estimators = 8L,
        softmax_temperature = 0.9,
        balance_probabilities = FALSE,
        average_before_softmax = FALSE,
        ensemble_subsamples = NULL
      )
    )
  )

  parsnip::set_encoding(
    model = "tabpfn_sdm",
    eng = "tabpfn",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  tabpfn_prob_info <-
    list(
      pre = NULL,
      post = function(x, object) {
        cnames <- paste0(".pred_", object$lvl)
        probs <- as.numeric(x)
        res <- data.frame(pred_0 = 1 - probs, pred_1 = probs)
        colnames(res) <- cnames
        res
      },
      func = c(pkg = "ENMTools", fun = "tabpfn_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )

  parsnip::set_pred(
    model = "tabpfn_sdm",
    eng = "tabpfn",
    mode = "classification",
    type = "prob",
    value = tabpfn_prob_info
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

    # Need to change this to reflect the actual model type
    title <- paste("GLM model for", x$species.name)
    suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }


  return(suit.plot)

}


# Predict method for models of class enmtools.tidy
predict.enmtools.tidy <- function(object, env, maxpts = 1000, clamp = TRUE, ...){

  # Make a plot of habitat suitability in the new region
  suitability <- terra::predict(env, object$model, type = "prob", na.rm = TRUE)

  # Clamping and getting a diff layer
  clamping.strength <- NA
  if(clamp == TRUE){
    env <- clamp.env(object$analysis.df, env)
    clamped.suitability <- terra::predict(env, object$model, type = "prob", na.rm = TRUE)
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
