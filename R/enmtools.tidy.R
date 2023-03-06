#' Takes an enmtools.species object with presence and background points, and builds
#' a fitted `tidymodels` workflow.
#'
#' @param species An enmtools.species object
#' @param env A SpatRaster of environmental data.
#' @param f A formula or tidymodels recipe
#' @param model A character string specifying the desired model, or a `parsnip`
#' model definition for any model with `mode = "classification"`. Default is
#' "glm". If a character string, choices are the standard ENMTools models:
#' `c("glm", "bc", "dm", "gam", "rf", "rf.ranger", "maxent")`
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
#' @param ... Additional arguments to be passed to the modelling function
#'
#' @return An enmtools model object containing species name, model formula (if any), model object, suitability raster, marginal response plots, and any evaluation objects that were created.
#'
#' @examples
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' enmtools.tidy(iberolacerta.clade$species$monticola, env = euro.worldclim, f = pres ~ bio1 + bio9)



enmtools.tidy <- function(species, env, f = NULL, model = "glm", test.prop = 0, eval = TRUE, nback = 1000, env.nback = 10000, report = NULL, overwrite = FALSE, rts.reps = 0, weights = "equal", bg.source = "default",  verbose = FALSE, clamp = TRUE, corner = NA, bias = NA, step = FALSE, model_args = list(), ...){

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

  ## check formula and construct tidymodels objects
  if(!is.null(f)) {
    if(inherits(f, "recipe")) {
      rec <- f
    } else {
      rec <- recipe(species, f, env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias,
                    weights = weights)
    }
  } else {
    rec <- recipe(species, env = env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias,
                  weights = weights)
  }
  preps <- enmtools.prep(species, env = env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias, weights = weights)

  analysis.df <- preps$data
  species <- preps$species

  wf <- workflows::workflow(rec, mod)

  if(weights == "equal"){
    wf <- workflows::add_case_weights(wf, .data$weights)
  }

  f2 <- make_formula(model, env, ...)
  if(!is.null(f2)) {
    f <- f2
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

        rts.prep <- enmtools.prep(rep.species, env, nback = 0, weights = weights)
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
          temp.sp$presence.points <- terra::vect(rep.test.data, geom=c("x", "y"), crs = terra::crs(species$presence.points))
          temp.sp.prep <- enmtools.prep(temp.sp, env, nback = 0, weights = weights)
          rep.test.data2 <- temp.sp.prep$data
          temp.sp <- temp.sp.prep$species

          thisrep.test.evaluation <-dismo::evaluate(as.numeric(unlist(predict(thisrep.tidy, new_data = rep.test.data2, type = "prob")$.pred_1)),
                                                    as.numeric(unlist(predict(thisrep.tidy, new_data = rts.df[rts.df$presence == 0, ], type = "prob")$.pred_1)))

          temp.sp <- rep.species
          temp.sp$presence.points <- terra::vect(rep.test.data, geom = c("x", "y"), crs = terra::crs(species$presence.points))
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

  class(output) <- c("enmtools.glm", "enmtools.model")

  # Doing response plots for each variable.  Doing this bit after creating
  # the output object because marginal.plots expects an enmtools.model object
  response.plots <- list()

  plot.vars <- all.vars(formula(recipes::prep(workflows::extract_preprocessor(this.fit))))

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

recipe.enmtools.species <- function (x, formula = NULL, env = NA, nback = 1000, bg.source = "default", verbose = FALSE, bias = NA, weights = "none", ..., vars = NULL, roles = NULL) {
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

enmtools.prep <- function(x, env = NA, nback = 1000, bg.source = "default", verbose = FALSE, bias = NA, weights = "none") {
  if(nback > 0) {
    species <- check.bg(x, env, nback = nback, bg.source = bg.source, verbose = verbose, bias = bias)
  } else {
    species <- x
  }
  species <- add.env(species, env = env, verbose = verbose)
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
         bc = pres_only_sdm())
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

bioclim_bridge <- function(x, y) {
  dismo::bioclim(as.matrix(x[y[[1]] == levels(y[[1]])[2], ]))
}

domain_bridge <- function(x, y) {
  dismo::domain(x[y == levels(y)[2], ])
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
  parsnip::set_dependency("pres_only_sdm", eng = "bioclim", pkg = "dismo")
  parsnip::set_dependency("pres_only_sdm", eng = "domain", pkg = "dismo")
  parsnip::set_dependency("pres_only_sdm", eng = "bioclim", pkg = "ENMTools")
  parsnip::set_dependency("pres_only_sdm", eng = "domain", pkg = "ENMTools")

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

}

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
