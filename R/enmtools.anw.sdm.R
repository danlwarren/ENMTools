#' Takes an emtools.species object with presence and background points, and builds an Available Niche Weighted SDM (ANWSDM)
#'
#' @param formula Standard formula applicable to \code{method}
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param method SDM method to use. Currently, only 'gam' and 'glm' are implemented.
#' @param standardise Should environmental data be standardised before model fitting? (Recommended)
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param eval Determines whether model evaluation should be done.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param k Dimension of the basis used to represent the smooth terms if \code{method="gam"}. Ignored otherwise. See documentation for \code{\link[mgcv]{s}} for details.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param ... Arguments to be passed to \code{method} function. e.g. parameter \code{k} to be passed to
#'
#' @export enmtools.anw.sdm


enmtools.anw.sdm <- function(species, env, f = NULL, method = "glm", standardise = TRUE, test.prop = 0, eval = TRUE, nback = 1000, k = 4, report = NULL, overwrite = FALSE, ...){

  notes <- NULL

  if(standardise) {
    message("standardising environmental variables...")
    values(env) <- scale(values(env))
  }
  species <- check.bg(species, env, nback = nback)

  # Builds a default formula using all env
  if(is.null(f)){
    if(method == "glm") {
      f <- as.formula(paste("presence", paste(c(names(env)), collapse = " + "), sep = " ~ "))
      notes <- c(notes, "No formula was provided, so a GLM formula was built automatically.")
    }
    if(method == "gam") {
      smoothers <- unlist(lapply(names(env), FUN = function(x) paste0("s(", x, ", k = ", k, ")")))
      f <- as.formula(paste("presence", paste(smoothers, collapse = " + "), sep = " ~ "))
      notes <- c(notes, "No formula was provided, so a GAM formula was built automatically")
    }
  }

  if(method == "glm") glm.precheck(f, species, env)
  if(method == "gam") gam.precheck(f, species, env)

  # Declaring some NAs in case we skip evaluation
  test.data <- NA
  model.evaluation <- NA
  env.model.evaluation <- NA
  test.evaluation <- NA
  env.test.evaluation <- NA

  if(test.prop > 0 & test.prop < 1){
    test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
    test.data <- species$presence.points[test.inds,]
    species$presence.points <- species$presence.points[-test.inds,]
  }

  # Calculate density of background points
  message("Calculating background point density in environmental space...")
  back.hyp <- env.species.hypervolume(species, env, use.background = TRUE, standardise = FALSE, show.plot = FALSE)

  ## add environmental data to species
  species <- add.env(species, env)
  ## calculate estimated density of points at each data.point
  message("Estimating expected density of presence points in environmental space...")
  pres.dens <- hypervolume_estimate_probability(back.hyp, species$presence.points[ , c(-1, -2)],
                                                set.edges.zero = FALSE )
  message("Estimating expected density of background points in environmental space...")
  back.dens <- hypervolume_estimate_probability(back.hyp, species$background.points[ , c(-1, -2)],
                                                set.edges.zero = FALSE )

  ## calculate weighting factor from estimated densities
  pres.wts <- 1/pres.dens
  back.wts <- 1/back.dens
  ## normalise weights
  pres.wts <- pres.wts / mean(pres.wts)
  back.wts <- back.wts / mean(back.wts)

  # Recast this formula so that the response variable is named "presence"
  # regardless of what was passed.
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"), response = "presence")

  analysis.df <- rbind(species$presence.points, species$background.points)
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))
  ## add weights
  wts <- c(pres.wts, back.wts)

  if(method == "glm") {
    this.anw.sdm <- glm(f, analysis.df[,-c(1,2)], family="quasibinomial", weights = wts, ...)
    #this.anw.sdm <- glm(f, analysis.df[,-c(1,2)], family="quasibinomial", weights = wts)
  }

  if(method == "gam") {
    this.anw.sdm <- gam(f, analysis.df[,-c(1,2)], family="binomial", ...)
    #this.anw.sdm <- gam(f, analysis.df[,-c(1,2)], family="binomial")
  }

  suitability <- predict(env, this.anw.sdm, type = "response")

  if(eval == TRUE){

    # This is a very weird hack that has to be done because dismo's evaluate function
    # fails if the stack only has one layer.
    if(length(names(env)) == 1){
      oldname <- names(env)
      env <- stack(env, env)
      names(env) <- c(oldname, "dummyvar")
      notes <- c(notes, "Only one predictor was provided, so a dummy variable was created in order to be compatible with dismo's prediction function.")
    }

    model.evaluation <-dismo::evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                                       this.anw.sdm, env)
    env.model.evaluation <- env.evaluate(species, this.anw.sdm, env)

    if(test.prop > 0 & test.prop < 1){
      test.evaluation <-dismo::evaluate(test.data, species$background.points[,1:2],
                                        this.anw.sdm, env)
      temp.sp <- species
      temp.sp$presence.points <- test.data
      env.test.evaluation <- env.evaluate(temp.sp, this.anw.sdm, env)
    }

  }

  output <- list(species.name = species$species.name,
                 formula = f,
                 analysis.df = analysis.df,
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.anw.sdm,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 suitability = suitability,
                 notes = notes)

  class(output) <- c("enmtools.anw.sdm", "enmtools.model")

  # Doing response plots for each variable.  Doing this bit after creating
  # the output object because plot.response expects an enmtools.model object
  response.plots <- list()

  for(i in names(env)){
    response.plots[[i]] <- plot.response(output, env, i)
  }

  output[["response.plots"]] <- response.plots

  if(!is.null(report)){
    if(file.exists(report) & overwrite == FALSE){
      stop("Report file exists, and overwrite is set to FALSE!")
    } else {
      cat("\n\nGenerating html report...\n")
      makereport(output, outfile = report)
    }
  }

  return(output)

}
