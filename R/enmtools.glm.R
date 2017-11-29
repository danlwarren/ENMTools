#' Takes an emtools.species object with presence and background points, and builds a GLM
#'
#' @param formula Standard GLM formula
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param eval Determines whether model evaluation should be done.  Turned on by default, but moses turns it off to speed things up.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param ... Arguments to be passed to glm()
#'
#' @export enmtools.glm
#' @method print enmtools.glm
#' @method summary enmtools.glm
#' @method plot enmtools.glm
#'
#' @examples
#' data(euro.worldclim)
#' data(iberolacerta.clade)
#' enmtools.glm(iberolacerta.clade$species$monticola, env = euro.worldclim)



enmtools.glm <- function(species, env, f = NULL, test.prop = 0, eval = TRUE, nback = 1000, report = NULL, overwrite = FALSE, ...){

  notes <- NULL

  species <- check.bg(species, env, nback = nback)

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

  if(test.prop > 0 & test.prop < 1){
    test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
    test.data <- species$presence.points[test.inds,]
    species$presence.points <- species$presence.points[-test.inds,]
  }

  ### Add env data
  species <- add.env(species, env)

  # Recast this formula so that the response variable is named "presence"
  # regardless of what was passed.
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"), response = "presence")

  analysis.df <- rbind(species$presence.points, species$background.points)
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))

  this.glm <- glm(f, analysis.df[,-c(1,2)], family="binomial", ...)


  if(as.integer(this.glm$aic) == 2 * length(this.glm$coefficients)){
    notes <- c(notes, "AIC is 2x number of coefficients, indicating an uninformative model.  This often indicates that you have too many predictors for your number of data points.")
  }

  suitability <- predict(env, this.glm, type = "response")

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
                                 this.glm, env)
    env.model.evaluation <- env.evaluate(species, this.glm, env)

    if(test.prop > 0 & test.prop < 1){
      test.evaluation <-dismo::evaluate(test.data, species$background.points[,1:2],
                                  this.glm, env)
      temp.sp <- species
      temp.sp$presence.points <- test.data
      env.test.evaluation <- env.evaluate(temp.sp, this.glm, env)
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
                 suitability = suitability,
                 notes = notes)

  class(output) <- c("enmtools.glm", "enmtools.model")

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
      cat("\n\nGenerating html report...\n")
      makereport(output, outfile = report)
    }
  }

  return(output)

}

# Summary for objects of class enmtools.glm
summary.enmtools.glm <- function(this.glm){

  cat("\n\nFormula:  ")
  print(this.glm$formula)

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.glm$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(summary(this.glm$model))

  cat("\n\nModel fit (training data):  ")
  print(this.glm$training.evaluation)

  cat("\n\nEnvironment space model fit (training data):  ")
  print(this.glm$env.training.evaluation)

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(this.glm$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(this.glm$test.evaluation)

  cat("\n\nEnvironment space model fit (test data):  ")
  print(this.glm$env.test.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.glm$suitability)

  cat("\n\nNotes:  \n")
  this.glm$notes

  plot(this.glm)

}

# Print method for objects of class enmtools.glm
print.enmtools.glm <- function(this.glm){

  print(summary(this.glm))

}


# Plot method for objects of class enmtools.glm
plot.enmtools.glm <- function(this.glm){


  suit.points <- data.frame(rasterToPoints(this.glm$suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = Suitability)) +
    scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = this.glm$analysis.df[this.glm$analysis.df$presence == 1,], aes(x = Longitude, y = Latitude),
               pch = 21, fill = "white", color = "black", size = 2)

  if(!(all(is.na(this.glm$test.data)))){
    suit.plot <- suit.plot + geom_point(data = this.glm$test.data, aes(x = Longitude, y = Latitude),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }

  return(suit.plot)

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

