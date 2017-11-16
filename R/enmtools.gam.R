#' Takes an emtools.species object with presence and background points, and builds a gam
#'
#' @param formula Standard gam formula
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param k Dimension of the basis used to represent the smooth term.  See documentation for s() for details.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param ... Arguments to be passed to gam()
#'
#' @export enmtools.gam
#' @method print enmtools.gam
#' @method summary enmtools.gam
#' @method plot enmtools.gam


enmtools.gam <- function(species, env, f = NULL, test.prop = 0, k = 4, nback = 1000, report = NULL, overwrite = FALSE, ...){

  notes <- NULL

  species <- check.bg(species, env, nback = nback)

  # Builds a default formula using all env
  if(is.null(f)){
    smoothers <- unlist(lapply(names(env), FUN = function(x) paste0("s(", x, ", k = ", k, ")")))
    f <- as.formula(paste("presence", paste(smoothers, collapse = " + "), sep = " ~ "))
    notes <- c(notes, "No formula was provided, so a GAM formula was built automatically")
  }

  #print(f)

  gam.precheck(f, species, env)

  test.data <- NA
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

  this.gam <- gam(f, analysis.df[,-c(1,2)], family="binomial", ...)

  suitability <- predict(env, this.gam, type = "response")

  # This is a very weird hack that has to be done because dismo's evaluate function
  # fails if the stack only has one layer.
  if(length(names(env)) == 1){
    oldname <- names(env)
    env <- stack(env, env)
    names(env) <- c(oldname, "dummyvar")
    notes <- c(notes, "Only one predictor was provided, so a dummy variable was created in order to be compatible with dismo's prediction function.")
  }

  model.evaluation <- dismo::evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                               this.gam, env)
  env.model.evaluation <- env.evaluate(species, this.gam, env)


  if(test.prop > 0 & test.prop < 1){
    test.evaluation <- dismo::evaluate(test.data, species$background.points[,1:2],
                                this.gam, env)
    temp.sp <- species
    temp.sp$presence.points <- test.data
    env.test.evaluation <- env.evaluate(temp.sp, this.gam, env)
  }



  output <- list(species.name = species$species.name,
                 formula = f,
                 analysis.df = analysis.df,
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.gam,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 suitability = suitability,
                 notes = notes)

  class(output) <- c("enmtools.gam", "enmtools.model")

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

# Summary for objects of class enmtools.gam
summary.enmtools.gam <- function(this.gam){

  cat("\n\nFormula:  ")
  print(this.gam$formula)

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.gam$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(summary(this.gam$model))

  cat("\n\ngam.check results:  ")
  print(gam.check(this.gam$model))

  cat("\n\nModel fit (training data):  ")
  print(this.gam$training.evaluation)

  cat("\n\nEnvironment space model fit (training data):  ")
  print(this.gam$env.training.evaluation)

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(this.gam$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(this.gam$test.evaluation)

  cat("\n\nEnvironment space model fit (test data):  ")
  print(this.gam$env.test.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.gam$suitability)

  cat("\n\nNotes:  \n")
  print(this.gam$notes)

  plot(this.gam)
}

# Print method for objects of class enmtools.gam
print.enmtools.gam <- function(this.gam){

  print(summary(this.gam))

}


# Plot method for objects of class enmtools.gam
plot.enmtools.gam <- function(this.gam){

  suit.points <- data.frame(rasterToPoints(this.gam$suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = Suitability)) +
    scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = this.gam$analysis.df[this.gam$analysis.df$presence == 1,], aes(x = Longitude, y = Latitude),
               pch = 21, fill = "white", color = "black", size = 2)

  if(!(all(is.na(this.gam$test.data)))){
    suit.plot <- suit.plot + geom_point(data = this.gam$test.data, aes(x = Longitude, y = Latitude),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }

  return(suit.plot)

}

# Function for checking data prior to running enmtools.gam
gam.precheck <- function(f, species, env){

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

