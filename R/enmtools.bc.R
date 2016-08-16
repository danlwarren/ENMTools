#' Takes an emtools.species object with presence and background points, and builds a Bioclim model
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param ... Arguments to be passed to bioclim()
#'
#' @export enmtools.bc
#' @export print.enmtools.bc
#' @export summary.enmtools.bc
#' @export plot.enmtools.bc

enmtools.bc <- function(species, env = NA, test.prop = 0, report = NULL, overwrite = FALSE, ...){

  notes <- NULL

  species <- check.bg(species, env, ...)

  bc.precheck(species, env)

  test.data <- NA
  test.evaluation <- NA
  env.test.evaluation <- NA

  if(test.prop > 0 & test.prop < 1){
    test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
    test.data <- species$presence.points[test.inds,]
    species$presence.points <- species$presence.points[-test.inds,]
  }

  # This is a very weird hack that has to be done because dismo's evaluate and bioclim function
  # fail if the stack only has one layer.
  if(length(names(env)) == 1){
    oldname <- names(env)
    env <- stack(env, env)
    env[[2]][!is.na(env[[2]])] <- 0
    names(env) <- c(oldname, "dummyvar")
    notes <- c(notes, "Only one predictor was provided, so a dummy variable was created in order to be compatible with dismo's prediction function.")
  }

  this.bc <- bioclim(env, species$presence.points[,1:2])

  suitability <- suitability <- predict(env, this.bc, type = "response")

  # This is a very weird hack that has to be done because dismo's evaluate function
  # fails if the stack only has one layer.
  if(length(names(env)) == 1){
    oldname <- names(env)
    env <- stack(env, env)
    names(env) <- c(oldname, "dummyvar")
  }

  model.evaluation <-dismo::evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                               this.bc, env)
  env.model.evaluation <- env.evaluate(species, this.bc, env)

  if(test.prop > 0 & test.prop < 1){
    test.evaluation <-dismo::evaluate(test.data, species$background.points[,1:2],
                                this.bc, env)
    temp.sp <- species
    temp.sp$presence.points <- test.data
    env.test.evaluation <- env.evaluate(temp.sp, this.bc, env)
  }

  output <- list(species.name = species$species.name,
                 analysis.df = species$presence.points[,1:2],
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.bc,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 suitability = suitability,
                 notes = notes)

  class(output) <- c("enmtools.bc", "enmtools.model")

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


# Summary for objects of class enmtools.bc
summary.enmtools.bc <- function(this.bc){

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.bc$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(this.bc$model)

  cat("\n\nModel fit (training data):  ")
  print(this.bc$training.evaluation)

  cat("\n\nEnvironment space model fit (training data):  ")
  print(this.bc$env.training.evaluation)

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(this.bc$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(this.bc$test.evaluation)

  cat("\n\nEnvironment space model fit (test data):  ")
  print(this.bc$env.test.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.bc$suitability)

  cat("\n\nNotes:  \n")
  print(this.bc$notes)

  #Note to self: plot command HAS to come last!
  plot(this.bc)

}

#Print method for objects of class enmtools.bc
print.enmtools.bc <- function(this.bc){

  print(summary(this.bc))

}

# Plot method for objects of class enmtools.bc
plot.enmtools.bc <- function(this.bc){


  suit.points <- data.frame(rasterToPoints(this.bc$suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = Suitability)) +
    scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = this.bc$analysis.df, aes(x = Longitude, y = Latitude),
               pch = 21, fill = "white", color = "black", size = 2)

  if(!(all(is.na(this.bc$test.data)))){
    suit.plot <- suit.plot + geom_point(data = this.bc$test.data, aes(x = Longitude, y = Latitude),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }

  return(suit.plot)

}

# Checking data for analysis using enmtools.bc
bc.precheck <- function(species, env, f){

  ### Check to make sure the data we need is there
  if(!inherits(species, "enmtools.species")){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  check.species(species)

  if(!inherits(species$presence.points, "data.frame")){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }

  if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
    stop("No environmental rasters were supplied!")
  }

  if(ncol(species$presence.points) != 2){
    stop("Species presence points do not contain longitude and latitude data!")
  }

}


