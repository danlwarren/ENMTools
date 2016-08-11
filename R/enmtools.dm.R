#' Takes an emtools.species object with presence and background points, and builds a Domain model
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param ... Arguments to be passed to bioclim()
#'
#' @export enmtools.dm
#' @export print.enmtools.dm
#' @export summary.enmtools.dm
#' @export plot.enmtools.dm

enmtools.dm <- function(species, env = NA, test.prop = 0, ...){

  notes <- NULL

  species <- check.bg(species, env, ...)

  dm.precheck(species, env)

  test.data <- NA
  test.evaluation <- NA
  env.test.evaluation <- NA

  if(test.prop > 0 & test.prop < 1){
    test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
    test.data <- species$presence.points[test.inds,]
    species$presence.points <- species$presence.points[-test.inds,]
  }

  # This is a very weird hack that has to be done because dismo's evaluate and domain function
  # fail if the stack only has one layer.
  if(length(names(env)) == 1){
    oldname <- names(env)
    env <- stack(env, env)
    env[[2]][!is.na(env[[2]])] <- 0
    names(env) <- c(oldname, "dummyvar")
    notes <- c(notes, "Only one predictor was provided, so a dummy variable was created in order to be compatible with dismo's prediction function.")
  }


  this.dm <- domain(env, species$presence.points[,1:2])

  # This is a very weird hack that has to be done because dismo's evaluate function
  # fails if the stack only has one layer.
  if(length(names(env)) == 1){
    oldname <- names(env)
    env <- stack(env, env)
    names(env) <- c(oldname, "dummyvar")
  }

  model.evaluation <- evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                               this.dm, env)
  env.model.evaluation <- env.evaluate(species, this.dm, env)

  if(test.prop > 0 & test.prop < 1){
    test.evaluation <- evaluate(test.data, species$background.points[,1:2],
                                this.dm, env)
    temp.sp <- species
    temp.sp$presence.points <- test.data
    env.test.evaluation <- env.evaluate(temp.sp, this.dm, env)
  }

  suitability <- predict(env, this.dm, type = "response")

  output <- list(species.names = species$species.name,
                 analysis.df = species$presence.points[,1:2],
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.dm,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 suitability = suitability,
                 notes = notes)

  class(output) <- c("enmtools.dm", "enmtools.model")

  # Doing response plots for each variable.  Doing this bit after creating
  # the output object because plot.response expects an enmtools.model object
  response.plots <- list()

  for(i in names(env)){
    response.plots[[i]] <- plot.response(output, env, i)
  }

  output[["response.plots"]] <- response.plots

  return(output)

}


# Summary for objects of class enmtools.dm
summary.enmtools.dm <- function(this.dm){

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.dm$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(this.dm$model)

  cat("\n\nModel fit (training data):  ")
  print(this.dm$training.evaluation)

  cat("\n\nEnvironment space model fit (training data):  ")
  print(this.dm$env.training.evaluation)

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(this.dm$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(this.dm$test.evaluation)

  cat("\n\nEnvironment space model fit (test data):  ")
  print(this.dm$env.test.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.dm$suitability)

  cat("\n\nNotes:  \n")
  print(this.dm$notes)

  plot(this.dm)

}


#Print method for objects of class enmtools.dm
print.enmtools.dm <- function(this.dm){

  print(summary(this.dm))

}

# Plot method for objects of class enmtools.dm
plot.enmtools.dm <- function(this.dm){

  suit.points <- data.frame(rasterToPoints(this.dm$suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = Suitability)) +
    scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability")) +
    coord_fixed() + theme_classic() +
    geom_point(data = this.dm$analysis.df, aes(x = Longitude, y = Latitude),
               pch = 21, fill = "white", color = "black", size = 2)

  if(!(all(is.na(this.dm$test.data)))){
    suit.plot <- suit.plot + geom_point(data = this.dm$test.data, aes(x = Longitude, y = Latitude),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }

  return(suit.plot)
}

# Checking data for analysis using enmtools.dm
dm.precheck <- function(species, env, f){

  ### Check to make sure the data we need is there
  if(!inherits(species, "enmtools.species")){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  check.species(species)

  if(!inherits(species$presence.points, "data.frame")){
    stop("Species presence.points do not appear to be an object of class data.frame")
  }

  if(!inherits(env, c("raster", "RasterLayer", "RasterStack"))){
    stop("No environmental rasters were supplied!")
  }

  if(ncol(species$presence.points) != 2){
    stop("Species presence points do not contain longitude and latitude data!")
  }
}


