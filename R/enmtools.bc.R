#' Takes an emtools.species object with presence and background points, and builds a Bioclim model
#'
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param ... Arguments to be passed to bioclim()
#'
#' @export enmtools.bc
#' @export print.enmtools.bc
#' @export summary.enmtools.bc
#' @export plot.enmtools.bc

enmtools.bc <- function(species, env = NA, test.prop = 0, ...){

  species <- check.bg(species, env, ...)

  bc.precheck(species, env)

  test.data <- NA
  test.evaluation <- NA

  if(test.prop > 0 & test.prop < 1){
    test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
    test.data <- species$presence.points[test.inds,]
    species$presence.points <- species$presence.points[-test.inds,]
  }

  this.bc <- bioclim(env, species$presence.points[,1:2])

  suitability <- suitability <- predict(env, this.bc, type = "response")

  model.evaluation <- evaluate(species$presence.points[,1:2], species$background.points[,1:2],
                               this.bc, env)

  if(test.prop > 0 & test.prop < 1){
    test.evaluation <- evaluate(test.data, species$background.points[,1:2],
                                this.bc, env)
  }

  output <- list(analysis.df = species$presence.points[,1:2],
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.bc,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 suitability = suitability)

  class(output) <- c("enmtools.bc", "enmtools.model")

  # Doing response plots for each variable.  Doing this bit after creating
  # the output object because plot.response expects an enmtools.model object
  response.plots <- list()

  for(i in names(env)){
    response.plots[[i]] <- plot.response(output, env, i)
  }

  output[["response.plots"]] <- response.plots

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

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(this.bc$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(this.bc$test.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.bc$suitability)
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

  if(!inherits(env, c("raster", "RasterLayer", "RasterStack"))){
    stop("No environmental rasters were supplied!")
  }

  if(ncol(species$presence.points) != 2){
    stop("Species presence points do not contain longitude and latitude data!")
  }

}


