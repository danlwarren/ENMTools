#' Takes an emtools.species object with presence and background points, and builds a Point Process Model (with Lasso penalty)
#'
#' @param formula Standard R formula
#' @param species An enmtools.species object
#' @param env A raster or raster stack of environmental data.
#' @param test.prop Proportion of data to withhold for model evaluation
#' @param eval Determines whether model evaluation should be done.  Turned on by default, but moses turns it off to speed things up.
#' @param nback Number of background points to draw from range or env, if background points aren't provided
#' @param back.accurate Should a more accurate method of generating background points be used, if background points aren't provided? If TRUE, this method produces a number of background points close to \code{nback}, but is much slower than the alternative method, which is fast but innaccurate.
#' @param normalise Should the suitability of the model be normalised? If FALSE (the default), suitability is returned as the predicted number of presence points in each grid cell (occurrence density). If TRUE, occurrence densities are divided by the total predicted density, to give a value ranging from 0 to 1, which represents the proportion of the predicted density for a species that occurs in each grid cell.
#' @param report Optional name of an html file for generating reports
#' @param overwrite TRUE/FALSE whether to overwrite a report file if it already exists
#' @param ... Arguments to be passed to ppmlasso()
#'
#' @details This runs a \code{ppmlasso} model of a species' distribution. It is generally recommended that background points should be on a grid for this method, as the background points are considered 'quadrature' points, used to estimate an integral. If background points are not provided, the function will generate them on a grid, rather than randomly, as is more usual for other SDM methods.
#'
#' @importFrom ppmlasso ppmlasso
#' @export enmtools.ppmlasso
#' @method print enmtools.ppmlasso
#' @method summary enmtools.ppmlasso
#' @method plot enmtools.ppmlasso


enmtools.ppmlasso <- function(species, env, f = NULL, test.prop = 0, eval = TRUE, nback = 10000, back.accurate = FALSE, normalise = FALSE, report = NULL, overwrite = FALSE, ...){

  notes <- NULL

  species <- check.bg.ppmlasso(species, env, nback = nback, back.accurate = back.accurate)

  # Builds a default formula using all env
  if(is.null(f)){
    f <- as.formula(paste0("presence ~ poly(", paste(c(names(env)), collapse = ", "), ", degree = 2, raw = TRUE)"))
    notes <- c(notes, "No formula was provided, so a GLM formula was built automatically.")
  }

  ppmlasso.precheck(f, species, env)

  # Declaring some NAs in case we skip evaluation
  test.data <- NA
  model.evaluation <- NA
  env.model.evaluation <- NA
  test.evaluation <- NA
  env.test.evaluation <- NA


  ### Add env data
  species <- add.env(species, env)

  if(test.prop > 0 & test.prop < 1){
    test.inds <- sample(1:nrow(species$presence.points), ceiling(nrow(species$presence.points) * test.prop))
    test.data <- species$presence.points[test.inds,]
    species$presence.points <- species$presence.points[-test.inds,]
  }

  # Recast this formula so that the response variable is blank for ppmlasso function
  # regardless of what was passed
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"))

  analysis.df <- rbind(cbind(species$presence.points, Pres = 1),
                       cbind(species$background.points, Pres = 0))
  wts <- ppmlasso_weights(species$presence.points, species$background.points,
                          c("Longitude", "Latitude"))
  analysis.df <- cbind(analysis.df, wt = wts)

  #this.ppmlasso <- ppmlasso(f, coord = c("Longitude", "Latitude"), data = analysis.df)
  this.ppmlasso <- ppmlasso(f, coord = c("Longitude", "Latitude"), data = analysis.df, ...)

  env_cell_area <- prod(res(env))
  p.fun <- function(object, newdata, ...) {
    predict.ppmlasso(object, newdata = newdata, ...)*env_cell_area
  }
  suitability <- predict(env, this.ppmlasso, fun = p.fun)

  if(normalise) {
    values(suitability) <- values(suitability) / sum(values(suitability), na.rm = TRUE)
  }

  if(eval == TRUE){

    # This is a very weird hack that has to be done because dismo's evaluate function
    # fails if the stack only has one layer.
    if(length(names(env)) == 1){
      oldname <- names(env)
      env <- stack(env, env)
      names(env) <- c(oldname, "dummyvar")
      notes <- c(notes, "Only one predictor was provided, so a dummy variable was created in order to be compatible with dismo's prediction function.")
    }

    model.evaluation <- dismo::evaluate(predict.ppmlasso(this.ppmlasso,
                                                         newdata = species$presence.points)[ , 1, drop = TRUE],
                                        predict.ppmlasso(this.ppmlasso,
                                                         newdata = species$background.points)[ , 1, drop = TRUE])

    env.model.evaluation <- env.evaluate(species, this.ppmlasso, env)

    if(test.prop > 0 & test.prop < 1){

      test.evaluation <- dismo::evaluate(predict.ppmlasso(this.ppmlasso,
                                                          newdata = test.data)[ , 1, drop = TRUE],
                                         predict.ppmlasso(this.ppmlasso,
                                                          newdata = species$background.points)[ , 1, drop = TRUE])
      temp.sp <- species
      temp.sp$presence.points <- test.data
      env.test.evaluation <- env.evaluate(temp.sp, this.ppmlasso, env)


    }

  }

  ## rename Pres to presence for compatability with other enmtools functions
  colnames(analysis.df)[colnames(analysis.df) == "Pres"] <- "presence"

  output <- list(species.name = species$species.name,
                 formula = f,
                 analysis.df = analysis.df,
                 test.data = test.data,
                 test.prop = test.prop,
                 model = this.ppmlasso,
                 training.evaluation = model.evaluation,
                 test.evaluation = test.evaluation,
                 env.training.evaluation = env.model.evaluation,
                 env.test.evaluation = env.test.evaluation,
                 suitability = suitability,
                 notes = notes)

  class(output) <- c("enmtools.ppmlasso", "enmtools.model")

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

# Summary for objects of class enmtools.glm
summary.enmtools.ppmlasso <- function(this.ppmlasso){

  cat("\n\nFormula:  ")
  print(this.ppmlasso$formula)

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.ppmlasso$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(summary(this.ppmlasso$model))

  # ppmlasso doesn't really have a pretty summary at the moment. Might have to come up with something ourselves
  # cat("\n\nModel fit (training data):  ")
  # print(this.ppmlasso$training.evaluation)

  cat("\n\nEnvironment space model fit (training data):  ")
  print(this.ppmlasso$env.training.evaluation)

  cat("\n\nProportion of data wittheld for model fitting:  ")
  cat(this.ppmlasso$test.prop)

  cat("\n\nModel fit (test data):  ")
  print(this.ppmlasso$test.evaluation)

  cat("\n\nEnvironment space model fit (test data):  ")
  print(this.ppmlasso$env.test.evaluation)

  cat("\n\nSuitability:  \n")
  print(this.ppmlasso$suitability)

  cat("\n\nNotes:  \n")
  this.ppmlasso$notes

  plot(this.ppmlasso)

}

# Print method for objects of class enmtools.ppmlasso
print.enmtools.ppmlasso <- function(this.ppmlasso){

  print(summary(this.ppmlasso))

}


#' Plot method for objects of class enmtools.ppmlasso
#' @param this.ppmlasso An enmtools.ppmlasso object
#' @param trans_col Transformation to apply to the colour range (Z axis). A character value corresponding to one of \code{ggplot2}'s builtin scale transformations (see argument \code{trans} in \code{\link[ggplot2]{continuous_scale}} for possible choices). \code{trans_col = 'sqrt'} will often work well for ppmlasso plots. If NULL, no transformation is applied.
#'
plot.enmtools.ppmlasso <- function(this.ppmlasso, trans_col = NULL){


  suit.points <- data.frame(rasterToPoints(this.ppmlasso$suitability))
  colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

  suit.plot <- ggplot(data = suit.points, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = Suitability)) +
    coord_fixed() + theme_classic() +
    geom_point(data = this.ppmlasso$analysis.df[this.ppmlasso$analysis.df$presence == 1,], aes(x = Longitude, y = Latitude),
               pch = 21, fill = "white", color = "black", size = 2)

  if(!(all(is.na(this.ppmlasso$test.data)))){
    suit.plot <- suit.plot + geom_point(data = this.ppmlasso$test.data, aes(x = Longitude, y = Latitude),
                                        pch = 21, fill = "green", color = "black", size = 2)
  }
  if(!is.null(trans_col)) {
    suit.plot <- suit.plot + scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability"), trans = trans_col)
  } else {
    suit.plot <- suit.plot + scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Suitability"))
  }

  return(suit.plot)

}

# Function for checking data prior to running enmtools.ppmlasso
ppmlasso.precheck <- function(f, species, env){

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

# Unexported helper function copied from ppmlasso (from which it was unexported) with permission of Author:
# Ian Renner
ppmlasso_weights <- function (sp.xy, quad.xy, coord = c("X", "Y"))
{
  sp.col = c(which(names(sp.xy) == coord[1]), which(names(sp.xy) ==
                                                      coord[2]))
  quad.col = c(which(names(quad.xy) == coord[1]), which(names(quad.xy) ==
                                                          coord[2]))
  X.inc = sort(unique(quad.xy[, quad.col[1]]))[2] - sort(unique(quad.xy[,
                                                                        quad.col[1]]))[1]
  Y.inc = sort(unique(quad.xy[, quad.col[2]]))[2] - sort(unique(quad.xy[,
                                                                        quad.col[2]]))[1]
  quad.0X = min(quad.xy[, quad.col[1]]) - floor(min(quad.xy[,
                                                            quad.col[1]])/X.inc) * X.inc
  quad.0Y = min(quad.xy[, quad.col[2]]) - floor(min(quad.xy[,
                                                            quad.col[2]])/Y.inc) * Y.inc
  X = c(sp.xy[, quad.col[1]], quad.xy[, quad.col[1]])
  Y = c(sp.xy[, quad.col[2]], quad.xy[, quad.col[2]])
  round.X = round((X - quad.0X)/X.inc) * X.inc
  round.Y = round((Y - quad.0Y)/Y.inc) * Y.inc
  round.id = paste(round.X, round.Y)
  round.table = table(round.id)
  wt = X.inc * Y.inc/as.numeric(round.table[match(round.id,
                                                  names(round.table))])
  wt
}
