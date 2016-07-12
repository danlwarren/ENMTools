#' Takes an emtools.species object with presence and background points, and builds a GLM
#'
#' @param formula Standard GLM formula
#' @param species An enmtools.species object
#'
#' @export enmtools.glm

enmtools.glm <- function(f, species){

  if(!class(f) == "formula"){
    stop("Argument \'formula\' must contain an R formula object!")
  }

  if(!"enmtools.species" %in% class(species)){
    stop("Argument \'species\' must contain an enmtools.species object!")
  }

  if(!any(c("data.frame") %in% class(species$presence.points))){
    print("Species presence.points do not appear to be an object of class data.frame")
  }

  if(!any(c("data.frame") %in% class(species$background.points))){
    print("Species background.points do not appear to be an object of class data.frame")
  }

  # Going to recast this formula so that the response variable is named "presence"
  # regardless of what was passed.
  f <- reformulate(attr(delete.response(terms(f)), "term.labels"), response = "presence")

  analysis.df <- rbind(species$presence.points[,-c(1,2)], species$background.points[,-c(1,2)])
  analysis.df$presence <- c(rep(1, nrow(species$presence.points)), rep(0, nrow(species$background.points)))

  this.glm <- glm(f, analysis.df, family="binomial")

  output <- list(formula = f,
                 analysis.df = analysis.df,
                 model = this.glm)

  class(output) <- "enmtools.glm"

  return(output)

}

summary.enmtools.glm <- function(this.glm){

  cat("\n\nFormula:  ")
  print(this.glm$formula)

  cat("\n\nData table (top ten lines): ")
  print(kable(head(this.glm$analysis.df, 10)))

  cat("\n\nModel:  ")
  print(this.glm$model)

}


print.enmtools.glm <- function(this.glm){

  summary(this.glm)

}

