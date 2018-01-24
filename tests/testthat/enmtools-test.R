rm(list=ls(all = TRUE))

library(testthat)
library(ENMTools)

#' Load data
#'
#'

iberolacerta <- read.csv("iberolacerta.points.csv")
ib.tree <- read.tree(file = "iberolacerta.brlens.tree")
euro.worldclim <- stack("euro.worldclim.grd")

expect_equal(class(ib.tree), "phylo")
expect_equal(length(names(euro.worldclim)), 19)
expect_true(inherits(euro.worldclim, "RasterStack"))

#' Make enmtools.species objects
#'
#'


make.species <- function(points, env, name){
  this.species <- enmtools.species(presence.points = points,
                                   species.name = name)
  this.species <- check.species(this.species)
  this.species$range <- background.raster.buffer(this.species$presence.points, 50000, mask = env)
  return(this.species)
}

expect_species <- function(species){
  expect_true(inherits(monticola, c("list", "enmtools.species")))
  expect_equal(names(monticola), c("range", "presence.points", "background.points",
                                   "models", "species.name"))
  expect_true(inherits(monticola$range, "RasterLayer"))
  expect_true(inherits(monticola$presence.points, "data.frame"))
  expect_true(inherits(monticola$species.name, "character"))
}


monticola <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta monticola",2:3],
                          euro.worldclim, "monticola")

martinezricai <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta martinezricai",2:3],
                              euro.worldclim, "martinezricai")

cyreni <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta cyreni",2:3],
                       euro.worldclim, "cyreni")

horvathi <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta horvathi",2:3],
                         euro.worldclim, "horvathi")

aurelioi <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta aurelioi",2:3],
                         euro.worldclim, "aurelioi")

aranica <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta aranica",2:3],
                        euro.worldclim, "aranica")

bonnali <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta bonnali",2:3],
                        euro.worldclim, "bonnali")

expect_species(monticola)
expect_species(martinezricai)
expect_species(cyreni)
expect_species(horvathi)
expect_species(aurelioi)
expect_species(aranica)
expect_species(bonnali)

#' Make an enmtools.clade object
#'
#'

iberolacerta.clade <- enmtools.clade(species = list(monticola = monticola,
                                                    martinezricai = martinezricai,
                                                    cyreni = cyreni,
                                                    horvathi = horvathi,
                                                    aurelioi = aurelioi,
                                                    aranica = aranica,
                                                    bonnali = bonnali), tree = ib.tree)

check.clade(iberolacerta.clade)

#' Build ENMs using various methods and test outputs
#'
#'

expect_enmtools_model <- function(model){
  expect_true(inherits(model, "enmtools.model"),
              info = "Not an enmtools.model object")

  expect_true(all(names(model) %in% c("species.name", "analysis.df", "test.data", "test.prop", "model",
                               "training.evaluation", "test.evaluation", "env.training.evaluation",
                               "env.test.evaluation", "suitability", "notes", "response.plots",
                               "formula")), info = "Unexpected items in enmtools.model object!")

  expect_true(inherits(model$species.name, "character"),
              info = "species.name is not a character")

  expect_true(inherits(model$analysis.df, "data.frame"),
              info = "analysis.df is not a data frame")

  expect_true(inherits(model$test.prop, "numeric"),
              info = "test.prop is not numeric")

  expect_true(all(class(model$model) %in% c("MaxEnt", "Domain", "Bioclim",
                                        "randomForest.formula", "randomForest",
                                        "ppmlasso", "list", "glm", "lm", "gam")),
              info = "Class of model is not recognized")

  # Evaluation on training data happens unless it's bypassed (GLM only I think)
  expect_true(inherits(model$training.evaluation, "ModelEvaluation"))
  expect_true(inherits(model$env.training.evaluation, "ModelEvaluation"))

  # Evaluation on test data is for test.prop > 0 only
  if(model$test.prop > 0){
    expect_true(inherits(model$test.evaluation, "ModelEvaluation"),
                info = "Test proportion greater than 0 but test.evaluation is not a ModelEvaluation object")

    expect_true(inherits(model$env.test.evaluation, "ModelEvaluation"),
                info = "Test proportion greater than 0 but env.test.evaluation is not a ModelEvaluation object")

    expect_true(inherits(model$test.data, "data.frame"),
                info = "Test proportion is greater than 0 but test.data is not a data frame")
  } else {
    expect_true(inherits(model$test.evaluation, "logical"))

    expect_true(inherits(model$test.data, "logical"))
  }

  expect_true(inherits(model$suitability, "RasterLayer"))

  expect_true(inherits(model$response.plots, "list"))

  expect_true(all(sapply(model$response.plots, class) %in% c("gg", "ggplot")))
}


cyreni.mx <- enmtools.maxent(cyreni, euro.worldclim, test.prop = 0.2)
expect_enmtools_model(cyreni.mx)

cyreni.dm <- enmtools.dm(cyreni, euro.worldclim, test.prop = 0.2)
expect_enmtools_model(cyreni.dm)

cyreni.bc <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0)
expect_enmtools_model(cyreni.bc)

cyreni.bc <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0.2)
expect_enmtools_model(cyreni.bc)

cyreni.rf <- enmtools.rf(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
expect_enmtools_model(cyreni.rf)

cyreni.ppm <- enmtools.ppmlasso(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
expect_enmtools_model(cyreni.ppm)

cyreni.glm <- enmtools.glm(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
expect_enmtools_model(cyreni.glm)

cyreni.gam <- enmtools.gam(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
expect_enmtools_model(cyreni.gam)



#' Geographic space metrics and visualization
#'
#'



#' Env space metrics and visualization
#'
#'



#' Monte Carlo tests, ENMTools-style
#'
#'


#' Ecospat tests
#'
#'






