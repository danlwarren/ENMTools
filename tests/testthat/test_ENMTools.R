library(testthat)
library(ENMTools)

data(iberolacerta.clade)
data(euro.worldclim)

expect_species <- function(species){
  expect_true(inherits(monticola, c("list", "enmtools.species")))
  expect_equal(names(monticola), c("range", "presence.points", "background.points",
                                   "models", "species.name"))
  expect_true(inherits(monticola$range, "RasterLayer"))
  expect_true(inherits(monticola$presence.points, "data.frame"))
  expect_true(inherits(monticola$species.name, "character"))
}


expect_enmtools_model <- function(model){
  expect_true(inherits(model, "enmtools.model"),
              info = "Not an enmtools.model object")

  expect_true(all(names(model) %in% c("species.name", "analysis.df", "test.data", "test.prop", "model",
                                      "training.evaluation", "test.evaluation", "env.training.evaluation",
                                      "env.test.evaluation", "rts.test",  "suitability", "clamping.strength", "call",  "notes", "response.plots",
                                      "formula")), info = "Unexpected items in enmtools.model object!")

  expect_true(inherits(model$species.name, "character"),
              info = "species.name is not a character")

  expect_true(inherits(model$analysis.df, "data.frame"),
              info = "analysis.df is not a data frame")

  expect_true(inherits(model$test.prop, "numeric"),
              info = "test.prop is not numeric")

  expect_true(all(class(model$model) %in% c("MaxEnt", "Domain", "Bioclim",
                                            "randomForest.formula", "randomForest",
                                            "ppmlasso", "list", "glm", "lm", "gam", "ranger")),
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


monticola <- iberolacerta.clade$species$monticola
martinezricai <- iberolacerta.clade$species$martinezricai
cyreni <- iberolacerta.clade$species$cyreni
horvathi <- iberolacerta.clade$species$horvathi
aurelioi <- iberolacerta.clade$species$aurelioi
aranica <- iberolacerta.clade$species$aranica
bonnali <- iberolacerta.clade$species$bonnali

ib.tree <- iberolacerta.clade$tree

test_that("enmtools.species object work", {
  expect_species(monticola)
  expect_species(martinezricai)
  expect_species(cyreni)
  expect_species(horvathi)
  expect_species(aurelioi)
  expect_species(aranica)
  expect_species(bonnali)
})

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

cyreni.dm <- enmtools.dm(cyreni, euro.worldclim, test.prop = 0.2)
cyreni.bc <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0)

test_that("enmtools.model objects work for core methods", {

  expect_enmtools_model(cyreni.dm)
  expect_enmtools_model(cyreni.bc)

  cyreni.bc <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0.2)
  expect_enmtools_model(cyreni.bc)

  cyreni.glm <- enmtools.glm(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
  expect_enmtools_model(cyreni.glm)



  # skip_on_ci()
  # skip_on_cran()
  # cyreni.mx <- enmtools.maxent(cyreni, euro.worldclim, test.prop = 0.2)
  # expect_enmtools_model(cyreni.mx)
})

test_that("rf model objects work", {
  skip_if_not_installed("randomForest")
  cyreni.rf <- enmtools.rf(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
  expect_enmtools_model(cyreni.rf)
})


test_that("ranger model objects work", {
  skip_if_not_installed("ranger")
  cyreni.rf.ranger <- enmtools.rf.ranger(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
  expect_enmtools_model(cyreni.rf.ranger)
})

test_that("ppm model objects work", {
  skip_if_not_installed("ppmlasso")
  cyreni.ppm <- enmtools.ppmlasso(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
  expect_enmtools_model(cyreni.ppm)
})

test_that("gam model objects work", {
  skip_if_not_installed("mgcv")
  cyreni.gam <- enmtools.gam(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
  expect_enmtools_model(cyreni.gam)
})


#' Simple interactive.plot tests
test_that("interactive.plot produces correct object", {
  skip_if_not_installed("leaflet")
  skip_on_ci()
  m_dm <- interactive.plot(cyreni.dm)
  m_dm_cluster <- interactive.plot(cyreni.dm, cluster.points = TRUE)
  expect_is(m_dm, "leaflet")
  expect_is(m_dm_cluster, "leaflet")
  expect_match(sapply(m_dm_cluster$x$calls, function(x) x$method), "addRasterImage", all = FALSE)
  expect_match(sapply(m_dm$x$calls, function(x) x$method), "addRasterImage", all = FALSE)
})

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






