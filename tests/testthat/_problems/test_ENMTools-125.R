# Extracted from test_ENMTools.R:125

# prequel ----------------------------------------------------------------------
library(testthat)
library(ENMTools)
set.seed(2282023)
expect_species <- function(species){
  expect_true(inherits(species, c("list", "enmtools.species")))
  expect_equal(names(species), c("range", "presence.points", "background.points",
                                   "models", "species.name"))
  expect_true(inherits(species$range, "SpatRaster"))
  expect_true(inherits(species$presence.points, "SpatVector"))
  expect_true(inherits(species$species.name, "character"))
}
expect_enmtools_model <- function(model){
  expect_true(inherits(model, "enmtools.model"),
              info = "Not an enmtools.model object")

  expect_true(all(names(model) %in% c("species.name", "analysis.df", "test.data", "test.prop", "model",
                                      "training.evaluation", "test.evaluation", "env.training.evaluation",
                                      "env.test.evaluation", "rts.test",  "suitability", "clamping.strength", "call",  "notes", "response.plots",
                                      "formula", "hv")), info = "Unexpected items in enmtools.model object!")

  expect_true(inherits(model$species.name, "character"),
              info = "species.name is not a character")

  expect_true(inherits(model$analysis.df, "data.frame"),
              info = "analysis.df is not a data frame")

  expect_true(inherits(model$test.prop, "numeric"),
              info = "test.prop is not numeric")

  expect_true(all(class(model$model) %in% c("MaxEnt", "Domain", "Bioclim",
                                            "randomForest.formula", "randomForest",
                                            "list", "glm", "lm", "gam", "ranger", "workflow",
                                            "maxnet", "Hypervolume")),
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

    expect_true(inherits(model$test.data, "SpatVector"),
                info = "Test proportion is greater than 0 but test.data is not a SpatVector")
  } else {
    expect_true(inherits(model$test.evaluation, "logical"))

    expect_true(inherits(model$test.data, "logical"))
  }

  expect_true(inherits(model$suitability, "SpatRaster"))

  expect_true(inherits(model$response.plots, "list"))

  expect_true(all(sapply(model$response.plots, is.ggplot)))
}
monticola <- iberolacerta.clade$species$monticola
martinezricai <- iberolacerta.clade$species$martinezricai
cyreni <- iberolacerta.clade$species$cyreni
horvathi <- iberolacerta.clade$species$horvathi
aurelioi <- iberolacerta.clade$species$aurelioi
aranica <- iberolacerta.clade$species$aranica
bonnali <- iberolacerta.clade$species$bonnali
ib.tree <- iberolacerta.clade$tree
iberolacerta.clade <- enmtools.clade(species = list(monticola = monticola,
                                                    martinezricai = martinezricai,
                                                    cyreni = cyreni,
                                                    horvathi = horvathi,
                                                    aurelioi = aurelioi,
                                                    aranica = aranica,
                                                    bonnali = bonnali), tree = ib.tree)
check.clade(iberolacerta.clade)

# test -------------------------------------------------------------------------
cyreni.dm <- enmtools.dm(cyreni, euro.worldclim, test.prop = 0.2)
cyreni.bc <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0)
expect_enmtools_model(cyreni.dm)
expect_enmtools_model(cyreni.bc)
cyreni.bc <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0.2)
expect_enmtools_model(cyreni.bc)
cyreni.glm <- enmtools.glm(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
expect_enmtools_model(cyreni.glm)
expect_enmtools_model(cyreni.glm)
