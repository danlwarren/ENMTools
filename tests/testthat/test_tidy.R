library(testthat)
library(ENMTools)

set.seed(1012026)

monticola <- iberolacerta.clade$species$monticola
cyreni <- iberolacerta.clade$species$cyreni
env <- euro.worldclim[[c("bio1", "bio12")]]

test_that("enmtools.tidy works with a parsnip model specification", {
  skip_if_not_installed("parsnip")

  mod <- enmtools.tidy(monticola, env, model = parsnip::logistic_reg(), test.prop = 0.2, nback = 200)
  expect_true(inherits(mod, c("enmtools.tidy", "enmtools.model")))
  expect_true(inherits(mod$model, "workflow"))
  expect_true(inherits(mod$suitability, "SpatRaster"))
  expect_true(inherits(mod$test.evaluation, "ModelEvaluation"))

  expect_error(enmtools.tidy(monticola, env, model = "glm", legacy = TRUE), "legacy")
  expect_warning(enmtools.tidy(monticola, env, model = "glm", nback = 200, model.args = list(step = TRUE)), "step")
})

test_that("DOMAIN follows Carpenter et al. 1993", {
  dat <- enmtools.prep(monticola, env, nback = 200)$data
  x <- dat[ , c("bio1", "bio12")]
  fit <- domain.bridge(x, dat[ , "presence", drop = FALSE])
  pres <- as.matrix(x[dat$presence == 1, ])
  rng <- apply(pres, 2, function(v) diff(range(v)))

  # Maximum over presences of one minus the range-standardized Gower distance
  expected <- apply(as.matrix(x), 1, function(p)
    max(0, max(1 - rowMeans(abs(sweep(pres, 2, p)) / matrix(rng, nrow(pres), 2, byrow = TRUE)))))
  expect_equal(domain.predict(fit, x), unname(expected))
  expect_true(all(domain.predict(fit, pres) == 1))

  # Variables that don't vary among presences are dropped with a warning
  x.const <- cbind(x, const = 1)
  expect_warning(fit.const <- domain.bridge(x.const, dat[ , "presence", drop = FALSE]), "const")
  expect_equal(colnames(fit.const$presence), c("bio1", "bio12"))

  dm <- enmtools.dm(monticola, env, nback = 200)
  dm.dismo <- enmtools.dm(monticola, env, nback = 200, dismo = TRUE)
  expect_s3_class(workflows::extract_fit_engine(dm$model), "enmtools.domain")
  expect_true(inherits(workflows::extract_fit_engine(dm.dismo$model), "Domain"))
})

test_that("presence-only models are fitted to presences only", {
  bc <- enmtools.bc(monticola, env, test.prop = 0.2, nback = 200)
  n.pres <- sum(bc$analysis.df$presence == 1)
  expect_equal(nrow(workflows::extract_fit_engine(bc$model)@presence), n.pres)

  dm <- enmtools.dm(monticola, env, test.prop = 0.2, nback = 200)
  expect_equal(nrow(workflows::extract_fit_engine(dm$model)$presence), n.pres)
})

test_that("envelope models use formulas and recipes only to select predictors", {
  f <- presence ~ poly(bio1, 2) + bio12 + bio1:bio12
  expect_warning(bc <- enmtools.bc(monticola, euro.worldclim, f = f, nback = 200), "ignored")
  expect_equal(colnames(workflows::extract_fit_engine(bc$model)@presence), c("bio1", "bio12"))
  expect_warning(dm <- enmtools.dm(monticola, euro.worldclim, f = f, nback = 200), "ignored")
  expect_equal(colnames(workflows::extract_fit_engine(dm$model)$presence), c("bio1", "bio12"))
  expect_no_warning(enmtools.dm(monticola, euro.worldclim, f = presence ~ bio1 + bio12, nback = 200))

  skip_if_not_installed("recipes")
  dat <- enmtools.prep(monticola, euro.worldclim, nback = 50)$data
  rec <- recipes::step_pca(recipes::recipe(presence ~ bio1 + bio5 + bio12, data = dat), recipes::all_predictors())
  expect_warning(dm.rec <- enmtools.dm(monticola, euro.worldclim, f = rec, nback = 200), "ignored")
  expect_equal(colnames(workflows::extract_fit_engine(dm.rec$model)$presence), c("bio1", "bio5", "bio12"))
})

test_that("clamping defaults are stored and remembered by predict", {
  glm <- enmtools.glm(monticola, env, nback = 200)
  glm.unclamped <- enmtools.glm(monticola, env, nback = 200, clamp = FALSE)
  bc <- enmtools.bc(monticola, env, nback = 200)
  bc.clamped <- enmtools.bc(monticola, env, nback = 200, clamp = TRUE)

  expect_true(glm$clamp)
  expect_false(glm.unclamped$clamp)
  expect_false(bc$clamp)
  expect_true(bc.clamped$clamp)

  # Projecting to more extreme conditions
  new.env <- env * 1.5
  expect_true(inherits(predict(glm, new.env)$clamping.strength, "SpatRaster"))
  expect_true(is.na(predict(glm.unclamped, new.env)$clamping.strength))
  expect_true(is.na(predict(bc, new.env)$clamping.strength))
  expect_true(inherits(predict(bc, new.env, clamp = TRUE)$clamping.strength, "SpatRaster"))
})

test_that("weights and model arguments are passed to the engines", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mgcv")

  rf <- enmtools.rf.ranger(monticola, env, nback = 200)
  w <- as.numeric(rf$analysis.df$weights)
  pres <- rf$analysis.df$presence == 1
  expect_equal(sum(w[pres]), sum(w[!pres]))
  expect_true(inherits(workflows::extract_fit_engine(rf$model), "ranger"))

  rf.none <- enmtools.rf.ranger(monticola, env, nback = 200, weights = "none")
  expect_true(all(as.numeric(rf.none$analysis.df$weights) == 1))

  gam <- enmtools.gam(monticola, env, nback = 200, gam.method = "ML", gam.select = FALSE)
  expect_equal(workflows::extract_fit_engine(gam$model)$method, "ML")

  glm <- enmtools.glm(monticola, env, nback = 200, weights = "none")
  expect_true(all(workflows::extract_fit_engine(glm$model)$prior.weights == 1))
})

test_that("hypothesis tests accept tidy models and check model types", {
  skip_on_cran()
  skip_if_not_installed("parsnip")

  id <- identity.test(monticola, cyreni, env, type = "tidy", model = parsnip::logistic_reg(), nreps = 2, nback = 200)
  expect_s3_class(id, "enmtools.identity.test")

  bg <- background.test(monticola, cyreni, env, type = "bc", nreps = 2, nback = 200)
  expect_s3_class(bg, "enmtools.background.test")

  expect_error(identity.test(monticola, cyreni, env, type = "tidy", nreps = 2), "parsnip")
  expect_error(identity.test(monticola, cyreni, env, type = "foo", nreps = 2), "not understood")
})

test_that("permutation.bias works with tidy models", {
  skip_on_cran()

  pb <- permutation.bias(monticola, env, env, type = "glm", nreps = 2)
  expect_s3_class(pb, "enmtools.permutation.bias")
  expect_true(inherits(pb$empirical.model, "enmtools.tidy"))
  expect_true(inherits(pb$mean.change.raster, "SpatRaster"))
})

test_that("enmtools.vip calculates permutation importance and SHAP values", {
  skip_on_cran()
  skip_if_not_installed("reshape2")

  glm <- enmtools.glm(monticola, env, nback = 200, test.prop = 0.2)
  vip <- enmtools.vip(glm, method = c("permute", "shap"), nsim = 2, shap.npres = 20, verbose = FALSE)
  expect_s3_class(vip, "enmtools.vip")
  expect_equal(sort(vip$permute$Variable), c("bio1", "bio12"))
  expect_s3_class(vip$shap.plot, "ggplot")

  # Exact SHAP values for a GLM add up to the linear predictor
  engine <- workflows::extract_fit_engine(glm$model)
  link <- predict(engine, newdata = vip$shap$X, type = "link")
  expect_equal(unname(rowSums(vip$shap$S) + vip$shap$baseline), unname(link), tolerance = 1e-6)

  skip_if_not_installed("treeshap")
  skip_if_not_installed("ranger")
  rf <- enmtools.rf.ranger(monticola, env, nback = 200)
  rf.vip <- enmtools.vip(rf, method = "shap", shap.npres = 20, verbose = FALSE)
  pred <- predict(rf$model, new_data = rf.vip$shap$X, type = "prob")$.pred_1
  expect_equal(unname(rowSums(rf.vip$shap$S) + rf.vip$shap$baseline), pred, tolerance = 1e-3)
})

test_that("enmtools.calibrate recalibrates models with tied predictions", {
  skip_on_cran()
  skip_if_not_installed("CalibratR")
  skip_if_not_installed("caret")
  skip_if_not_installed("ResourceSelection")
  skip_if_not_installed("ecospat")

  bc <- enmtools.bc(monticola, env, test.prop = 0.3, nback = 200)
  expect_message(cal <- suppressWarnings(enmtools.calibrate(bc, recalibrate = TRUE)), "ties")
  expect_s3_class(cal, "enmtools.recalibrated.model")
  expect_true(all(sapply(cal$calibrated.suitabilities, inherits, "SpatRaster")))
})

test_that("moses.list compares nested models fitted to the same data", {
  skip_on_cran()

  moses <- moses.list(list(monticola, cyreni), env, nback = 200)
  separate.ll <- sum(sapply(moses$separate.glms, function(x) as.numeric(logLik(workflows::extract_fit_engine(x$model)))))
  combined.ll <- as.numeric(logLik(workflows::extract_fit_engine(moses$combined.glm$model)))

  # Separate models nest the combined one, so they can't fit worse
  expect_gte(separate.ll, combined.ll)
  expect_true(all(sapply(moses$separate.glms, function(x) all(workflows::extract_fit_engine(x$model)$prior.weights == 1))))

  expect_error(moses.list(list(monticola, cyreni), env, weights = "equal"), "weights")
})
