expect_species <- function(species){
  expect_true(inherits(monticola, c("list", "enmtools.species")))
  expect_equal(names(monticola), c("range", "presence.points", "background.points",
                                   "models", "species.name"))
  expect_true(inherits(monticola$range, "RasterLayer"))
  expect_true(inherits(monticola$presence.points, "data.frame"))
  expect_true(inherits(monticola$species.name, "character"))
}
