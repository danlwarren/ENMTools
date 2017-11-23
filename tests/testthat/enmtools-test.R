rm(list=ls(all = TRUE))

library(testthat)
library(ENMTools)

#' Load data
#'
#'

iberolacerta <- read.csv("iberolacerta.points.csv")
ib.tree <- read.tree(file = "iberolacerta.brlens.tree")
euro.worldclim <- stack("Euro Worldclim.grd")

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


#' Env space metrics and visualization
#'
#'


#' Monte Carlo tests, ENMTools-style
#'
#'


#' Ecospat tests
#'
#'






