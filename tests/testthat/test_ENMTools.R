library(testthat)
library(ENMTools)

data(iberolacerta.clade)
data(euro.worldclim)
source("helper_expect_species.R")
source("helper_make_species.R")
source("helper_expect_enmtools_model.R")

monticola <- iberolacerta.clade$species$monticola
martinezricai <- iberolacerta.clade$species$martinezricai
cyreni <- iberolacerta.clade$species$cyreni
horvathi <- iberolacerta.clade$species$horvathi
aurelioi <- iberolacerta.clade$species$aurelioi
aranica <- iberolacerta.clade$species$aranica
bonnali <- iberolacerta.clade$species$bonnali

ib.tree <- iberolacerta.clade$tree

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






