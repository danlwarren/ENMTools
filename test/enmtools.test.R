setwd("~/GitHub/ENMTools/test")
library(ENMTools)

# This code builds the readme.rmd file into a regular md file, which GitHub understands better
knit(input="Readme.Rmd", output = "readme.md")

env.files <- list.files(path = "testdata/", pattern = "pc", full.names = TRUE)
env <- stack(env.files)
names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4")
env <- setMinMax(env)

ahli <- enmtools.species()
check.species(ahli)

allogus <- enmtools.species(species.name = "allogus", presence.points = read.csv("testdata/allogus.csv")[,3:4])
check.species(allogus)

ahli <- enmtools.species(species.name = "ahli", presence.points = read.csv("testdata/ahli.csv")[,3:4])
check.species(ahli)

ahli$range <- background.raster.buffer(ahli$presence.points, 50000, mask = env)
allogus$range <- background.raster.buffer(allogus$presence.points, 50000, mask = env)

ahli$background.points <- background.points.buffer(points = ahli$presence.points,radius = 20000, n = 1000, mask = env[[1]])
allogus$background.points <- background.points.buffer(points = allogus$presence.points,radius = 20000, n = 1000, mask = env[[1]])

# Should fail because presence and background have different col names
check.species(ahli)

colnames(ahli$background.points) <- colnames(ahli$presence.points)
colnames(allogus$background.points) <- colnames(allogus$presence.points)

two.anoles <- enmtools.clade(list(ahli, allogus))
two.anoles
check.clade(two.anoles)

summary(ahli)
print(ahli)
plot(ahli)



summary(two.anoles)
plot(two.anoles)

ahli.glm <- enmtools.glm(pres ~ layer.1 + layer.2 + layer.3 + layer.4, ahli, env, test.prop = 0.2)
ahli.glm
ahli.glm$response.plots

onevar.glm <- enmtools.glm(pres ~ layer.1, ahli, env, test.prop = 0.2)
onevar.glm
onevar.glm$response.plots

allogus.glm <- enmtools.glm(pres ~ layer.1 + layer.2 + layer.3 + layer.4, allogus, env)
allogus.glm

ahli.twovar.glm <- enmtools.glm(pres ~ layer.1 + layer.4, ahli, env)
ahli.twovar.glm

# Presently leaving out GAM, as it is hard to see how I'm going to be able to
# use it in the same way as the other methods.
# Might eventually write a function to parse GLM-style functions to a GAMmable function
# ahli.gam <- enmtools.gam(presence ~, ahli, env)


ahli.bc <- enmtools.bc(ahli, env, test.prop = 0.2)
ahli.bc
ahli.bc$response.plots

ahli.bc2 <- enmtools.bc(ahli, env[[c("layer.1", "layer.4")]])


ahli.mx <- enmtools.maxent(ahli, env, test.prop = 0.2)
ahli.mx
ahli.mx$response.plots

ahli.dm <- enmtools.dm(ahli, env, test.prop = 0.2)
ahli.dm
ahli.dm$response.plots

allogus.dm <- enmtools.dm(allogus, env)
allogus.dm

raster.cor(allogus.dm, ahli.dm)
raster.cor(ahli.mx, ahli.dm)

plot(raster.standardize(ahli.dm))

raster.breadth(allogus.dm)

raster.overlap(allogus.dm, allogus.dm$suitability)
raster.overlap(ahli.dm, allogus.dm)

raster.resid(ahli.dm, ahli.glm)

plot(raster.resid(ahli.glm, ahli.dm)$residuals)

env.overlap(ahli.dm, ahli.glm, env)



id.glm <- identity.test(species.1 = ahli, species.2 = allogus, env = env, type = "glm", f = presence ~ layer.1 + layer.2 + layer.3 + layer.4, nreps = 4)

id.mx <- identity.test(species.1 = ahli, species.2 = allogus, env = env, type = "mx", nreps = 4)

id.bc <- identity.test(species.1 = ahli, species.2 = allogus, env = env, type = "bc", nreps = 4)

id.dm <- identity.test(species.1 = ahli, species.2 = allogus, env = env, type = "dm", nreps = 4)

bg.glm.sym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "glm",
                           f = presence ~ layer.1 + layer.2 + layer.3 + layer.4, nreps = 4, test.type = "symmetric" )

bg.glm.asym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "glm",
                              f = presence ~ layer.1 + layer.2 + layer.3 + layer.4, nreps = 4, test.type = "asymmetric" )

bg.bc.sym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "bc", nreps = 4, test.type = "symmetric" )

bg.bc.asym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "bc", nreps = 4, test.type = "asymmetric" )

bg.dm.sym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "dm", nreps = 4, test.type = "symmetric" )

bg.dm.asym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "dm", nreps = 4, test.type = "asymmetric" )

bg.mx.sym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "mx", nreps = 4, test.type = "symmetric" )


bg.mx.asym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "mx", nreps = 4, test.type = "asymmetric" )


rbl.bc <- rangebreak.linear(ahli, allogus, env, type = "bc", nreps = 4)

rbl.dm <- rangebreak.linear(ahli, allogus, env, type = "dm", nreps = 4)

rbl.glm <- rangebreak.linear(ahli, allogus, env, type = "bc", f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, nreps = 4)

rbl.mx <- rangebreak.linear(ahli, allogus, env, type = "mx", nreps = 4)


rbb.bc <- rangebreak.blob(ahli, allogus, env, type = "bc", nreps = 4)

rbb.dm <- rangebreak.blob(ahli, allogus, env, type = "dm", nreps = 4)

rbb.mx <- rangebreak.blob(ahli, allogus, env, type = "mx", nreps = 4)

rbb.glm <- rangebreak.blob(ahli, allogus, env, type = "glm", f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, nreps = 4)

# Should draw background from env
ahli.2 <- enmtools.species(species.name = "ahli", presence.points = read.csv("testdata/ahli.csv")[,3:4])
ahli.glm.nobg <- enmtools.glm(ahli.2, env = env, f = pres ~ layer.1 + layer.2)
ahli.glm.nobg

# Should draw background from range
ahli.3 <- ahli
ahli.3$background.points <- NA
ahli.3
ahli.glm.rangebg <- enmtools.glm(ahli.3, env = env, f = pres ~ layer.1 + layer.2)
ahli.glm.rangebg


allogus.nobg <- enmtools.species(species.name = "allogus", presence.points = read.csv("testdata/allogus.csv")[,3:4])
ahli.nobg <- enmtools.species(species.name = "ahli", presence.points = read.csv("testdata/ahli.csv")[,3:4])

id.nobg <- identity.test(allogus.nobg, ahli.nobg, env = env, type = "dm", nreps = 4)

# Next steps: ribbon bg test - currently have precheck code working but need to do the rest
# Have a think about the idea of incorporating test data,
# particularly now that the interface for modeling is so nice.
# ppmlasso
# use inherits instead of %in% class

ribbon <- enmtools.species(species.name = "ribbon")
ribbon$presence.points <- data.frame(Longitude = runif(n = 10, min = -79, max = -78.5),
                                      Latitude = runif(n = 10, min = 21.7, max = 22.1))
plot(env[[1]])
points(ribbon$presence.points)
ribbon$range <- background.raster.buffer(ribbon$presence.points, 20000, mask = env)
ribbon

rbr <- rangebreak.ribbon(ahli, allogus, ribbon, env, type = "dm", width = 0.3, nreps = 4)
