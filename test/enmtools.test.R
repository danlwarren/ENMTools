setwd("~/GitHub/ENMTools/test")
library(ENMTools)


env.files <- list.files(path = "testdata/", pattern = "pc", full.names = TRUE)
env <- stack(env.files)

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

ahli.glm <- enmtools.glm(pres ~ layer.1 + layer.2 + layer.3 + layer.4, ahli, env)
ahli.glm


ahli.twovar.glm <- enmtools.glm(pres ~ layer.1 + layer.4, ahli, env)
ahli.twovar.glm

ahli.bc <- enmtools.bc(ahli, env)
ahli.bc2 <- enmtools.bc(ahli, env[[c("layer.1", "layer.4")]])


ahli.mx <- enmtools.maxent(ahli, env)
