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

ahli$background.points <- background.points.buffer(points = ahli$presence.points,radius = 10000, n = 1000, mask = env[[1]])
allogus$background.points <- background.points.buffer(points = allogus$presence.points,radius = 10000, n = 1000, mask = env[[1]])

two.anoles <- enmtools.clade(list(ahli, allogus))
two.anoles
check.clade(two.anoles)

summary(ahli)
print(ahli)
plot(ahli)

summary(two.anoles)
plot(two.anoles)

# NOTE TO SELF: started off passing a vector of args to insist on to make testing easier,
# but this quickly turned into a nightmare.  We'll just check the existence of the important
# bits manually before each analysis.




