library(ppmlasso)
library(raster)
library(sp)
library(ENMTools)
data(BlueMountains)

euc_pnts <- BlueMountains$env
coordinates(euc_pnts) <- ~X+Y
gridded(euc_pnts) <- TRUE
euc_rast <- lapply(1:(ncol(euc_pnts)), function(x) raster(euc_pnts, layer = x))
euc_rast <- stack(euc_rast)

env <- euc_rast
names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4", "layer.5", "layer.5")
env <- setMinMax(env)


euc_species <- enmtools.species()
euc_species$presence.points <- BlueMountains$eucalypt


test <- as.data.frame(euc_rast, xy = TRUE, na.rm = TRUE)


env.files <- list.files(path = "test/testdata/", pattern = "pc", full.names = TRUE)
env2 <- stack(env.files)
names(env2) <- c("layer.1", "layer.2", "layer.3", "layer.4")
env2 <- setMinMax(env2)

ahli <- enmtools.species()
ahli$species.name <- "ahli"
ahli$presence.points <- read.csv("test/testdata/ahli.csv")[,3:4]
ahli$range <- background.raster.buffer(ahli$presence.points, 50000, mask = env)
ahli$background.points <- background.points.buffer(points = ahli$presence.points,
                                                   radius = 20000, n = 1000, mask = env2[[1]])

