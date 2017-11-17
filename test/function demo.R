library(ENMTools)
setwd("~/GitHub/ENMTools/test")

allogus <- enmtools.species(species.name = "allogus", presence.points = read.csv("testdata/allogus.csv")[,3:4])
check.species(allogus)

env.files <- list.files(path = "testdata/", pattern = "pc", full.names = TRUE)
env <- stack(env.files)
names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4")
env <- setMinMax(env)

allogus.glm <- enmtools.glm(allogus, env, f = presence ~ layer.1 + layer.2, test.prop = 0.2)
plot(allogus.glm)
allogus.glm$test.evaluation
visualize.enm(allogus.glm, env, layers = c("layer.1", "layer.2"))

allogus.gam <- enmtools.gam(allogus, env[[c("layer.1", "layer.2")]], test.prop = 0.2)
plot(allogus.gam)
allogus.gam$test.evaluation
visualize.enm(allogus.gam, env, layers = c("layer.1", "layer.2"))

allogus.mx <- enmtools.maxent(allogus, env[[c("layer.1", "layer.2")]], test.prop = 0.2)
plot(allogus.mx)
allogus.mx$test.evaluation
visualize.enm(allogus.mx, env, layers = c("layer.1", "layer.2"))

allogus.dm <- enmtools.dm(allogus, env[[c("layer.1", "layer.2")]], test.prop = 0.2)
plot(allogus.dm)
allogus.dm$test.evaluation
visualize.enm(allogus.dm, env, layers = c("layer.1", "layer.2"))

allogus.bc <- enmtools.bc(allogus, env[[c("layer.1", "layer.2")]], test.prop = 0.2)
plot(allogus.bc)
allogus.bc$test.evaluation
visualize.enm(allogus.bc, env, layers = c("layer.1", "layer.2"))
