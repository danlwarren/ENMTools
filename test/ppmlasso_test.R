setwd("test")
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

test1 <- enmtools.ppmlasso(ahli, env)
test1
plot(test1, trans = 'sqrt')

test2 <- enmtools.ppmlasso(allogus, env)
test2
plot(test2, trans = 'sqrt')

test3 <- enmtools.ppmlasso(ahli, env, back.accurate = TRUE)
test3
plot(test3, trans = 'sqrt')

test4 <- enmtools.ppmlasso(allogus, env, back.accurate = TRUE)
test4
plot(test4, trans = 'sqrt')

test5 <- enmtools.ppmlasso(ahli, env, normalise = TRUE)
test5
plot(test5, trans = 'sqrt')

test6 <- enmtools.ppmlasso(allogus, env, normalise = TRUE)
test6
plot(test6, trans = 'sqrt')

test7 <- enmtools.ppmlasso(ahli, env, test.prop = 0.33)
test7
plot(test7, trans = 'sqrt')

test8 <- enmtools.ppmlasso(allogus, env, test.prop = 0.33)
test8
plot(test8, trans = 'sqrt')

## This does not work. Looks like the problem is with calc.B1. -Inf is common. Will test a solution.
envtest1 <- env.breadth(test1, env, max.reps = 10000)
