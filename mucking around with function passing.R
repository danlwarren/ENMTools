library(dismo)
library(ENMTools)
library(rgeos)

env <- stack(list.files("/Users/danwarren/GitHub/ENMTools/ex/", ".asc", full.names = TRUE))

ahli.pa <- cbind(rep(1, nrow(ahli)), ahli[,2:3])
ahli.back <- cbind(rep(0, 1000),
  background.buffer(ahli[,2:3], radius = 10000, n = 1000, mask = env[[1]]))

colnames(ahli.pa) <- c("presence", "lon", "lat")
colnames(ahli.back) <- c("presence", "lon", "lat")
ahli.pa <- rbind(ahli.pa, ahli.back)

allogus_east.pa <- cbind(rep(1, nrow(allogus_east)), allogus_east[,2:3])
allogus_east.back <- cbind(rep(0, 1000),
                   background.buffer(allogus_east[,2:3], radius = 10000, n = 1000, mask = env[[1]]))

colnames(allogus_east.pa) <- c("presence", "lon", "lat")
colnames(allogus_east.back) <- c("presence", "lon", "lat")
allogus_east.pa <- rbind(allogus_east.pa, allogus_east.back)

ahli.pa <- cbind(ahli.pa, extract(env[[3:6]], ahli.pa[,2:3]))
allogus_east.pa <- cbind(allogus_east.pa, extract(env[[3:6]], allogus_east.pa[,2:3]))

ahli.model <- glm(presence ~ ., data = ahli.pa[,-c(2,3)])

glm.function <- function(this.data, this.glm){
  this.glm(this.data)
}

glm2 <- function(x){
  glm(presence ~ ., data =  x[,-c(2:3)])
}

glm.function(ahli.pa, function(x) glm(presence ~ ., data =  x[,-c(2:3)]))
glm.function(allogus_east.pa, glm2)

#' General structure of background test function:
#'  Take two data frames of presence points, args for backgrounding, model function as above
#'
#'  background.test(ahli, allogus_east, env, nback = 1000, radius = 100000, nreps,
#'                  function(x) glm(presence ~ ., data = x[,-c(2,3)]))
#'
#'  Sample background for real points, build models, predict, and measure overlaps
#'  Do reps.identity nreps time and repeat steps
#'  return:
#'    qplot density plot or histogram with empirical value as vline
#'    return df of empirical overlaps using I, D, Baumgartner's stuff, others
#'    return same for reps
#'    Optionally spit an Rmd/html file summarizing run, maybe pics for each rep
