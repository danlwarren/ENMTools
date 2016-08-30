rm(list=ls(all=TRUE))

library(ENMTools)
setwd("~/GitHub/ENMTools/test")
library(phyloclim)

env.files <- list.files("~/GitHub/SDM-Sim/Australia CliMond/", full.names = TRUE)
env <- stack(env.files[1])[[1:19]]
newext <- extent(144, 160, -40, -10)
env <- crop(env, newext)

format.latlon <- function(latlon){

  loncols <- c(which(grepl("^lon", colnames(latlon), ignore.case = TRUE)), match("x", tolower(colnames(latlon))))
  latcols <- c(which(grepl("^lat", colnames(latlon), ignore.case = TRUE)), match("y", tolower(colnames(latlon))))

  loncols <- loncols[which(!is.na(loncols))]
  latcols <- latcols[which(!is.na(latcols))]

  # Check whether we've got one column for each, and make sure they're not the same column
  if(length(latcols == 1) & length(loncols == 1) & latcols != loncols){
    output <- data.frame(cbind(latlon[,loncols], latlon[,latcols]))
    colnames(output) <- c("Longitude", "Latitude")
  } else {
    stop("Unable to auotmatically determine Longitude and Latitude columns.  Please rename to Longitude and Latitude.")
  }

  return(output)

}

phyltable <- read.csv("~/Dropbox/ongoing projects/Model Selection Niche Modeling/Moses/env_at_Phyllurus_lin_loc_goodrecords.csv", header=TRUE)
# background <- read.csv("~/Dropbox/ongoing projects/Model Selection Niche Modeling/Moses/env_at_gekkonidae_sites.csv", header=TRUE)
# background <- background[complete.cases(background),]
phyltable <- phyltable[phyltable$Use == 1,]
tree <- read.tree(file="phyllurus.tre")
tree <- drop.tip(tree, tree$tip.label[!tree$tip.label %in% phyltable$Species])
tree <- compute.brlen(tree)
plot(tree)


# Should I write a function to auto-build clades from a csv and a tree?
for(i in tree$tip.label){
  print(i)
  this.p <- format.latlon(phyltable[phyltable$Species == i,c("Long", "Lat")])
  assign(i, enmtools.species(presence.points = this.p,
                             species.name = i))
}

phyl.clade <- enmtools.clade(species = list(amnicola = amnicola,
                                            platurus = platurus,
                                            caudiannulatus = caudiannulatus,
                                            gulbaru = gulbaru,
                                            nephthys = nephthys,
                                            championae = championae,
                                            ossa = ossa,
                                            isis = isis),
                             tree = tree)

overlap <- data.frame(matrix(NA, nrow = length(tree$tip.label), ncol = length(tree$tip.label)))
colnames(overlap) <- rownames(overlap) <- tree$tip.label

# Loop through clade, get overlaps
for(i in 1:length(names(phyl.clade$species))){
  for(j in i:length(names(phyl.clade$species))){

    print(paste(names(phyl.clade$species)[i], "vs.", names(phyl.clade$species)[j]))

    if(i == j){
      this.overlap <- 1
    } else {
      this.overlap <- raster.overlap(enmtools.glm(phyl.clade$species[[i]], env),
                                     enmtools.glm(phyl.clade$species[[j]], env))
    }
    overlap[i,j] <- this.overlap
  }
}

write.csv(overlap, "phyl.overlaps.csv")

phyloverlaps <- read.csv("phyl.overlaps.csv")
rownames(phyloverlaps) <- phyloverlaps[,1]
phyloverlaps <- phyloverlaps[,-1]

node.overlap(phyloverlaps, tree)





test.tree <- compute.brlen(read.tree(text = "((((A,B,C,D),E,(F,G)),(H,I)),J);"))
test.overlap <- matrix(data = 1, nrow = 10, ncol = 10)
colnames(test.overlap) <- rownames(test.overlap) <- test.tree$tip.label
node.overlap(overlap = test.overlap, tree = test.tree)
test <- age.overlap.correlation(test.overlap, test.tree, 50)

# SHould all be ones
node.overlap(overlap = test.overlap, tree = test.tree)

test.tree <- compute.brlen(rtree(50))
test.overlap <- matrix(data = runif(2500), nrow = 50, ncol = 50)
colnames(test.overlap) <- rownames(test.overlap) <- test.tree$tip.label
system.time(test1 <- age.overlap.correlation(overlap = test.overlap, tree = test.tree, nreps = 10))
system.time(test2 <- age.range.correlation(test.tree, test.overlap, n = 10))


ntax <- rep(seq(4, 60, by = 4))
newtimes <- rep(NA, length(ntax))
oldtimes <- rep(NA, length(ntax))
for(i in 1:length(ntax)){
  print(ntax[i])
  this.tree <- compute.brlen(rtree(ntax[i]))
  this.overlap <- data.frame(matrix(data = runif(ntax[i] ** 2), nrow = ntax[i], ncol = ntax[i]))
  colnames(this.overlap) <- rownames(this.overlap) <- this.tree$tip.label
  newtimes[i] <- system.time(test1 <- age.overlap.correlation(this.overlap, this.tree, 100))
  oldtimes[i] <- system.time(test2 <- age.range.correlation(this.tree, this.overlap, n = 100))
  print(paste(newtimes[i], oldtimes[i]))
  print(all(test1$empirical.df[,2] == test2[,2]))
}

qplot(oldtimes, newtimes) + geom_abline(slope = 1, intercept = 0)
