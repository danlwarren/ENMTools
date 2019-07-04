<<<<<<< HEAD
library(MuMIn)
=======

library(ENMTools)
>>>>>>> master
setwd("~/GitHub/ENMTools/test")



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



<<<<<<< HEAD
test <- moses.list(phyl.clade$species, env)
=======
test <- moses.list(phyl.clade$species, env, test.prop = 0.2)
>>>>>>> master

# background <- background[sample(length(background[,1]), length(phyltable[,1]), replace=TRUE),3:24]
# background <- cbind(phyltable[,1:2], background)
# alltable <- rbind(phyltable, background)
# #alltable now contains a table with presence and absence points for each species
# #Species are under column "Lineage"
#
#
# # These bits just show how to get clade membership from a tree
# membership.matrix <- clade.matrix(tree)
# membership.matrix$clade.matrix
#
# #This function takes a clade matrix and returns the unique comparisons
# unique.clades <- function(x){
#     output <- matrix(ncol=ncol(x), nrow=0)
#     for(i in 1:length(x[,1]))
#     {
#         keep <- TRUE
#         if(i > 1){
#             for(j in 1:length(output[,1])){
#                 if(all(x[i,] == abs(1 - output[j,]))){
#                     keep <- FALSE
#                 }
#             }
#         }
#         if(keep == TRUE){output <- rbind(output, x[i,])}
#     }
#     output
# }
#
# # You can use geiger and just do phyltable[phyltable$Lineage %in% tips(tree,14),]
# # to get a phyltable that only has presences from the tips descended from node 14!
#
# test.subsets <- function(tree, this.table){
#     for(i in 1:(length(tree$edge)/2 + 1)){
#         desc <- tree$edge[tree$edge[,1] == i,2]
#         print(i)
#         if(length(desc) > 0){
#             #From this point it would be easy for dichotomous nodes, e.g.,
#             #
#             #mergetest(
#             #    x=list(this.table[this.table$Lineage %in% desc[1],], this.table[this.table$Lineage %in% desc[1],]),
#             #    formula = whatever, critvalue="mean", style="multi", family = "binomial", level = 1, method = "g"
#             #)
#             #
#             # But I need to figure out how to do it for polytomies.  I think mergetest
#             # can already handle polytomies, but I need to figure out how to build the list to feed it as an
#             # argument.
#             for(j in 1:length(desc)){print(tips(tree, desc[j]))}
#         }
#     }
# }
# test.subsets(tree)
