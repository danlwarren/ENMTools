#######################################################
# ENMTools example script
# Modified from:
# 
# https://github.com/danlwarren/ENMTools#interacting-with-enmtools
# 
# ...mostly to make it runnable from-scratch (with just
#    an internet connection)
#######################################################


library(devtools)	# install_github
#install_github("danlwarren/ENMTools")
library(ENMTools)	# hierarchical species distribution modeling
# enmtools.aoc


library(ape)
library(rgbif)
library(phyloclim) # for e.g. phyloclim:::descendants

library(BioGeoBEARS)
sourceall("/GitHub/ENMTools/R/")

wd = "/GitHub/ENMTools/inst/extdata/"
setwd(wd)

# Download only first time you run, then change to FALSE
download_data_files = FALSE
# Species occurrences from a CSV file (from ENMTools GitHub page)
remote_dir = "https://raw.githubusercontent.com/danlwarren/ENMTools/master/test/testdata/"
# Species occurrences from a LOCAL file (on your computer)
local_dir = "tmp"



# Create an empty species object
ahli = enmtools.species()
ahli

# Load a species object with occurrence data
spnames = c("ahli", "allogus")
species_occs_fns = paste0(spnames, ".csv")
remote_fns = paste0(remote_dir, species_occs_fns)
local_fns = paste0(local_dir, "/", species_occs_fns)
#local_fn = "hi/this/is/my/folder/and_file_name.csv" # for a file you have on disk
if (file.exists(local_dir) == FALSE)
	{
	dir.create(local_dir)
	}

# Downloading the file. You may need to change the 'method' argument
# on some operating systems
if (download_data_files == TRUE)
	{
	for (i in 1:length(remote_fns))
		{
		download.file(url=remote_fns[i], destfile=local_fns[i], method="curl")
		}
	}

# Read in the occurrences files
for (i in 1:length(local_fns))
	{
	occs = read.csv(file=local_fns[i])

	# Make an ENMTools species object
	cmdtxt = paste0(spnames[i], " = enmtools.species(species.name='", spnames[i], "', presence.points=occs[,3:4])")
	eval(parse(text=cmdtxt))
	#ahli = enmtools.species(species.name="ahli", presence.points=occs[,3:4])
	#check.species(ahli)
	}



#######################################################
# Download and load environmental data (ASCII rasters)
# (from ENMTools GitHub page)
#######################################################
env_fns = c("pc1.asc", "pc2.asc", "pc3.asc", "pc4.asc")
cat("\nDownloading ", length(env_fns), "...", sep="")
for (i in 1:length(env_fns))
	{
	env_fn = env_fns[i]
	cat("\nFile #", i, "/", length(env_fns), ": ", env_fn, "...\n", sep="")
	remote_fn = paste0(remote_dir, env_fn)
	local_fn = paste0(local_dir, "/", env_fn)
	if (download_data_files == TRUE)
		{
		download.file(url=remote_fn, destfile=local_fn, method="curl", quiet=FALSE)
		}
	#cat("done.")
	} # END for (i in 1:length(env_fns))

local_env_fns <- list.files(path=local_dir, pattern="pc", full.names=TRUE)
local_env_fns
env <- raster::stack(local_env_fns)
names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4")
env <- raster::setMinMax(env)
orig_env = env

#######################################################
# Assign background points
#######################################################
radius = 20000
n = 1000
for (i in 1:length(spnames))
	{
	# Let's use the new defaults.
	#ahli$background.points = background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=FALSE)
	cmdtxt = paste0(spnames[i], "$background.points = background.points.buffer(points=", spnames[i], "$presence.points, radius=", radius, ", n=", n, ", mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type='random', extract_values=FALSE)")
	eval(parse(text=cmdtxt))
	} # END for (i in 1:length(spnames))


#######################################################
# Assign background raster
#######################################################
cat("Assigning background raster for ", length(spnames), " species...", sep="")
for (i in 1:length(spnames))
	{
	cat("\nProcessing species #", i, "/", length(spnames), ": ", spnames[i], "...", sep="")
	
	# Let's use the new defaults.
	#ahli$range = background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=FALSE)
	cmdtxt = paste0(spnames[i], "$range = background.raster.buffer(points=", spnames[i], "$presence.points, radius=", radius, ", mask=env, cropfirst=FALSE)")
	eval(parse(text=cmdtxt))
	} # END for (i in 1:length(spnames))
cat("finished.\n")



#######################################################
# RUN A BUNCH OF SLOW ANALYSES, OR LOAD THEM,
# IF RUN BEFORE
#######################################################

runslow = TRUE
if (runslow == TRUE) {


#######################################################
# Building an ENM for each species
#######################################################
ahli.glm_linear <- enmtools.glm(species = ahli, env = env, f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, test.prop = 0.2)
allogus.glm_linear <- enmtools.glm(species = allogus, env = env, f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, test.prop = 0.2)

# Estimated responses
ahli.glm_linear$response.plots
allogus.glm_linear$response.plots


#######################################################
# Building an ENM for each species, with polynomial effects (not linear)
#######################################################
ahli.glm <- enmtools.glm(species = ahli, env = env, f = pres ~ poly(layer.1, 2) + poly(layer.2, 2) + poly(layer.3, 2) + poly(layer.4, 2), test.prop = 0.2)
allogus.glm <- enmtools.glm(species = allogus, env = env, f = pres ~ poly(layer.1, 2) + poly(layer.2, 2) + poly(layer.3, 2) + poly(layer.4, 2), test.prop = 0.2)

# Estimated responses
pdffn = "ahli.glm_response.plots.pdf"
pdf(file=pdffn, width=6, height=6)
ahli.glm$response.plots
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)

pdffn = "allogus.glm_response.plots.pdf"
pdf(file=pdffn, width=6, height=6)
allogus.glm$response.plots
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)


# The colored raster of the background.plot shows you the density of 
# background points in environment space, while the white points again 
# represent your occurrence points in environment space.
pdffn = "visualize_ahli.glm.pdf"
pdf(file=pdffn, width=6, height=6)
visualize.enm(ahli.glm, env, layers = c("layer.2", "layer.4"))
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)


pdffn = "visualize_allogus.glm.pdf"
pdf(file=pdffn, width=6, height=6)
visualize.enm(allogus.glm, env, layers = c("layer.2", "layer.4"))
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)



# GAM, Bioclim, Domain, and Maxent
# 
# The procedure for building Bioclim, Domain, and Maxent models is similar to the
# procedure for GLMs, with the exception that you do not need to pass a formula to
# the model function. Note that running Maxent models requires a bit of extra setup;
# see dismo documentation for details.
# 
ahli.gam <- enmtools.gam(ahli, env, test.prop = 0.2)
ahli.dm <- enmtools.dm(ahli, env, test.prop = 0.2)
ahli.bc <- enmtools.bc(ahli, env, test.prop = 0.2)
ahli.mx <- enmtools.maxent(ahli, env, test.prop = 0.2)

allogus.gam <- enmtools.gam(allogus, env, test.prop = 0.2)
allogus.dm <- enmtools.dm(allogus, env, test.prop = 0.2)
allogus.bc <- enmtools.bc(allogus, env, test.prop = 0.2)
allogus.mx <- enmtools.maxent(allogus, env, test.prop = 0.2)


#####################################################
# Metrics: breadth, correlation, and overlap
#####################################################
# Metrics: breadth, correlation, and overlap
# 
# ENMTools provides a number of metrics for ENMs and for similarities between ENMs.
# These include measures of niche breadth, based on Levins(1968). An important
# caveat when interpreting these metrics is that they are driven to some (variable)
# extent by the availability of different combinations of environmental variables.
# As such they are more accurately interpreted as a measurment of the smoothness of
# the geographic distribution of suitability scores than as an estimate of the
# breadth of the fundamental niche; an organism with narrow fundamental niche
# breadth that nonetheless encompasses a set of environmental conditions that is
# quite common will have a high breadth when measured using ENMs, while having a low
# breadth in environment space.
raster_breadth_result <- ENMTools::raster.breadth(x=ahli.glm)
raster_breadth_result


# ENMTools also provides metrics for measuring similarity between ENMs. These
# include Schoener's D (Schoener 1968), I (Warren et al. 2008), and the Spearman
# rank correlation coefficient between two rasters. While D and I are commonly used
# in the ENM literature, they may tend to overestimate similarity between ENMs when
# many grid cells are of similar values (e.g., when two species prefer different
# habitat but the region contains a great deal of habitat that is unsuitable for
# both).
raster_overlap_result <- ENMTools::raster.overlap(x=ahli.glm, y=allogus.glm)
raster_overlap_result


# A new feature of the R version of ENMTools is that you can now use these same
# metrics in the n-dimensional space of all combinations of environmental variables,
# instead of restricting your measures of model similarity to those sets of
# conditions that appear in the training region. This is done by repeatedly drawing
# Latin hypercube samples from the space of all possible combinations of
# environmental variables given the min and max of each variable within the training
# region. ENMTools continues to draw samples until subsequent iterations differ by
# less than a specified tolerance value. Lower tolerance values result in more
# precise estimates of overlap, but can take much longer to calculate.
env_overlap_result <- ENMTools::env.overlap(model.1=ahli.glm, model.2=allogus.glm, env=env, tolerance=.001)
env_overlap_result








# Hypothesis testing
# 
# Niche identity or equivalency test
# 
# In this example, we will run a niche identity (also called equivalency) test, as in
# Warren et al. 2008. This test takes the presence points for a pair of species and
# randomly reassigns them to each species, then builds ENMs for these randomized
# occurrences. By doing this many times, we can estimate the probability distribution
# for ENM overlap between species under the null hypothesis that the two species'
# occurrences in the environment are effectively a random draw from the same
# underlying distribution. Note that niche evolution is only one of many reasons why
# two species' realized environmental distributions might cause departures from this
# null hypothesis. See Warren et al. 2014 for details.
# 
# To run an identity test, we need to decide what type of models we will build, how
# many replicates we will run, and (in the case of GLM) a model formula to use for
# empirical models and the Monte Carlo replicates. The resulting object contains the
# replicate models, p values, and plots of the results. Typically idenity tests are
# run with at least 99 replicates, but we are using a smaller number here for the sake
# of execution time.
# 
# NOTE: In order for models to be comparable, both empirical and pseudoreplicate
# models for the identity test are conducted with pseudoabsence points pooled for the
# two species being compared.
id.glm <- identity.test(species.1=ahli, species.2=allogus, env=env, type="glm", nreps=4)
pdffn = "id.glm.pdf"
pdf(file=pdffn, width=12, height=12)
id.glm 
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)



# Background or similarity test
# 
# The background or similarity test compares the overlap seen between two species'
# ENMs to the overlap expected by chance if one or both species was effectively
# choosing habitat at random from within their broad geographic range. The purpose of
# this test is to correct for the availability of habitat and ask whether the observed
# similarity between species or populations is significantly more (or less) than
# expected given the available set of environments in the regions in which they occur.
# 
# NOTE: In order for models to be comparable, both empirical and pseudoreplicate
# models for the background test are conducted with pseudoabsence points pooled for
# the two species being compared.
# 
# In Warren et al. 2008, we developed this test in the context of comparing one
# species' actual occurrence to the random background occurrences of the other
# species. This is what we call an "asymmetric" test, and in our case we did the test
# in both directions with the idea that we might compare the results of A vs. B
# background to the results of B vs. A background. This may be informative in some
# cases, but many people have also found this asymmetry confusing (and indeed it is
# often difficult to interpret). For that reason, the background test here can be
# conducted against a null hypothesis that is generated from "asymmetric" (species.1
# vs species.2 background) or "symmetric" (species.1 background vs. species.2
# background) comparisons.
# 
# Here, for instance, is a Bioclim background test using the classical asymmetric approach:
bg.bc.asym <- background.test(species.1=ahli, species.2=allogus, env=env, type="bc", nreps=4, test.type="asymmetric" )

pdffn = "bg.bc.asym.pdf"
pdf(file=pdffn, width=12, height=12)
bg.bc.asym
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)




# And here is a Domain background test using the symmetric approach:
bg.dm.sym <- background.test(species.1=ahli, species.2=allogus, env=env, type="dm", nreps=4, test.type="symmetric")

pdffn = "bg.dm.sym.pdf"
pdf(file=pdffn, width=12, height=12)
bg.dm.sym
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)





# Ecospat tests
# 
# Using enmtools.species objects also provides a simplified interface to the 
# niche equivalency and similarity tests (or identity and background tests, 
# respectively) that were developed by Broennimann et al. (2012). These tests 
# do not rely on ENMs, instead using kernel density smoothing to estimate 
# density of the species in environment space. Ecospat also uses the density 
# of the available environment to correct for availability when measuring 
# overlaps, so that overlaps are not strictly driven by availability of 
# combinations of environmental variables.
# 
# These tests only work with two environmental axes, so they are often done with 
# the top two PC axes of a set of environments. In our case we'll just pick a 
# couple of environmental layers, though (layer.1 and layer.2). Here's an 
# equivalency/identity test:
# nreps = 99; layers = NULL; th.sp=0; th.env=0; R=100
# species.1=ahli; species.2=allogus; env=env[[c("layer.1", "layer.2")]]
esp.id <- enmtools.ecospat.id(species.1=ahli, species.2=allogus, env=env[[c("layer.1", "layer.2")]])

pdffn = "esp.id.pdf"
pdf(file=pdffn, width=12, height=12)
esp.id
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)




# And here's a symmetric background test. The difference between symmetric and 
# asymmetric for these tests is the same as for the background tests presented above.
# species.1=ahli; species.2=allogus; env=env[[c("layer.1", "layer.3")]]; test.type="symmetric"
# nreps = 99; layers = NULL; th.sp=0; th.env=0; R=100
esp.bg.sym <- enmtools.ecospat.bg(ahli, allogus, env[[c("layer.1", "layer.3")]], test.type="symmetric")

pdffn = "esp.bg.sym.pdf"
pdf(file=pdffn, width=12, height=12)
esp.bg.sym
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)




# Rangebreak tests
# 
# ENMTools also allows you to perform linear, blob, and ribbon rangebreak tests as
# developed in Glor and Warren 2011. The linear and blob tests are two versions of a test
# that permit one to ask whether the geographic regions occupied by two species are more
# environmentally different than expected by chance. The ribbon test, meanwhile, is
# designed to test whether the ranges of two species are divided by a region that is
# relatively unsuitable to one or both forms.
# 
# For the linear and blob tests, you call them very much like you would the identity and
# background tests. Here's a linear one using GLM models:
rbl.glm <- rangebreak.linear(species.1=ahli, species.2=allogus, env=env, type="glm", nreps=4, nback=1000)

pdffn = "rbl.glm.pdf"
pdf(file=pdffn, width=12, height=12)
rbl.glm
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)




# And here's a blob test using Bioclim:
rbb.bc <- rangebreak.blob(species.1=ahli, species.2=allogus, env=env, type="bc", nreps=4, nback=1000)
pdffn = "rbb.bc.pdf"
pdf(file=pdffn, width=12, height=12)
rbb.bc
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)




# If you want to access the individual replicates (for instance to see 
# how your ranges are being split up), you can find them in the list 
# named "replicate.models" inside your rangebreak test object.
rbl.glm$replicate.models$ahli.rep.1
rbl.glm$replicate.models$allogus.rep.1


# For the ribbon rangebreak test, you will need one extra thing; a third
# enmtools.species object representing the occurrence points (for one or both species)
# that fall within the ribbon of putatively unsuitable habitat. In the case of these
# two anoles we don't have such a ribbon, so we'll just simulate one based on some
# random points.
ribbon <- enmtools.species(species.name="ribbon")
ribbon$presence.points <- data.frame(Longitude=runif(n=10, min=-79, max=-78.5),
                                      Latitude=runif(n=10, min=21.7, max=22.1))
plot(env[[1]])
points(ribbon$presence.points, pch=16)

ribbon$range <- background.raster.buffer(points=ribbon$presence.points, radius=20000, mask=env, cropfirst=FALSE)

pdffn = "ribbon.pdf"
pdf(file=pdffn, width=12, height=12)
ribbon
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)




# Now we'll run a ribbon rangebreak test using GLM models with quadratic effects. We
# also need to tell it the width of the ribbons to generate for the replicates. The
# units for the width argument are the same units that the presence points are in; e.g.,
# if the points are in decimal degrees you should supply the width of the barrier in
# decimal degrees.
rbr.glm <- rangebreak.ribbon(species.1=ahli, species.2=allogus, ribbon=ribbon, env=env, type="glm", f=pres ~ poly(layer.1, 2) + poly(layer.2, 2) + poly(layer.3, 2) + poly(layer.4, 2), width=0.5, nreps=4, nback=1000)

pdffn = "rbr.glm.pdf"
pdf(file=pdffn, width=12, height=12)
rbr.glm
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)



# Note that the output table here has slope, intercept, and intercept offset.
rbr.glm$lines.df

# The intercept denotes the intercept corresponding to the CENTER of each ribbon. To get
# the lines denoting the edges of the ribbons (for example if you want to plot the
# ribbons on a map), you add and substract the offset. In other words, the top edge of
# the ribbon is given by y=(slope * x) + intercept + offset, while the bottom edge is
# given by y=(slope * x) + intercept - offset.
 


#######################################################
# Building an enmtools.clade object
#######################################################
# 
# Some of the tests in ENMTools, including some really neat ones that are still in
# development, require you to build an enmtools.clade object. These objects are simply
# lists that contain a phylogeny and a set of enmtools.species objects. It's important
# that the names of the species objects and their species.name attributes match the names
# in the phylogeny's tip.labels. For demonstration, we're going to build an object for a
# clade of five anoles from Hispaniola. We have the tree, so we're just going to grab
# occurrence data from GBIF using the rgbif package.

NEXUS_fn = "StarBEAST_MCC.species.txt"
remote_NEXUS_fn = paste0(remote_dir, NEXUS_fn)
local_NEXUS_fn = paste0(local_dir, "/", NEXUS_fn)
#local_fn="hi/this/is/my/folder/and_file_name.csv" # for a file you have on disk

# Downloading the file. You may need to change the 'method' argument
# on some operating systems
if (download_data_files == TRUE)
	{
	download.file(url=remote_NEXUS_fn, destfile=local_NEXUS_fn, method="curl")
	}



# Read in the phylogeny data
hisp.anoles <- read.nexus(file=local_NEXUS_fn)
keepers <- c("brevirostris", "marron", "caudalis", "websteri", "distichus")
hisp.anoles <- drop.tip(phy=hisp.anoles, tip=hisp.anoles$tip.label[!hisp.anoles$tip.label %in% keepers])
plot(hisp.anoles)
# So there's our tree. 

# Now we're going to grab some environmental data.
dir_name = "Hispaniola_WorldClim"
remote_dir_name = paste0(remote_dir, dir_name)
local_dir_name = paste0(local_dir, "/", dir_name)
if (file.exists(local_dir_name) == FALSE)
	{
	dir.create(local_dir_name)
	}

# Download from ENMTools GitHub page
ascii_fns = c("snv_1.asc", "snv_2.asc", "snv_3.asc", "snv_4.asc", "snv_5.asc", "snv_6.asc", "snv_7.asc", "snv_8.asc", "snv_9.asc", "snv_10.asc", "snv_11.asc", "snv_12.asc", "snv_13.asc", "snv_14.asc", "snv_15.asc", "snv_16.asc", "snv_17.asc", "snv_18.asc", "snv_19.asc", "snv_20.asc")
remote_ascii_fns = paste0(remote_dir_name, "/", ascii_fns)
local_ascii_fns = paste0(local_dir_name, "/", ascii_fns)
if (download_data_files == TRUE)
	{
	for (i in 1:length(remote_ascii_fns))
		{
		download.file(url=remote_ascii_fns[i], destfile=local_ascii_fns[i], method="curl")
		}
	} # END if (download_data_files == TRUE)

hisp.env <- stack(list.files(local_dir_name, full.names=TRUE))
hisp.env <- setMinMax(hisp.env)



# And then we'll create a function to build species from GBIF.

# Automate the process of downloading data and removing duds and dupes
species.from.gbif <- function(genus, species, name=NA, env)
	{
  # Name it after the species epithet unless told otherwise
  if(is.na(name))
  	{
    name <- species
  	}

  # Get GBIF data
  this.sp <- enmtools.species(presence.points=gbif(genus=genus, species=species)[,c("lon", "lat")],
                              species.name=name)

  # Rename columns, get rid of duds
  colnames(this.sp$presence.points) <- c("Longitude", "Latitude")
  this.sp$presence.points <- this.sp$presence.points[complete.cases(extract(env, this.sp$presence.points)),]
  this.sp$presence.points <- this.sp$presence.points[!duplicated(this.sp$presence.points),]

  this.sp$range <- background.raster.buffer(this.sp$presence.points, 50000, mask=hisp.env)

  return(this.sp)
	} # END species.from.gbif <- function(genus, species, name=NA, env)


# Now we'll create five species and add them to a species.clade object that is called brev.clade.
brevirostris <- species.from.gbif(genus="Anolis", species="brevirostris", env=hisp.env)
marron <- species.from.gbif(genus="Anolis", species="marron", env=hisp.env)
caudalis <- species.from.gbif(genus="Anolis", species="caudalis", env=hisp.env)
websteri <- species.from.gbif(genus="Anolis", species="websteri", env=hisp.env)
distichus <- species.from.gbif(genus="Anolis", species="distichus", env=hisp.env)

brev.clade <- enmtools.clade(species=list(brevirostris, marron, caudalis, websteri, distichus), tree=hisp.anoles)
check.clade(brev.clade)


# Age-overlap correlation tests (AOC)
# 
# The AOC tests allow you to examine patterns of range, point, and ENM overlap in the
# context of a phylogeny. This is effectively a generalized version of several analyses:
# age-range correlation (e.g., Fitzpatrick and Turelli 2006), ENM overlap in the context
# of a phylogeny (e.g., Knouft et al. 2006, Warren et al. 2008), and point overlaps
# (e.g., Cardillo and Warren 2016).
# 
# These tests require the creation of an enmtools.clade object, as above. AOC tests
# consist of two steps: first, the average overlap at each node in the phylogeny is
# calcualted using a method that takes tree topology into account (see Fitzpatrick and
# Turelli 2006), then we perform a linear regression to measure the relationship between
# node age and average overlap. Due to the fact that these overlaps violate many of the
# assumptions of a regular linear regression, however (e.g., errors are not iid), we
# can't calculate significance in the typical way. Instead we performa Monte Carlo test,
# permuting the identity of the tips of the tree and repeating the node averaging and
# modeling steps. Finally we measure statistical significance by comparing the empirical
# slope and intercept to the distribution of slopes and intercepts from the Monte Carlo
# replicates.
# 
# First, let's do one using range overlaps, as in Fitzpatrick and Turelli 2006. Note that
# this analysis requires that each of your species have a range raster stored in their
# species object (we did that as part of the function used above).

# clade=brev.clade;  nreps=50; overlap.source="range"
# env=NULL;  model=NULL; overlap.matrix=NULL; metric="D"
range.aoc <- enmtools.aoc(clade=brev.clade,  nreps=50, overlap.source="range")
summary(range.aoc)

# Now we can do one using point overlaps just by changing the overlap.source argument:
point.aoc <- enmtools.aoc(clade=brev.clade,  nreps=50, overlap.source="points")
summary(point.aoc)

# Or we can use similarity between ENMs built for each species. Here we'll use Bioclim models:
bc.aoc <- enmtools.aoc(clade=brev.clade,  env=hisp.env, nreps=50, overlap.source="bc")

pdffn = "bc.aoc.pdf"
pdf(file=pdffn, width=12, height=12)
bc.aoc
dev.off()
cmdstr = paste0("open ", pdffn)
system(cmdstr)



# Save so that we don't have to re-run them...
save(ahli.glm_linear, file="tmp/ahli.glm_linear.Rdata")
save(allogus.glm_linear, file="tmp/allogus.glm_linear.Rdata")

save(ahli.glm, file="tmp/ahli.glm.Rdata")
save(ahli.gam, file="tmp/ahli.gam.Rdata")
save(ahli.dm, file="tmp/ahli.dm.Rdata")
save(ahli.bc, file="tmp/ahli.bc.Rdata")
save(ahli.mx, file="tmp/ahli.mx.Rdata")

save(allogus.glm, file="tmp/allogus.glm.Rdata")
save(allogus.gam, file="tmp/allogus.gam.Rdata")
save(allogus.dm, file="tmp/allogus.dm.Rdata")
save(allogus.bc, file="tmp/allogus.bc.Rdata")
save(allogus.mx, file="tmp/allogus.mx.Rdata")

save(raster_breadth_result, file="tmp/raster_breadth_result.Rdata")
save(raster_overlap_result, file="tmp/raster_overlap_result.Rdata")
save(env_overlap_result, file="tmp/env_overlap_result.Rdata")


save(id.glm, file="tmp/id.glm.Rdata")
save(bg.bc.asym, file="tmp/bg.bc.asym.Rdata")
save(bg.dm.sym, file="tmp/bg.dm.sym.Rdata")
save(esp.id, file="tmp/esp.id.Rdata")
save(esp.bg.sym, file="tmp/esp.bg.sym.Rdata")
save(rbl.glm, file="tmp/rbl.glm.Rdata")

save(rbb.bc, file="tmp/rbb.bc.Rdata")
save(ribbon, file="tmp/ribbon.Rdata")
save(rbr.glm, file="tmp/rbr.glm.Rdata")
save(brevirostris, file="tmp/brevirostris.Rdata")
save(marron, file="tmp/marron.Rdata")
save(caudalis, file="tmp/caudalis.Rdata")
save(websteri, file="tmp/websteri.Rdata")
save(distichus, file="tmp/distichus.Rdata")
save(brev.clade, file="tmp/brev.clade.Rdata")
save(range.aoc, file="tmp/range.aoc.Rdata")
save(point.aoc, file="tmp/point.aoc.Rdata")
save(bc.aoc, file="tmp/bc.aoc.Rdata")
#save(, file="tmp/.Rdata")
#save(, file="tmp/.Rdata")
#save(, file="tmp/.Rdata")

} else {
# Loads to the file prefix

load(file="tmp/ahli.glm_linear")
load(file="tmp/allogus.glm_linear")

load(file="tmp/ahli.glm")
load(file="tmp/ahli.gam")
load(file="tmp/ahli.dm")
load(file="tmp/ahli.bc")
load(file="tmp/ahli.mx")

load(file="tmp/allogus.glm")
load(file="tmp/allogus.gam")
load(file="tmp/allogus.dm")
load(file="tmp/allogus.bc")
load(file="tmp/allogus.mx")

load(file="tmp/raster_breadth_result")
load(file="tmp/raster_overlap_result")
load(file="tmp/env_overlap_result")

load(file="tmp/id.glm.Rdata")
load(file="tmp/bg.bc.asym.Rdata")
load(file="tmp/bg.dm.sym.Rdata")
load(file="tmp/esp.id.Rdata")
load(file="tmp/esp.bg.sym.Rdata")
load(file="tmp/rbl.glm.Rdata")
load(file="tmp/rbb.bc.Rdata")
load(file="tmp/ribbon.Rdata")
load(file="tmp/rbr.glm.Rdata")
load(file="tmp/brevirostris.Rdata")
load(file="tmp/marron.Rdata")
load(file="tmp/caudalis.Rdata")
load(file="tmp/websteri.Rdata")
load(file="tmp/distichus.Rdata")
load(file="tmp/brev.clade.Rdata")
load(file="tmp/range.aoc.Rdata")
load(file="tmp/point.aoc.Rdata")
load(file="tmp/bc.aoc.Rdata")
} # END if (runslow == TRUE)
