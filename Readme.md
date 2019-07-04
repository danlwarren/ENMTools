ENMTools
========

This package implements various tests, visualizations, and metrics for
use with environmental niche models (ENMs) and species distribution
models (SDMs).

### Citation:

Warren, D.L., N. Matzke, M. Cardillo, J. Baumgartner, L. Beaumont, N.
Huron, M. Simões, and R. Dinnage. 2019. ENMTools (Software Package).
URL: <https://github.com/danlwarren/ENMTools>.
<doi:10.5281/zenodo.3268814>

------------------------------------------------------------------------

Installation
============

At present, ENMTools is downloadable from
<https://github.com/danlwarren/ENMTools>. There are multiple ways to
download it. The easiest is to use devtools and install from GitHub.

### Installing from GitHub using devtools

Run the following code from your R console:

    install.packages("devtools")
    library(devtools)
    install_github("danlwarren/ENMTools")
    library(ENMTools)

### Install from zip file

A zipped version of the package is available at
<https://github.com/danlwarren/ENMTools/archive/master.zip>. To install
from the zip file, download a copy of it to your system. Once it's
finished downloading, type the following (where PATH is the path to the
zip file):

    install.packages("devtools")
    library(devtools)
    install_local("PATH")
    library(ENMTools)

------------------------------------------------------------------------

Interacting with ENMTools
=========================

### Creating enmtools.species objects

First we're going to load in some environmental data.

    env.files <- list.files(path = "test/testdata/", pattern = "pc", full.names = TRUE)
    env <- stack(env.files)
    names(env) <- c("pc1", "pc2", "pc3", "pc4")
    env <- setMinMax(env)

ENMTools is primarily designed to examine patterns of similarity and
difference between ENMs for different species. In order to simplify
interactions with the functions in ENMTools, you need to put your data
for each of your species into an enmtools.species object. You can create
and view an empty enmtools.species object just by typing:

    ahli <- enmtools.species()
    ahli

    ## 
    ## 
    ## Range raster not defined.
    ## 
    ## Presence points not defined.
    ## 
    ## Background points not defined.
    ## 
    ## Species name not defined.

You can add data to this object manually:

    names(ahli)

    ## [1] "range"             "presence.points"   "background.points"
    ## [4] "models"            "species.name"

    ahli$species.name <- "ahli"
    ahli$presence.points <- read.csv("./test/testdata/ahli.csv")[,2:3]
    ahli$range <- background.raster.buffer(ahli$presence.points, 50000, mask = env)
    ahli$background.points <- background.points.buffer(points = ahli$presence.points,
                                                       radius = 20000, n = 1000, mask = env[[1]])

    ahli

    ## 
    ## 
    ## Range raster: 
    ## class      : RasterLayer 
    ## dimensions : 418, 1535, 641630  (nrow, ncol, ncell)
    ## resolution : 0.008333333, 0.008333333  (x, y)
    ## extent     : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
    ## crs        : NA 
    ## source     : memory
    ## names      : pc1 
    ## values     : 1, 1  (min, max)
    ## 
    ## 
    ## 
    ## Presence points (first ten only): 
    ## 
    ##  Longitude   Latitude
    ## ----------  ---------
    ##   -80.0106    21.8744
    ##   -79.9086    21.8095
    ##   -79.8065    21.7631
    ##   -79.8251    21.8095
    ##   -79.8807    21.8374
    ##   -79.9550    21.8374
    ##   -80.3446    22.0136
    ##   -80.2983    21.9951
    ##   -80.1776    21.9023
    ##   -80.1591    21.9673
    ## 
    ## 
    ## Background points (first ten only): 
    ## 
    ##  Longitude   Latitude
    ## ----------  ---------
    ##  -79.79559   21.78754
    ##  -79.99559   22.00420
    ##  -79.97892   21.78754
    ##  -80.43726   22.16254
    ##  -79.76226   21.80420
    ##  -80.19559   22.10420
    ##  -80.17892   21.92920
    ##  -79.97059   21.86254
    ##  -79.97892   22.14587
    ##  -79.82059   21.67920
    ## 
    ## 
    ## Species name:  ahli

Or you can add bits of it when the object is created:

    allogus <- enmtools.species(species.name = "allogus", 
                                presence.points = read.csv("./test/testdata/allogus.csv")[,3:4])

    allogus$range <- background.raster.buffer(allogus$presence.points, 50000, mask = env)
    allogus$background.points <- background.points.buffer(points = allogus$presence.points,
                                                          radius = 20000, n = 1000, mask = env[[1]])

    allogus

    ## 
    ## 
    ## Range raster: 
    ## class      : RasterLayer 
    ## dimensions : 418, 1535, 641630  (nrow, ncol, ncell)
    ## resolution : 0.008333333, 0.008333333  (x, y)
    ## extent     : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
    ## crs        : NA 
    ## source     : memory
    ## names      : pc1 
    ## values     : 1, 1  (min, max)
    ## 
    ## 
    ## 
    ## Presence points (first ten only): 
    ## 
    ##  Longitude   Latitude
    ## ----------  ---------
    ##   -79.2527    22.2109
    ##   -78.7774    22.2241
    ##   -78.6189    22.2373
    ##   -78.1039    21.1809
    ##   -78.0247    21.1809
    ##   -77.9983    20.9301
    ##   -77.9719    21.7091
    ##   -77.9719    21.5507
    ##   -77.9323    21.6167
    ##   -77.9323    20.7320
    ## 
    ## 
    ## Background points (first ten only): 
    ## 
    ##  Longitude   Latitude
    ## ----------  ---------
    ##  -78.57892   22.23754
    ##  -77.06226   20.88754
    ##  -76.97892   20.72087
    ##  -78.18726   20.92087
    ##  -77.03726   20.08754
    ##  -76.50392   20.39587
    ##  -77.12059   19.95420
    ##  -77.95392   21.86254
    ##  -75.52059   20.76254
    ##  -75.79559   20.64587
    ## 
    ## 
    ## Species name:  allogus

However, ENMTools also contains some sample data. It contains an
enmtools.clade object called "iberolacerta.clade", which holds several
enmtools.species objects. It also has some low-resolution Worldclim data
that we can use to demonstrate functions. We'll pull two of those
species out now.

    data(iberolacerta.clade)
    data(euro.worldclim)
    monticola <- iberolacerta.clade$species$monticola
    cyreni <- iberolacerta.clade$species$cyreni
    env <- euro.worldclim

Building an ENM
---------------

ENMTools contains functions to simplify the ENM construction process.
Using enmtools.species objects and the correct modeling commands, we can
build models very quickly. These commands are primarily wrappers to
dismo model construction and projection functions, and at present are
only available for GLM, Maxent, Domain, and Bioclim models. One of the
nice bits about this setup is that it allows enmtools to automatically
generate suitability maps, do model evaluation, and plot the marginal
suitability of habitat for each variable separately.

Before we get started modeling, however, we might want to trim our
predictor set so as to reduce collinearity. ENMTools contains a couple
of handy functions for this.

    raster.cor.matrix(env)

    ##              bio1         bio2        bio3        bio4       bio5
    ## bio1   1.00000000  0.208715969  0.36766171 -0.21357041  0.8853513
    ## bio2   0.20871597  1.000000000  0.58362743  0.30918934  0.5417192
    ## bio3   0.36766171  0.583627431  1.00000000 -0.56601419  0.3052211
    ## bio4  -0.21357041  0.309189341 -0.56601419  1.00000000  0.1754180
    ## bio5   0.88535133  0.541719174  0.30522110  0.17541802  1.0000000
    ## bio6   0.93295069 -0.017231880  0.42521254 -0.52131150  0.6875177
    ## bio7  -0.05329392  0.709020870 -0.14900115  0.87998888  0.4016817
    ## bio8   0.24945265  0.068562982 -0.16262988  0.30400935  0.2862188
    ## bio9   0.73849291  0.183148800  0.42195497 -0.37866158  0.6400464
    ## bio10  0.96045067  0.311324869  0.21975162  0.06247355  0.9606340
    ## bio11  0.95921479  0.101719882  0.48107648 -0.47305206  0.7533941
    ## bio12 -0.60846674 -0.482731496 -0.31488066 -0.07961919 -0.7137513
    ## bio13 -0.41433559 -0.510317568 -0.25626002 -0.19124801 -0.5722885
    ## bio14 -0.72871981 -0.338229901 -0.37618609  0.16712793 -0.7297745
    ## bio15  0.45421260 -0.007931916  0.11838593 -0.21759678  0.3821863
    ## bio16 -0.44597594 -0.506032477 -0.26995545 -0.16924912 -0.5920746
    ## bio17 -0.70338945 -0.343806007 -0.33935275  0.10832583 -0.7243068
    ## bio18 -0.81091065 -0.331213024 -0.47078459  0.28348226 -0.7776879
    ## bio19 -0.12414066 -0.396445432  0.07348362 -0.48121938 -0.3430162
    ##              bio6        bio7        bio8       bio9       bio10
    ## bio1   0.93295069 -0.05329392  0.24945265  0.7384929  0.96045067
    ## bio2  -0.01723188  0.70902087  0.06856298  0.1831488  0.31132487
    ## bio3   0.42521254 -0.14900115 -0.16262988  0.4219550  0.21975162
    ## bio4  -0.52131150  0.87998888  0.30400935 -0.3786616  0.06247355
    ## bio5   0.68751772  0.40168174  0.28621877  0.6400464  0.96063401
    ## bio6   1.00000000 -0.38884596  0.09285337  0.7734384  0.80726704
    ## bio7  -0.38884596  1.00000000  0.24603268 -0.1633540  0.20071994
    ## bio8   0.09285337  0.24603268  1.00000000 -0.2200483  0.33135353
    ## bio9   0.77343845 -0.16335400 -0.22004834  1.0000000  0.66175815
    ## bio10  0.80726704  0.20071994  0.33135353  0.6617581  1.00000000
    ## bio11  0.98896057 -0.29134540  0.12985480  0.7886188  0.84907884
    ## bio12 -0.48704454 -0.29133189 -0.30622476 -0.4629735 -0.65829840
    ## bio13 -0.28138429 -0.37121605 -0.19351156 -0.3074562 -0.48860809
    ## bio14 -0.67564389 -0.07381576 -0.25699492 -0.6265099 -0.71411350
    ## bio15  0.47445069 -0.11344874  0.17648853  0.3970693  0.41693023
    ## bio16 -0.31485467 -0.35410914 -0.21747973 -0.3274923 -0.51511693
    ## bio17 -0.63769389 -0.11473790 -0.30488093 -0.5552129 -0.70368996
    ## bio18 -0.80126262  0.02381502  0.03291308 -0.8163353 -0.76592247
    ## bio19  0.07715578 -0.53249335 -0.51356967  0.1072073 -0.26914814
    ##             bio11       bio12      bio13       bio14        bio15
    ## bio1   0.95921479 -0.60846674 -0.4143356 -0.72871981  0.454212596
    ## bio2   0.10171988 -0.48273150 -0.5103176 -0.33822990 -0.007931916
    ## bio3   0.48107648 -0.31488066 -0.2562600 -0.37618609  0.118385927
    ## bio4  -0.47305206 -0.07961919 -0.1912480  0.16712793 -0.217596785
    ## bio5   0.75339408 -0.71375135 -0.5722885 -0.72977455  0.382186338
    ## bio6   0.98896057 -0.48704454 -0.2813843 -0.67564389  0.474450688
    ## bio7  -0.29134540 -0.29133189 -0.3712161 -0.07381576 -0.113448735
    ## bio8   0.12985480 -0.30622476 -0.1935116 -0.25699492  0.176488534
    ## bio9   0.78861884 -0.46297347 -0.3074562 -0.62650986  0.397069264
    ## bio10  0.84907884 -0.65829840 -0.4886081 -0.71411350  0.416930231
    ## bio11  1.00000000 -0.54163405 -0.3297284 -0.72423144  0.492885864
    ## bio12 -0.54163405  1.00000000  0.9064818  0.77619752 -0.243139096
    ## bio13 -0.32972845  0.90648180  1.0000000  0.48404051  0.130409758
    ## bio14 -0.72423144  0.77619752  0.4840405  1.00000000 -0.743674332
    ## bio15  0.49288586 -0.24313910  0.1304098 -0.74367433  1.000000000
    ## bio16 -0.36513899  0.91789736  0.9902335  0.50120043  0.124449153
    ## bio17 -0.68356379  0.80791339  0.5245850  0.98592339 -0.729480228
    ## bio18 -0.82992875  0.76402046  0.5883523  0.84673991 -0.462267795
    ## bio19  0.01511498  0.74717071  0.7906993  0.31690226  0.128299899
    ##            bio16      bio17       bio18       bio19
    ## bio1  -0.4459759 -0.7033895 -0.81091065 -0.12414066
    ## bio2  -0.5060325 -0.3438060 -0.33121302 -0.39644543
    ## bio3  -0.2699555 -0.3393528 -0.47078459  0.07348362
    ## bio4  -0.1692491  0.1083258  0.28348226 -0.48121938
    ## bio5  -0.5920746 -0.7243068 -0.77768793 -0.34301616
    ## bio6  -0.3148547 -0.6376939 -0.80126262  0.07715578
    ## bio7  -0.3541091 -0.1147379  0.02381502 -0.53249335
    ## bio8  -0.2174797 -0.3048809  0.03291308 -0.51356967
    ## bio9  -0.3274923 -0.5552129 -0.81633533  0.10720732
    ## bio10 -0.5151169 -0.7036900 -0.76592247 -0.26914814
    ## bio11 -0.3651390 -0.6835638 -0.82992875  0.01511498
    ## bio12  0.9178974  0.8079134  0.76402046  0.74717071
    ## bio13  0.9902335  0.5245850  0.58835234  0.79069926
    ## bio14  0.5012004  0.9859234  0.84673991  0.31690226
    ## bio15  0.1244492 -0.7294802 -0.46226779  0.12829990
    ## bio16  1.0000000  0.5362200  0.61586400  0.79209065
    ## bio17  0.5362200  1.0000000  0.81764699  0.38396995
    ## bio18  0.6158640  0.8176470  1.00000000  0.17654879
    ## bio19  0.7920907  0.3839700  0.17654879  1.00000000

That's great, but it's a bit hard to pick variables this way. Let's try
it visually instead.

    raster.cor.plot(env)

    ## $cor.mds.plot

![](Readme_files/figure-markdown_strict/collinearity2-1.png)

    ## 
    ## $cor.heatmap

![](Readme_files/figure-markdown_strict/collinearity2-2.png)

The raster.cor.plot function gives us two visualizations. One heatmap
that colors pairs of predictors by their Pearson correlation
coefficient, and one cluster plot that does mds scaling of the predictor
variables and then plots them in a two dimensional space so that more
correlated predictors are closer to each other. We're going to make an
arbitrary decision to just use three predictors, and to keep those
predictors relatively uncorrelated we'll select predictors that are far
apart in this mds plot. Here we'll choose bio1, bio12, and bio7.

    env <- env[[c("bio1", "bio12", "bio7")]]
    plot(env)

![](Readme_files/figure-markdown_strict/subsetenv-1.png)

    raster.cor.matrix(env)

    ##              bio1      bio12        bio7
    ## bio1   1.00000000 -0.6084667 -0.05329392
    ## bio12 -0.60846674  1.0000000 -0.29133189
    ## bio7  -0.05329392 -0.2913319  1.00000000

### GLM

GLMs usually require the user to supply a formula, an enmtools.species
object, and some environmental data. If your formula is a strictly
additive function of all of the environmental layers in env, though,
enmtools.glm will build a formula automatically.

    monticola.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ bio1 + bio12 + bio7, test.prop = 0.2)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.

    monticola.glm

    ## 
    ## 
    ## Formula:  presence ~ bio1 + bio12 + bio7
    ## <environment: 0x7fbefbb7c0f8>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##       Longitude   Latitude   bio1   bio12   bio7   presence
    ## ---  ----------  ---------  -----  ------  -----  ---------
    ## 1     -5.171215   43.06957     78     917    249          1
    ## 2     -6.036635   43.02531     76    1012    246          1
    ## 3     -7.679727   40.38852    137    1143    247          1
    ## 4     -7.790437   40.30959    129    1231    242          1
    ## 5     -7.473340   43.78935    140     931    179          1
    ## 6     -6.575039   42.91070     84    1012    247          1
    ## 7     -5.132756   43.49572    133     822    190          1
    ## 10    -7.621731   40.34170    101    1514    229          1
    ## 11    -7.645674   40.36543    101    1514    229          1
    ## 13    -6.990000   42.57000    107     893    253          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6240  -0.7954  -0.4988   0.8415   2.1911  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  9.0387856  2.2499375   4.017 5.89e-05 ***
    ## bio1        -0.0352850  0.0065683  -5.372 7.79e-08 ***
    ## bio12       -0.0001144  0.0007325  -0.156    0.876    
    ## bio7        -0.0213880  0.0050851  -4.206 2.60e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.70  on 719  degrees of freedom
    ## Residual deviance: 512.41  on 716  degrees of freedom
    ## AIC: 252.02
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7310979 
    ## cor            : 0.3095978 
    ## max TPR+TNR at : 0.01620189 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.4223212 
    ## cor            : -0.0178785 
    ## max TPR+TNR at : 0.3814719 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.736591 
    ## cor            : 0.1924099 
    ## max TPR+TNR at : -0.00896788 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.4194731 
    ## cor            : -0.008373719 
    ## max TPR+TNR at : 0.4024466 
    ## 
    ## 
    ## Suitability:  
    ## class      : RasterLayer 
    ## dimensions : 54, 162, 8748  (nrow, ncol, ncell)
    ## resolution : 0.1666667, 0.1666667  (x, y)
    ## extent     : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
    ## crs        : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## source     : memory
    ## names      : layer 
    ## values     : 0.02633004, 0.9962582  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_strict/build_glms1-1.png)

Notice this produces the same formula as:

    monticola.glm <- enmtools.glm(species = monticola, env = env, test.prop = 0.2)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.

    monticola.glm

    ## 
    ## 
    ## Formula:  presence ~ bio1 + bio12 + bio7
    ## <environment: 0x7fbeea824fc8>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##       Longitude   Latitude   bio1   bio12   bio7   presence
    ## ---  ----------  ---------  -----  ------  -----  ---------
    ## 1     -5.171215   43.06957     78     917    249          1
    ## 2     -6.036635   43.02531     76    1012    246          1
    ## 3     -7.679727   40.38852    137    1143    247          1
    ## 4     -7.790437   40.30959    129    1231    242          1
    ## 5     -7.473340   43.78935    140     931    179          1
    ## 7     -5.132756   43.49572    133     822    190          1
    ## 9     -4.941888   43.35310    128     843    194          1
    ## 10    -7.621731   40.34170    101    1514    229          1
    ## 11    -7.645674   40.36543    101    1514    229          1
    ## 12    -7.642539   40.36317    101    1514    229          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6379  -0.7870  -0.5065   0.8394   2.2056  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  8.5283769  2.2181089   3.845 0.000121 ***
    ## bio1        -0.0350648  0.0065054  -5.390 7.04e-08 ***
    ## bio12        0.0001073  0.0007217   0.149 0.881828    
    ## bio7        -0.0201999  0.0050326  -4.014 5.97e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.70  on 719  degrees of freedom
    ## Residual deviance: 511.42  on 716  degrees of freedom
    ## AIC: 251.64
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7362624 
    ## cor            : 0.3121786 
    ## max TPR+TNR at : -0.01573163 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.4229288 
    ## cor            : -0.01736621 
    ## max TPR+TNR at : 0.4959922 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.7256611 
    ## cor            : 0.1852202 
    ## max TPR+TNR at : 0.01866536 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.416525 
    ## cor            : -0.01040267 
    ## max TPR+TNR at : 0.4157732 
    ## 
    ## 
    ## Suitability:  
    ## class      : RasterLayer 
    ## dimensions : 54, 162, 8748  (nrow, ncol, ncell)
    ## resolution : 0.1666667, 0.1666667  (x, y)
    ## extent     : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
    ## crs        : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## source     : memory
    ## names      : layer 
    ## values     : 0.02645355, 0.9967466  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_strict/build_glms2-1.png)

If you want a more complicated formula, though (e.g., with interactions
or polynomial effects), you'll need to supply that manually.

    monticola.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) * poly(bio12, 2), test.prop = 0.2)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.

    monticola.glm

    ## 
    ## 
    ## Formula:  presence ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2) + poly(bio7, 
    ##     2):poly(bio12, 2)
    ## <environment: 0x7fbed20e80e0>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##       Longitude   Latitude   bio1   bio12   bio7   presence
    ## ---  ----------  ---------  -----  ------  -----  ---------
    ## 1     -5.171215   43.06957     78     917    249          1
    ## 2     -6.036635   43.02531     76    1012    246          1
    ## 3     -7.679727   40.38852    137    1143    247          1
    ## 4     -7.790437   40.30959    129    1231    242          1
    ## 7     -5.132756   43.49572    133     822    190          1
    ## 9     -4.941888   43.35310    128     843    194          1
    ## 10    -7.621731   40.34170    101    1514    229          1
    ## 11    -7.645674   40.36543    101    1514    229          1
    ## 12    -7.642539   40.36317    101    1514    229          1
    ## 13    -6.990000   42.57000    107     893    253          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3197  -0.7204  -0.3827   0.6340   2.8296  
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -0.7282     0.3948  -1.844   0.0651 .  
    ## poly(bio1, 2)1                  -33.1239     6.0971  -5.433 5.55e-08 ***
    ## poly(bio1, 2)2                  -36.5310     6.9511  -5.255 1.48e-07 ***
    ## poly(bio7, 2)1                  -23.8667    12.6013  -1.894   0.0582 .  
    ## poly(bio7, 2)2                   18.0181    10.3828   1.735   0.0827 .  
    ## poly(bio12, 2)1                  28.1731    12.1169   2.325   0.0201 *  
    ## poly(bio12, 2)2                  -6.1736     9.3566  -0.660   0.5094    
    ## poly(bio7, 2)1:poly(bio12, 2)1  126.9641   417.5648   0.304   0.7611    
    ## poly(bio7, 2)2:poly(bio12, 2)1 -297.1738   277.7655  -1.070   0.2847    
    ## poly(bio7, 2)1:poly(bio12, 2)2  388.7163   271.2807   1.433   0.1519    
    ## poly(bio7, 2)2:poly(bio12, 2)2  230.2286   164.5608   1.399   0.1618    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.70  on 719  degrees of freedom
    ## Residual deviance: 452.43  on 709  degrees of freedom
    ## AIC: 241.3
    ## 
    ## Number of Fisher Scoring iterations: 5
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7930063 
    ## cor            : 0.4071561 
    ## max TPR+TNR at : 0.01625976 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.6719659 
    ## cor            : 0.08894771 
    ## max TPR+TNR at : 0.4481033 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.7779072 
    ## cor            : 0.2417324 
    ## max TPR+TNR at : -0.2080328 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.6598308 
    ## cor            : 0.04157826 
    ## max TPR+TNR at : 0.2955711 
    ## 
    ## 
    ## Suitability:  
    ## class      : RasterLayer 
    ## dimensions : 54, 162, 8748  (nrow, ncol, ncell)
    ## resolution : 0.1666667, 0.1666667  (x, y)
    ## extent     : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
    ## crs        : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## source     : memory
    ## names      : layer 
    ## values     : 2.220446e-16, 0.9923567  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_strict/build_glms3-1.png)

To check out the marginal response functions, you only need to type

    monticola.glm$response.plots

    ## $bio1

![](Readme_files/figure-markdown_strict/response_plots-1.png)

    ## 
    ## $bio7

![](Readme_files/figure-markdown_strict/response_plots-2.png)

    ## 
    ## $bio12

![](Readme_files/figure-markdown_strict/response_plots-3.png)

These plots present a smoothed estimate of the frequency of different
levels of the environemntal variable in the presence data and the
background points, along with the estimated relationship between that
environmental predictor and the suitability of habitat from the model.

You can also visualize your models and data in a 2D environment space
using any pair of layers from your environment stack. These plots hold
all non-plotted variables (bio7 in this case) constant at their mean
value across all presence points, then vary the plotted variables
between the minimum and maximum values in env.

The suit.plot shows you suitability in environment space as a function
of your two variables, with brighter colors representing variable
combinations predicted to be more suitable. The points represent the
occurrence points for your species in that environment space.

The colored raster of the background.plot shows you the density of
background points in environment space, while the white points again
represent your occurrence points in environment space.

    visualize.enm(monticola.glm, env, layers = c("bio1", "bio12"), plot.test.data = TRUE)

    ## $background.plot

![](Readme_files/figure-markdown_strict/visualize.enm-1.png)

    ## 
    ## $suit.plot

![](Readme_files/figure-markdown_strict/visualize.enm-2.png)

### GAM, Bioclim, Domain, and Maxent

The procedure for building Bioclim, Domain, and Maxent models is similar
to the procedure for GLMs, with the exception that you do not need to
pass a formula to the model function for Maxent, Domain, and Bioclim
models. Note that running Maxent models requires a bit of extra setup;
see dismo documentation for details.

    monticola.gam <- enmtools.gam(monticola, env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) * poly(bio12, 2), test.prop = 0.2)
    monticola.dm <- enmtools.dm(monticola, env, test.prop = 0.2)
    monticola.bc <- enmtools.bc(monticola, env, test.prop = 0.2)
    monticola.mx <- enmtools.maxent(monticola, env, test.prop = 0.2)

Metrics: breadth, correlation, and overlap
------------------------------------------

ENMTools provides a number of metrics for ENMs and for similarities
between ENMs. These include measures of niche breadth, based on
Levins(1968). An important caveat when interpreting these metrics is
that they are driven to some (variable) extent by the availability of
different combinations of environmental predictors. As such they are
more accurately interpreted as a measurment of the smoothness of the
geographic distribution of suitability scores than as an estimate of the
breadth of the fundamental niche; an organism with narrow fundamental
niche breadth that nonetheless encompasses a set of environmental
conditions that is quite common will have a high breadth when measured
using ENMs, while having a low breadth in environment space.

    raster.breadth(monticola.glm)

    ## $B1
    ## [1] 0.9445673
    ## 
    ## $B2
    ## [1] 0.5414073

ENMTools also provides metrics for measuring similarity between ENMs.
These include Schoener's D (Schoener 1968), I (Warren et al. 2008), and
the Spearman rank correlation coefficient between two rasters. While D
and I are commonly used in the ENM literature, they may tend to
overestimate similarity between ENMs when many grid cells are of similar
values (e.g., when two species prefer different habitat but the region
contains a great deal of habitat that is unsuitable for both).

    monticola.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = 0.2)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.

    cyreni.glm <- enmtools.glm(species = cyreni, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = 0.2)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.

    raster.overlap(monticola.glm, cyreni.glm)

    ## $D
    ## [1] 0.669057
    ## 
    ## $I
    ## [1] 0.8839939
    ## 
    ## $rank.cor
    ## [1] 0.30307

A new feature of the R version of ENMTools is that you can now use these
same metrics in the n-dimensional space of all combinations of
environmental variables, instead of restricting your measures of model
similarity to those sets of conditions that appear in the training
region. This is done by repeatedly drawing Latin hypercube samples from
the space of all possible combinations of environmental variables given
the min and max of each variable within the training region. ENMTools
continues to draw samples until subsequent iterations differ by less
than a specified tolerance value. Lower tolerance values result in more
precise estimates of overlap, but can take much longer to calculate.

    monticola.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = 0.2)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.

    cyreni.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = 0.2)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.

    env.overlap(monticola.glm, cyreni.glm, env, tolerance = .001)

    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."

    ## $env.D
    ## [1] 0.9631428
    ## 
    ## $env.I
    ## [1] 0.9984573
    ## 
    ## $env.cor
    ## [1] 0.9986247
    ## 
    ## $env.D.plot

![](Readme_files/figure-markdown_strict/env_overlap-1.png)

    ## 
    ## $env.I.plot

![](Readme_files/figure-markdown_strict/env_overlap-2.png)

    ## 
    ## $env.cor.plot

![](Readme_files/figure-markdown_strict/env_overlap-3.png)

The plots that come out of these environment space functions are used
for diagnosing convergence of the overlap/breadth metric. Ideally what
you want is a relationship between the metric and the number of samples
that shows no clear directional trend.

Hypothesis testing
------------------

### Niche identity or equivalency test

In this example, we will run a niche identity (also called equivalency)
test, as in Warren et al. 2008. This test takes the presence points for
a pair of species and randomly reassigns them to each species, then
builds ENMs for these randomized occurrences. By doing this many times,
we can estimate the probability distribution for ENM overlap between
species under the null hypothesis that the two species' occurrences in
the environment are effectively a random draw from the same underlying
distribution. Note that niche evolution is only one of many reasons why
two species' realized environmental distributions might cause departures
from this null hypothesis. See Warren et al. 2014 for details.

To run an identity test, we need to decide what type of models we will
build, how many replicates we will run, and (in the case of GLM and GAM)
a model formula to use for empirical models and the Monte Carlo
replicates. The resulting object contains the replicate models, p
values, and plots of the results. Typically idenity tests are run with
at least 99 replicates, but we are using a smaller number here for the
sake of execution time.

*NOTE:* In order for models to be comparable, both empirical and
pseudoreplicate models for the identity test are conducted with
pseudoabsence points pooled for the two species being compared.

    id.glm <- identity.test(species.1 = monticola, species.2 = cyreni, env = env, type = "glm", nreps = 4)

    id.glm

    ## 
    ## 
    ##  
    ## 
    ## Identity test monticola vs. cyreni
    ## 
    ## objectentity test p-values:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.2      0.2      0.2      0.2      0.2      0.2 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I    rank.cor       env.D       env.I     env.cor
    ## ----------  ----------  ----------  ----------  ----------  ----------  ----------
    ## empirical    0.3244487   0.5929443   0.1049357   0.3679904   0.6157157   0.1346273
    ## rep 1        0.9650752   0.9986401   0.9895283   0.9651080   0.9985140   0.9917857
    ## rep 2        0.8542767   0.9803007   0.9731018   0.9216392   0.9855454   0.9796226
    ## rep 3        0.9130669   0.9932626   0.9964534   0.9510721   0.9946931   0.9967796
    ## rep 4        0.9482891   0.9971329   0.9808413   0.9496265   0.9968980   0.9826041

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### Background or similarity test

The background or similarity test compares the overlap seen between two
species' ENMs to the overlap expected by chance if one or both species
was effectively choosing habitat at random from within their broad
geographic range. The purpose of this test is to correct for the
availability of habitat and ask whether the observed similarity between
species or populations is significantly more (or less) than expected
given the available set of environments in the regions in which they
occur.

*NOTE:* In order for models to be comparable, both empirical and
pseudoreplicate models for the background test are conducted with
pseudoabsence points pooled for the two species being compared.

In Warren et al. 2008, we developed this test in the context of
comparing one species' actual occurrence to the random background
occurrences of the other species. This is what we call an "asymmetric"
test, and in our case we did the test in both directions with the idea
that we might compare the results of A vs. B background to the results
of B vs. A background. This may be informative in some cases, but many
people have also found this asymmetry confusing (and indeed it is often
difficult to interpret). For that reason, the background test here can
be conducted against a null hypothesis that is generated from
"asymmetric" (species.1 vs species.2 background) or "symmetric"
(species.1 background vs. species.2 background) comparisons.

Here, for instance, is a Bioclim background test using the classical
asymmetric approach:

    bg.bc.asym <- background.test(species.1 = monticola, species.2 = cyreni, env = env, type = "bc", nreps = 4, test.type = "asymmetric" )

    bg.bc.asym

    ## 
    ## 
    ##  
    ## 
    ## Asymmetric background test
    ##  monticola vs. cyreni background
    ## 
    ## background test p-values:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.4      0.4      0.4      0.4      0.4      0.4 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I     rank.cor       env.D       env.I     env.cor
    ## ----------  ----------  ----------  -----------  ----------  ----------  ----------
    ## empirical    0.0242365   0.1455829   -0.0450249   0.0127989   0.1069760   0.1573093
    ## rep 1        0.6247480   0.8714637    0.6733882   0.5334867   0.7988016   0.7494258
    ## rep 2        0.6476934   0.8833554    0.7158207   0.5367838   0.7873901   0.6921213
    ## rep 3        0.6388181   0.8850088    0.7048999   0.5496922   0.8085522   0.7310567
    ## rep 4        0.6375834   0.8787972    0.7043380   0.5228360   0.7911751   0.7083220

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/unnamed-chunk-5-1.png)

And here is a Domain background test using the symmetric approach:

    bg.dm.sym <- background.test(species.1 = monticola, species.2 = cyreni, env = env, type = "dm", nreps = 4, test.type = "symmetric" )

    bg.dm.sym

    ## 
    ## 
    ##  
    ## 
    ## Symmetric background test
    ##  monticola background vs. cyreni background
    ## 
    ## background test p-values:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.4      0.4      0.4      0.4      0.4      0.4 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I     rank.cor       env.D       env.I     env.cor
    ## ----------  ----------  ----------  -----------  ----------  ----------  ----------
    ## empirical    0.1974048   0.4272913   -0.1020590   0.0791433   0.2673722   0.2231269
    ## rep 1        0.9502306   0.9969369    0.7029342   0.8279814   0.9455793   0.9142887
    ## rep 2        0.9656746   0.9987258    0.8421624   0.9110843   0.9840390   0.9849042
    ## rep 3        0.9668991   0.9980430    0.8720834   0.8844004   0.9691735   0.9796014
    ## rep 4        0.9493448   0.9966695    0.6564745   0.8386918   0.9465098   0.9399921

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/unnamed-chunk-7-1.png)

### Ecospat tests

Using enmtools.species objects also provides a simplified interface to
the niche equivalency and similarity tests (or identity and background
tests, respectively) that were developed by Broennimann et al. (2012).
These tests do not rely on ENMs, instead using kernel density smoothing
to estimate density of the species in environment space. Ecospat also
uses the density of the available environment to correct for
availability when measuring overlaps, so that overlaps are not strictly
driven by availability of combinations of environmental variables.

These tests only work with two environmental axes, so they are often
done with the top two PC axes of a set of environments. In our case
we'll just pick a couple of environmental layers, though (bio1 and
bio2). Here's an equivalency/identity test:

    esp.id <- enmtools.ecospat.id(monticola, cyreni, env[[c("bio1", "bio12")]])

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.

    esp.id

    ## 
    ## 
    ##  
    ## 
    ## Ecospat identity test monticola vs. cyreni
    ## 
    ## ecospat.id test empirical overlaps:
    ## $D
    ## [1] 0.0477392
    ## 
    ## $I
    ## [1] 0.2097866
    ## 
    ## 
    ## 
    ## ecospat.id test p-values:
    ## D I 
    ## 0 0

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/ecospat_identity-1.png)![](Readme_files/figure-markdown_strict/ecospat_identity-2.png)

    ## NULL

And here's a symmetric background test. The difference between symmetric
and asymmetric for these tests is the same as for the background tests
presented above.

    esp.bg.sym <- enmtools.ecospat.bg(monticola, cyreni, env[[c("bio1", "bio12")]], test.type = "symmetric")

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.

    esp.bg.sym

    ## 
    ## 
    ##  
    ## 
    ## Ecospat background test symmetric monticola vs. cyreni
    ## 
    ## ecospat.bg test empirical overlaps:
    ## $D
    ## [1] 0.0477392
    ## 
    ## $I
    ## [1] 0.2097866
    ## 
    ## 
    ## 
    ## ecospat.bg test p-values:
    ##    D    I 
    ## 0.56 0.52

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/ecospat_background-1.png)![](Readme_files/figure-markdown_strict/ecospat_background-2.png)

    ## NULL

Note that if you provide more than two layers to the enmtools.ecospat
function, it will performa a PCA analysis on the provided layers and
measure overlaps on the first two axes of that PCA space.

    esp.bg.sym <- enmtools.ecospat.bg(monticola, cyreni, env, test.type = "symmetric")

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## [1] "More than two layers in environment stack and no layers argument passed, performing PCA..."

    esp.bg.sym

    ## 
    ## 
    ##  
    ## 
    ## Ecospat background test symmetric monticola vs. cyreni
    ## 
    ## ecospat.bg test empirical overlaps:
    ## $D
    ## [1] 0.04437498
    ## 
    ## $I
    ## [1] 0.20645
    ## 
    ## 
    ## 
    ## ecospat.bg test p-values:
    ##    D    I 
    ## 0.66 0.52

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/ecospat_background2-1.png)![](Readme_files/figure-markdown_strict/ecospat_background2-2.png)

    ## NULL

### Rangebreak tests

ENMTools also allows you to perform linear, blob, and ribbon rangebreak
tests as developed in Glor and Warren 2011. The linear and blob tests
are two versions of a test that permit one to ask whether the geographic
regions occupied by two species are more environmentally different than
expected by chance. The ribbon test, meanwhile, is designed to test
whether the ranges of two species are divided by a region that is
relatively unsuitable to one or both forms.

For the linear and blob tests, you call them very much like you would
the identity and background tests. Here's a linear one using GLM models:

    rbl.glm <- rangebreak.linear(monticola, cyreni, env, type = "glm", nreps = 4)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## Building empirical models...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Building replicate models...
    ## 
    ## Replicate 1 ...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 2 ...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 3 ...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 4 ...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."

    rbl.glm

    ## 
    ## 
    ##  
    ## 
    ## Linear rangebreak test monticola vs. cyreni
    ## 
    ## rangebreak test p-values:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.2      0.2      0.8      0.2      0.4      0.8 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I     rank.cor       env.D       env.I      env.cor
    ## ----------  ----------  ----------  -----------  ----------  ----------  -----------
    ## empirical    0.3244487   0.5929443    0.1049357   0.3686182   0.6160385    0.1366328
    ## rep 1        0.4899984   0.7817582   -0.0685635   0.5710654   0.7936515    0.0811828
    ## rep 2        0.4892322   0.7098343    0.0104562   0.4162890   0.5984488   -0.3403526
    ## rep 3        0.5676085   0.8087495   -0.0948330   0.4739482   0.6549290   -0.5331683
    ## rep 4        0.5648791   0.8296909    0.2358907   0.5395269   0.7727425    0.4865291

![](Readme_files/figure-markdown_strict/rangebreak_linear-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/rangebreak_linear-2.png)

And here's a blob test using Bioclim:

    rbb.bc <- rangebreak.blob(monticola, cyreni, env, type = "bc", nreps = 4)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## Building empirical models...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Building replicate models...
    ## 
    ## Replicate 1 ...

    ## Warning in data.frame(..., check.names = FALSE): row names were found from
    ## a short variable and have been discarded

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 2 ...

    ## Warning in data.frame(..., check.names = FALSE): row names were found from
    ## a short variable and have been discarded

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 3 ...

    ## Warning in data.frame(..., check.names = FALSE): row names were found from
    ## a short variable and have been discarded

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 4 ...

    ## Warning in data.frame(..., check.names = FALSE): row names were found from
    ## a short variable and have been discarded

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."

    rbb.bc

    ## 
    ## 
    ##  
    ## 
    ## blob rangebreak test monticola vs. cyreni
    ## 
    ## rangebreak test p-values:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      1.0      1.0      1.0      0.8      0.8      0.8 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I     rank.cor       env.D       env.I     env.cor
    ## ----------  ----------  ----------  -----------  ----------  ----------  ----------
    ## empirical    0.0242365   0.1455829   -0.0450249   0.0137616   0.1105921   0.1601701
    ## rep 1        0.0242365   0.1455829   -0.0450249   0.0136902   0.1105707   0.1589182
    ## rep 2        0.0242365   0.1455829   -0.0450249   0.0131433   0.1075037   0.1546165
    ## rep 3        0.0242365   0.1455829   -0.0450249   0.0131419   0.1083426   0.1570253
    ## rep 4        0.0242365   0.1455829   -0.0450249   0.0142214   0.1124521   0.1624109

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/rangebreak_blob-1.png)

If you want to access the individual replicates (for instance to see how
your ranges are being split up), you can find them in the list named
"replicate.models" inside your rangebreak test object.

    rbl.glm$replicate.models$monticola.rep.1

    ## 
    ## 
    ## Formula:  presence ~ bio1 + bio12 + bio7
    ## <environment: 0x7fbed1499578>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##  Longitude   Latitude   bio1   bio12   bio7   presence
    ## ----------  ---------  -----  ------  -----  ---------
    ##       0.13      42.64     61    1053    238          1
    ##      -0.02      42.59     62    1027    240          1
    ##      -3.77      40.78    114     491    287          1
    ##      -3.77      40.78    114     491    287          1
    ##      -3.77      40.78    114     491    287          1
    ##      -3.86      40.72     94     568    284          1
    ##      -3.89      40.69     94     568    284          1
    ##      -3.89      40.69     94     568    284          1
    ##      -3.89      40.69     94     568    284          1
    ##      -3.89      40.78     94     568    284          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7515  -0.7047  -0.4922   0.7251   1.8735  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  5.292e+00  2.191e+00   2.416   0.0157 *  
    ## bio1        -4.620e-02  5.784e-03  -7.987 1.38e-15 ***
    ## bio12       -5.792e-05  7.022e-04  -0.082   0.9343    
    ## bio7        -1.512e-03  5.247e-03  -0.288   0.7732    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 720.87  on 901  degrees of freedom
    ## Residual deviance: 616.53  on 898  degrees of freedom
    ## AIC: 303.39
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 260 
    ## n absences     : 642 
    ## AUC            : 0.7679098 
    ## cor            : 0.3722836 
    ## max TPR+TNR at : -0.03935635 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 260 
    ## n absences     : 10000 
    ## AUC            : 0.4716504 
    ## cor            : 0.004989247 
    ## max TPR+TNR at : 0.3766126 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0
    ## 
    ## Model fit (test data):  [1] NA
    ## 
    ## 
    ## Environment space model fit (test data):  [1] NA
    ## 
    ## 
    ## Suitability:  
    ## class      : RasterLayer 
    ## dimensions : 54, 162, 8748  (nrow, ncol, ncell)
    ## resolution : 0.1666667, 0.1666667  (x, y)
    ## extent     : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
    ## crs        : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## source     : memory
    ## names      : layer 
    ## values     : 0.03028804, 0.9979967  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_strict/rbl_reps-1.png)

    rbl.glm$replicate.models$cyreni.rep.1

    ## 
    ## 
    ## Formula:  presence ~ bio1 + bio12 + bio7
    ## <environment: 0x7fbecd056b38>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##  Longitude   Latitude   bio1   bio12   bio7   presence
    ## ----------  ---------  -----  ------  -----  ---------
    ##      -7.10      43.02    110     994    227          1
    ##      -6.71      43.47    121     920    211          1
    ##      -6.71      43.47    121     920    211          1
    ##      -6.97      43.20    117     942    220          1
    ##      -6.97      43.20    117     942    220          1
    ##      -6.84      43.38    117     977    211          1
    ##      -6.84      43.38    117     977    211          1
    ##      -7.09      43.11    110     994    227          1
    ##      -7.09      43.11    110     994    227          1
    ##      -6.97      43.29    117     942    220          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.94294  -0.21303  -0.07276  -0.05155   1.26988  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) 14.7640301  6.1642914   2.395   0.0166 *  
    ## bio1        -0.0033002  0.0170544  -0.194   0.8466    
    ## bio12       -0.0003822  0.0021234  -0.180   0.8572    
    ## bio7        -0.0627133  0.0145805  -4.301  1.7e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 210.72  on 847  degrees of freedom
    ## Residual deviance: 101.38  on 844  degrees of freedom
    ## AIC: 44.261
    ## 
    ## Number of Fisher Scoring iterations: 6
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 76 
    ## n absences     : 772 
    ## AUC            : 0.9113035 
    ## cor            : 0.4622452 
    ## max TPR+TNR at : -0.2148879 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 76 
    ## n absences     : 10000 
    ## AUC            : 0.7083842 
    ## cor            : 0.07638908 
    ## max TPR+TNR at : 0.5549205 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0
    ## 
    ## Model fit (test data):  [1] NA
    ## 
    ## 
    ## Environment space model fit (test data):  [1] NA
    ## 
    ## 
    ## Suitability:  
    ## class      : RasterLayer 
    ## dimensions : 54, 162, 8748  (nrow, ncol, ncell)
    ## resolution : 0.1666667, 0.1666667  (x, y)
    ## extent     : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
    ## crs        : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## source     : memory
    ## names      : layer 
    ## values     : 0.001154449, 0.9976502  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_strict/rbl_reps-2.png)

For the ribbon rangebreak test, you will need one extra thing; a third
enmtools.species object representing the occurrence points (for one or
both species) that fall within the ribbon of putatively unsuitable
habitat. In the case of these two lizards we don't have such a ribbon,
so we'll just simulate one based on some random points.

    plot(env[[1]])
    points(cyreni$presence.points, col = "red")
    points(monticola$presence.points, col = "blue")

    ribbon <- enmtools.species(species.name = "ribbon")
    ribbon$presence.points <- data.frame(Longitude = runif(n = 10, min = -9, max = 0),
                                          Latitude = runif(n = 10, min = 40.5, max = 42))
    points(ribbon$presence.points, pch = 16)

![](Readme_files/figure-markdown_strict/build_ribbon-1.png)

    ribbon$range <- background.raster.buffer(ribbon$presence.points, 20000, mask = env)
    ribbon

    ## 
    ## 
    ## Range raster: 
    ## class      : RasterLayer 
    ## dimensions : 54, 162, 8748  (nrow, ncol, ncell)
    ## resolution : 0.1666667, 0.1666667  (x, y)
    ## extent     : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
    ## crs        : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## source     : memory
    ## names      : bio1 
    ## values     : 1, 1  (min, max)
    ## 
    ## 
    ## 
    ## Presence points (first ten only): 
    ## 
    ##   Longitude   Latitude
    ## -----------  ---------
    ##  -6.9988659   41.27830
    ##  -8.0237974   40.75874
    ##  -2.9280718   41.21679
    ##  -0.1083568   41.94188
    ##  -0.2053868   40.80873
    ##  -2.3802044   40.71032
    ##  -7.8316145   41.19650
    ##  -0.6423769   41.89845
    ##  -0.9805759   41.52521
    ##  -2.8223883   41.88515
    ## 
    ## 
    ## Background points not defined.
    ## 
    ## Species name:  ribbon

Now we'll run a ribbon rangebreak test using GLM models with quadratic
effects. We also need to tell it the width of the ribbons to generate
for the replicates. The units for the width argument are the same units
that the presence points are in; e.g., if the points are in decimal
degrees you should supply the width of the barrier in decimal degrees.

    rbr.glm <- rangebreak.ribbon(monticola, cyreni, ribbon, env, type = "glm", f = pres ~ poly(bio1, 2) + poly(bio12, 2) + poly(bio7, 2), width = 0.5, nreps = 4)

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## Building empirical models...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species ribbon 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species outside 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Building replicate models...
    ## 
    ## Replicate 1 ...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species ribbon 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species outside 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 2 ...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species ribbon 
    ##  Processing presence points...
    ##  Processing background points...

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species outside 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 3 ...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species ribbon 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species outside 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## 
    ## Replicate 4 ...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species ribbon 
    ##  Processing presence points...
    ##  Processing background points...

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## Adding environmental data to species outside 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."
    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."

    rbr.glm

    ## 
    ## 
    ##  
    ## 
    ## ribbon rangebreak test monticola vs. cyreni
    ## 
    ## rangebreak test p-values...
    ## 
    ## Species 1 vs. Species 2:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      1.2      1.2      0.4      0.4      0.4      1.2 
    ## 
    ## Species 1 vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.8      1.2      0.4      0.8      0.8      0.4 
    ## 
    ## Species 2 vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.4      0.8      0.4      0.4      0.4      0.4 
    ## 
    ## Outside vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.8      1.2      0.4      0.8      0.8      0.4 
    ## 
    ## 
    ## Replicates:
    ## 
    ## Species 1 vs. Species 2:
    ##                    D         I    rank.cor      env.D     env.I
    ## empirical 0.10804657 0.3060365 -0.12272718 0.07189009 0.1750578
    ## rep 1     0.37632040 0.6557407  0.09869762 0.21676011 0.3563165
    ## rep 2     0.08959717 0.2741635  0.12322255 0.10622085 0.2983803
    ## rep 3     0.23421039 0.4654351 -0.12059431 0.17871075 0.3124100
    ## rep 4     0.08853269 0.2755832  0.13292103 0.12007964 0.3210344
    ##               env.cor
    ## empirical  0.01449019
    ## rep 1     -0.04829717
    ## rep 2      0.29061097
    ## rep 3     -0.22973626
    ## rep 4      0.33220017
    ## 
    ## Species 1 vs. Ribbon:
    ##                    D         I    rank.cor      env.D     env.I    env.cor
    ## empirical 0.54731819 0.7832922 -0.01461423 0.16329425 0.3013604 -0.2141816
    ## rep 1     0.54039561 0.7908293  0.41341329 0.32139651 0.5249230  0.4645284
    ## rep 2     0.30333218 0.5615043  0.38784074 0.27405566 0.5302810  0.5304237
    ## rep 3     0.59415667 0.8489086  0.53061440 0.43871784 0.6813579  0.4834408
    ## rep 4     0.09579333 0.2909038  0.30802841 0.03907368 0.1742451  0.2554053
    ## 
    ## Species 2 vs. Ribbon:
    ##                   D         I  rank.cor      env.D     env.I    env.cor
    ## empirical 0.1876482 0.4119861 0.1925618 0.03879466 0.1012175 -0.3114447
    ## rep 1     0.7892906 0.9320477 0.9102640 0.55014979 0.7592867  0.8094674
    ## rep 2     0.4226715 0.6890689 0.8825460 0.34272501 0.6009103  0.7614384
    ## rep 3     0.6211232 0.8266829 0.6791794 0.70270299 0.8074153  0.6747770
    ## rep 4     0.2263865 0.4064181 0.7276099 0.18400095 0.3987646  0.5117592
    ## 
    ## Outside vs. Ribbon:
    ##                    D         I   rank.cor      env.D     env.I    env.cor
    ## empirical 0.59937727 0.8253330 0.05074856 0.20265754 0.3658422 -0.2119834
    ## rep 1     0.59367361 0.8347674 0.55941562 0.35522127 0.5935487  0.6101857
    ## rep 2     0.31818153 0.5853617 0.38512573 0.30735449 0.5694350  0.6015144
    ## rep 3     0.80236178 0.9625940 0.86746083 0.63594335 0.8561294  0.6688097
    ## rep 4     0.09866055 0.2974828 0.34502157 0.03159382 0.1542301  0.1888756

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/rangebreak_ribbon-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/rangebreak_ribbon-2.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/rangebreak_ribbon-3.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Readme_files/figure-markdown_strict/rangebreak_ribbon-4.png)

Note that the output table here has slope, intercept, and intercept
offset.

    rbr.glm$lines.df

    ##        slope intercept    offset
    ## 1 -1.4596571  32.36216 0.4423375
    ## 2  0.1688008  44.35921 0.2535367
    ## 3 -0.5832021  38.29264 0.2894094
    ## 4  0.4676698  46.36662 0.2759886

The intercept denotes the intercept corresponding to the CENTER of each
ribbon. To get the lines denoting the edges of the ribbons (for example
if you want to plot the ribbons on a map), you add and substract the
offset. In other words, the top edge of the ribbon is given by y =
(slope \* x) + intercept + offset, while the bottom edge is given by y =
(slope \* x) + intercept - offset.

### Building an enmtools.clade object

Some of the tests in ENMTools, including some really neat ones that are
still in development, require you to build an enmtools.clade object.
These objects are simply lists that contain a phylogeny and a set of
enmtools.species objects. It's important that the names of the species
objects and their species.name attributes match the names in the
phylogeny's tip.labels. For demonstration, we're going to build an
object for a clade of five anoles from Hispaniola. We have the tree, so
we're just going to grab occurrence data from GBIF using the rgbif
package.

    library(rgbif)
    library(ape)

    ## 
    ## Attaching package: 'ape'

    ## The following objects are masked from 'package:raster':
    ## 
    ##     rotate, zoom

    hisp.anoles <- read.nexus(file = "test/testdata/StarBEAST_MCC.species.txt")

    keepers <- c("brevirostris", "marron", "caudalis", "websteri", "distichus")

    hisp.anoles <- drop.tip(phy = hisp.anoles, tip = hisp.anoles$tip.label[!hisp.anoles$tip.label %in% keepers])
    plot(hisp.anoles)

![](Readme_files/figure-markdown_strict/read_tree-1.png)

So there's our tree. Now we're going to grab some environmental data.

    hisp.env <- stack(list.files("test/testdata/Hispaniola_Worldclim", full.names = TRUE))
    hisp.env <- setMinMax(hisp.env)

And then we'll create a function to build species from GBIF.

    # Automate the process of downloading data and removing duds and dupes
    species.from.gbif <- function(genus, species, name = NA, env){

      # Name it after the species epithet unless told otherwise
      if(is.na(name)){
        name <- species
      }

      # Get GBIF data
      this.sp <- enmtools.species(presence.points = gbif(genus = genus, species = species)[,c("lon", "lat")],
                                  species.name = name)

      # Rename columns, get rid of duds
      colnames(this.sp$presence.points) <- c("Longitude", "Latitude")
      this.sp$presence.points <- this.sp$presence.points[complete.cases(extract(env, this.sp$presence.points)),]
      this.sp$presence.points <- this.sp$presence.points[!duplicated(this.sp$presence.points),]

      this.sp$range <- background.raster.buffer(this.sp$presence.points, 50000, mask = hisp.env)

      return(this.sp)
    }

Now we'll create five species and add them to a species.clade object
that is called brev.clade.

    brevirostris <- species.from.gbif(genus = "Anolis", species = "brevirostris", env = hisp.env)
    marron <- species.from.gbif(genus = "Anolis", species = "marron", env = hisp.env)
    caudalis <- species.from.gbif(genus = "Anolis", species = "caudalis", env = hisp.env)
    websteri <- species.from.gbif(genus = "Anolis", species = "websteri", env = hisp.env)
    distichus <- species.from.gbif(genus = "Anolis", species = "distichus", env = hisp.env)


    brev.clade <- enmtools.clade(species = list(brevirostris, marron, caudalis, websteri, distichus), tree = hisp.anoles)
    check.clade(brev.clade)

    ## 
    ## 
    ## An enmtools.clade object with 5 species
    ## 
    ## Species names: 
    ##   brevirostris    caudalis    distichus   marron      websteri
    ## 
    ## Tree: 
    ## 
    ## Phylogenetic tree with 5 tips and 4 internal nodes.
    ## 
    ## Tip labels:
    ## [1] "brevirostris" "caudalis"     "distichus"    "marron"      
    ## [5] "websteri"    
    ## 
    ## Rooted; includes branch lengths.
    ## 
    ## 
    ## Data Summary: 
    ## 
    ## 
    ##                species.names   in.tree   presence   background   range   
    ## -------------  --------------  --------  ---------  -----------  --------
    ## brevirostris   brevirostris    TRUE      183        0            present 
    ## caudalis       caudalis        TRUE      21         0            present 
    ## distichus      distichus       TRUE      712        0            present 
    ## marron         marron          TRUE      13         0            present 
    ## websteri       websteri        TRUE      18         0            present

That's one way to build a clade object by hand, but there's already one
built into ENMTools to experiment with so we'll just use that.

    data(iberolacerta.clade)

### Age-overlap correlation tests (AOC)

The AOC tests allow you to examine patterns of range, point, and ENM
overlap in the context of a phylogeny. This is effectively a generalized
version of several analyses: age-range correlation (e.g., Fitzpatrick
and Turelli 2006), ENM overlap in the context of a phylogeny (e.g.,
Knouft et al. 2006, Warren et al. 2008), and point overlaps (e.g.,
Cardillo and Warren 2016).

These tests require the creation of an enmtools.clade object, as above.
AOC tests consist of two steps: first, the average overlap at each node
in the phylogeny is calcualted using a method that takes tree topology
into account (see Fitzpatrick and Turelli 2006), then we perform a
linear regression to measure the relationship between node age and
average overlap. Due to the fact that these overlaps violate many of the
assumptions of a regular linear regression, however (e.g., errors are
not iid), we can't calculate significance in the typical way. Instead we
performa Monte Carlo test, permuting the identity of the tips of the
tree and repeating the node averaging and modeling steps. Finally we
measure statistical significance by comparing the empirical slope and
intercept to the distribution of slopes and intercepts from the Monte
Carlo replicates.

First, let's do one using geog.range.overlaps, as in Fitzpatrick and
Turelli 2006. Note that this analysis requires that each of your species
have a range raster stored in their species object (we did that as part
of the function used above).

    range.aoc <- enmtools.aoc(clade = iberolacerta.clade,  nreps = 50, overlap.source = "range")
    summary(range.aoc)

    ## 
    ## 
    ## Age-Overlap Correlation test
    ## 
    ## 50 replicates 
    ## 
    ## p values:
    ##      (Intercept) empirical.df$age 
    ##       0.03921569       0.03921569

![](Readme_files/figure-markdown_strict/range_aoc-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Readme_files/figure-markdown_strict/range_aoc-2.png)

    ## NULL

Now we can do one using point overlaps just by changing the
overlap.source argument:

    point.aoc <- enmtools.aoc(clade = iberolacerta.clade,  nreps = 50, overlap.source = "points")
    summary(point.aoc)

    ## 
    ## 
    ## Age-Overlap Correlation test
    ## 
    ## 50 replicates 
    ## 
    ## p values:
    ##      (Intercept) empirical.df$age 
    ##        0.4313725        0.4705882

![](Readme_files/figure-markdown_strict/point_aoc-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Readme_files/figure-markdown_strict/point_aoc-2.png)

    ## NULL

Or we can use similarity between ENMs built for each species. Here we'll
use GLM models:

    glm.aoc <- enmtools.aoc(clade = iberolacerta.clade,  env = env, nreps = 50, overlap.source = "glm", f = pres ~ poly(bio1, 2) + poly(bio12, 2))

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species monticola 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species martinezricai 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species cyreni 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species horvathi 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species aurelioi 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species aranica 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## Adding environmental data to species bonnali 
    ##  Processing presence points...
    ##  Processing background points...
    ## 
    ## 
    ## Drawing background from species background points.

    summary(glm.aoc)

    ## 
    ## 
    ## Age-Overlap Correlation test
    ## 
    ## 50 replicates 
    ## 
    ## p values:
    ##      (Intercept) empirical.df$age 
    ##        0.2352941        0.2352941

![](Readme_files/figure-markdown_strict/enm_aoc-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Readme_files/figure-markdown_strict/enm_aoc-2.png)

    ## NULL

### Literature cited

\*Broennimann, O., Fitzpatrick, M. C., Pearman, P. B., Petitpierre, B.,
Pellissier, L., Yoccoz, N. G., Thuiller, W., Fortin, M.-J., Randin, C.,
Zimmermann, N. E., Graham, C. H. and Guisan, A. (2012), Measuring
ecological niche overlap from occurrence and spatial environmental data.
Global Ecology and Biogeography, 21: 481–497.
<doi:10.1111/j.1466-8238.2011.00698.x*>

*Fitzpatrick, B. M., & Turelli, M. (2006). The geography of mammalian
speciation: mixed signals from phylogenies and range maps. Evolution,
60(3), 601-615.*

*Knouft, J. H., Losos, J. B., Glor, R. E., & Kolbe, J. J. (2006).
Phylogenetic analysis of the evolution of the niche in lizards of the
Anolis sagrei group. Ecology, 87(sp7).*

*Levins, R. 1968. Evolution In Changing Environments. Monographs in
Population Biology, volume 2. Princeton University Press, Princeton, New
Jersey, USA.*

*Schoener, T. W. 1968. Anolis lizards of Bimini: resource partitioning
in a complex fauna. Ecology 49:704- 726.*

*Warren, D.L., R.E. Glor, and M. Turelli. 2008. Environmental niche
identity versus conservatism: quantitative approaches to niche
evolution. Evolution 62:2868-2883. doi:
10.1111/j.1558-5646.2008.00482.x*

*Warren, D.L., M. Cardillo, D.F. Rosauer, and D.I. Bolnick. 2014.
Mistaking geography for biology: inferring processes from species
distributions. Trends in Ecology and Evolution 29 (10), 572-580. doi:
10.1016/j.tree.2014.08.003*
