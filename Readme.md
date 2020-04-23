ENMTools
========

This package implements various tests, visualizations, and metrics for
use with environmental niche models (ENMs) and species distribution
models (SDMs).

### Citation

Warren, D.L., N. Matzke, M. Cardillo, J. Baumgartner, L. Beaumont, N.
Huron, M. Simões, Teresa L. Iglesias, and R. Dinnage. 2019. ENMTools
(Software Package). URL:
<a href="https://github.com/danlwarren/ENMTools" class="uri">https://github.com/danlwarren/ENMTools</a>.
<a href="doi:10.5281/zenodo.3268814" class="uri">doi:10.5281/zenodo.3268814</a>

------------------------------------------------------------------------

Installation
============

At present, ENMTools is downloadable from
<a href="https://github.com/danlwarren/ENMTools" class="uri">https://github.com/danlwarren/ENMTools</a>.
There are multiple ways to download it. The easiest is to use devtools
and install from GitHub.

### Installing from GitHub using devtools

Run the following code from your R console:

``` r
install.packages("devtools")
library(devtools)
install_github("danlwarren/ENMTools")
library(ENMTools)
```

### Install from zip file

A zipped version of the package is available at
<a href="https://github.com/danlwarren/ENMTools/archive/master.zip" class="uri">https://github.com/danlwarren/ENMTools/archive/master.zip</a>.
To install from the zip file, download a copy of it to your system. Once
it’s finished downloading, type the following (where PATH is the path to
the zip file):

``` r
install.packages("devtools")
library(devtools)
install_local("PATH")
library(ENMTools)
```

------------------------------------------------------------------------

Interacting with ENMTools
=========================

### Creating enmtools.species objects

First we’re going to load in some environmental data.

``` r
env.files <- list.files(path = "./testdata/", pattern = "pc", full.names = TRUE)
env <- stack(env.files)
names(env) <- c("pc1", "pc2", "pc3", "pc4")
env <- setMinMax(env)
```

ENMTools is primarily designed to examine patterns of similarity and
difference between ENMs for different species. In order to simplify
interactions with the functions in ENMTools, you need to put your data
for each of your species into an enmtools.species object. You can create
and view an empty enmtools.species object just by typing:

``` r
ahli <- enmtools.species()
ahli
```

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

``` r
names(ahli)
```

    ## [1] "range"             "presence.points"   "background.points"
    ## [4] "models"            "species.name"

``` r
ahli$species.name <- "ahli"
ahli$presence.points <- read.csv("./testdata/ahli.csv")[,2:3]
ahli$range <- background.raster.buffer(ahli$presence.points, 50000, mask = env)
ahli$background.points <- background.points.buffer(points = ahli$presence.points,
                                                   radius = 20000, n = 1000, mask = env[[1]])

ahli
```

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
    ##  -79.89559   21.85420
    ##  -79.84559   21.70420
    ##  -80.18726   21.92087
    ##  -79.81226   21.77920
    ##  -80.08726   22.09587
    ##  -79.92892   22.08754
    ##  -80.42059   22.10420
    ##  -79.77059   21.77087
    ##  -80.40392   22.16254
    ##  -80.20392   22.08754
    ## 
    ## 
    ## Species name:  ahli

Or you can add bits of it when the object is created:

``` r
allogus <- enmtools.species(species.name = "allogus", 
                            presence.points = read.csv("./testdata/allogus.csv")[,2:3])

allogus$range <- background.raster.buffer(allogus$presence.points, 50000, mask = env)
allogus$background.points <- background.points.buffer(points = allogus$presence.points,
                                                      radius = 20000, n = 1000, mask = env[[1]])

allogus
```

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
    ##  -78.22892   21.24587
    ##  -75.94559   20.69587
    ##  -79.08726   22.26254
    ##  -76.97892   20.27087
    ##  -77.01226   20.87087
    ##  -77.02892   20.83754
    ##  -75.79559   20.38754
    ##  -76.92059   20.82087
    ##  -76.29559   19.98754
    ##  -79.23726   22.29587
    ## 
    ## 
    ## Species name:  allogus

However, ENMTools also contains some sample data. It contains an
enmtools.clade object called “iberolacerta.clade”, which holds several
enmtools.species objects. It also has some low-resolution Worldclim data
that we can use to demonstrate functions. We’ll pull two of those
species out now.

``` r
data(iberolacerta.clade)
data(euro.worldclim)
monticola <- iberolacerta.clade$species$monticola
cyreni <- iberolacerta.clade$species$cyreni
env <- euro.worldclim
```

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

``` r
raster.cor.matrix(env)
```

    ##              bio1         bio2        bio3        bio4       bio5        bio6
    ## bio1   1.00000000  0.208715969  0.36766171 -0.21357041  0.8853513  0.93295069
    ## bio2   0.20871597  1.000000000  0.58362743  0.30918934  0.5417192 -0.01723188
    ## bio3   0.36766171  0.583627431  1.00000000 -0.56601419  0.3052211  0.42521254
    ## bio4  -0.21357041  0.309189341 -0.56601419  1.00000000  0.1754180 -0.52131150
    ## bio5   0.88535133  0.541719174  0.30522110  0.17541802  1.0000000  0.68751772
    ## bio6   0.93295069 -0.017231880  0.42521254 -0.52131150  0.6875177  1.00000000
    ## bio7  -0.05329392  0.709020870 -0.14900115  0.87998888  0.4016817 -0.38884596
    ## bio8   0.24945265  0.068562982 -0.16262988  0.30400935  0.2862188  0.09285337
    ## bio9   0.73849291  0.183148800  0.42195497 -0.37866158  0.6400464  0.77343845
    ## bio10  0.96045067  0.311324869  0.21975162  0.06247355  0.9606340  0.80726704
    ## bio11  0.95921479  0.101719882  0.48107648 -0.47305206  0.7533941  0.98896057
    ## bio12 -0.60846674 -0.482731496 -0.31488066 -0.07961919 -0.7137513 -0.48704454
    ## bio13 -0.41433559 -0.510317568 -0.25626002 -0.19124801 -0.5722885 -0.28138429
    ## bio14 -0.72871981 -0.338229901 -0.37618609  0.16712793 -0.7297745 -0.67564389
    ## bio15  0.45421260 -0.007931916  0.11838593 -0.21759678  0.3821863  0.47445069
    ## bio16 -0.44597594 -0.506032477 -0.26995545 -0.16924912 -0.5920746 -0.31485467
    ## bio17 -0.70338945 -0.343806007 -0.33935275  0.10832583 -0.7243068 -0.63769389
    ## bio18 -0.81091065 -0.331213024 -0.47078459  0.28348226 -0.7776879 -0.80126262
    ## bio19 -0.12414066 -0.396445432  0.07348362 -0.48121938 -0.3430162  0.07715578
    ##              bio7        bio8       bio9       bio10       bio11       bio12
    ## bio1  -0.05329392  0.24945265  0.7384929  0.96045067  0.95921479 -0.60846674
    ## bio2   0.70902087  0.06856298  0.1831488  0.31132487  0.10171988 -0.48273150
    ## bio3  -0.14900115 -0.16262988  0.4219550  0.21975162  0.48107648 -0.31488066
    ## bio4   0.87998888  0.30400935 -0.3786616  0.06247355 -0.47305206 -0.07961919
    ## bio5   0.40168174  0.28621877  0.6400464  0.96063401  0.75339408 -0.71375135
    ## bio6  -0.38884596  0.09285337  0.7734384  0.80726704  0.98896057 -0.48704454
    ## bio7   1.00000000  0.24603268 -0.1633540  0.20071994 -0.29134540 -0.29133189
    ## bio8   0.24603268  1.00000000 -0.2200483  0.33135353  0.12985480 -0.30622476
    ## bio9  -0.16335400 -0.22004834  1.0000000  0.66175815  0.78861884 -0.46297347
    ## bio10  0.20071994  0.33135353  0.6617581  1.00000000  0.84907884 -0.65829840
    ## bio11 -0.29134540  0.12985480  0.7886188  0.84907884  1.00000000 -0.54163405
    ## bio12 -0.29133189 -0.30622476 -0.4629735 -0.65829840 -0.54163405  1.00000000
    ## bio13 -0.37121605 -0.19351156 -0.3074562 -0.48860809 -0.32972845  0.90648180
    ## bio14 -0.07381576 -0.25699492 -0.6265099 -0.71411350 -0.72423144  0.77619752
    ## bio15 -0.11344874  0.17648853  0.3970693  0.41693023  0.49288586 -0.24313910
    ## bio16 -0.35410914 -0.21747973 -0.3274923 -0.51511693 -0.36513899  0.91789736
    ## bio17 -0.11473790 -0.30488093 -0.5552129 -0.70368996 -0.68356379  0.80791339
    ## bio18  0.02381502  0.03291308 -0.8163353 -0.76592247 -0.82992875  0.76402046
    ## bio19 -0.53249335 -0.51356967  0.1072073 -0.26914814  0.01511498  0.74717071
    ##            bio13       bio14        bio15      bio16      bio17       bio18
    ## bio1  -0.4143356 -0.72871981  0.454212596 -0.4459759 -0.7033895 -0.81091065
    ## bio2  -0.5103176 -0.33822990 -0.007931916 -0.5060325 -0.3438060 -0.33121302
    ## bio3  -0.2562600 -0.37618609  0.118385927 -0.2699555 -0.3393528 -0.47078459
    ## bio4  -0.1912480  0.16712793 -0.217596785 -0.1692491  0.1083258  0.28348226
    ## bio5  -0.5722885 -0.72977455  0.382186338 -0.5920746 -0.7243068 -0.77768793
    ## bio6  -0.2813843 -0.67564389  0.474450688 -0.3148547 -0.6376939 -0.80126262
    ## bio7  -0.3712161 -0.07381576 -0.113448735 -0.3541091 -0.1147379  0.02381502
    ## bio8  -0.1935116 -0.25699492  0.176488534 -0.2174797 -0.3048809  0.03291308
    ## bio9  -0.3074562 -0.62650986  0.397069264 -0.3274923 -0.5552129 -0.81633533
    ## bio10 -0.4886081 -0.71411350  0.416930231 -0.5151169 -0.7036900 -0.76592247
    ## bio11 -0.3297284 -0.72423144  0.492885864 -0.3651390 -0.6835638 -0.82992875
    ## bio12  0.9064818  0.77619752 -0.243139096  0.9178974  0.8079134  0.76402046
    ## bio13  1.0000000  0.48404051  0.130409758  0.9902335  0.5245850  0.58835234
    ## bio14  0.4840405  1.00000000 -0.743674332  0.5012004  0.9859234  0.84673991
    ## bio15  0.1304098 -0.74367433  1.000000000  0.1244492 -0.7294802 -0.46226779
    ## bio16  0.9902335  0.50120043  0.124449153  1.0000000  0.5362200  0.61586400
    ## bio17  0.5245850  0.98592339 -0.729480228  0.5362200  1.0000000  0.81764699
    ## bio18  0.5883523  0.84673991 -0.462267795  0.6158640  0.8176470  1.00000000
    ## bio19  0.7906993  0.31690226  0.128299899  0.7920907  0.3839700  0.17654879
    ##             bio19
    ## bio1  -0.12414066
    ## bio2  -0.39644543
    ## bio3   0.07348362
    ## bio4  -0.48121938
    ## bio5  -0.34301616
    ## bio6   0.07715578
    ## bio7  -0.53249335
    ## bio8  -0.51356967
    ## bio9   0.10720732
    ## bio10 -0.26914814
    ## bio11  0.01511498
    ## bio12  0.74717071
    ## bio13  0.79069926
    ## bio14  0.31690226
    ## bio15  0.12829990
    ## bio16  0.79209065
    ## bio17  0.38396995
    ## bio18  0.17654879
    ## bio19  1.00000000

That’s great, but it’s a bit hard to pick variables this way. Let’s try
it visually instead.

``` r
raster.cor.plot(env)
```

    ## $cor.mds.plot

![](Readme_files/figure-markdown_github/collinearity2-1.png)

    ## 
    ## $cor.heatmap

![](Readme_files/figure-markdown_github/collinearity2-2.png)

The raster.cor.plot function gives us two visualizations. One heatmap
that colors pairs of predictors by their Pearson correlation
coefficient, and one cluster plot that does mds scaling of the predictor
variables and then plots them in a two dimensional space so that more
correlated predictors are closer to each other. We’re going to make an
arbitrary decision to just use three predictors, and to keep those
predictors relatively uncorrelated we’ll select predictors that are far
apart in this mds plot. Here we’ll choose bio1, bio12, and bio7.

``` r
env <- env[[c("bio1", "bio12", "bio7")]]
plot(env)
```

![](Readme_files/figure-markdown_github/subsetenv-1.png)

``` r
raster.cor.matrix(env)
```

    ##              bio1      bio12        bio7
    ## bio1   1.00000000 -0.6084667 -0.05329392
    ## bio12 -0.60846674  1.0000000 -0.29133189
    ## bio7  -0.05329392 -0.2913319  1.00000000

### GLM

GLMs usually require the user to supply a formula, an enmtools.species
object, and some environmental data. If your formula is a strictly
additive function of all of the environmental layers in env, though,
enmtools.glm will build a formula automatically.

``` r
monticola.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ bio1 + bio12 + bio7, test.prop = 0.2)
```

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

``` r
monticola.glm
```

    ## 
    ## 
    ## Formula:  presence ~ bio1 + bio12 + bio7
    ## <environment: 0x7fd87f610498>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##  Longitude   Latitude   bio1   bio12   bio7   presence
    ## ----------  ---------  -----  ------  -----  ---------
    ##  -5.171215   43.06957     78     917    249          1
    ##  -6.036635   43.02531     76    1012    246          1
    ##  -7.679727   40.38852    137    1143    247          1
    ##  -7.790437   40.30959    129    1231    242          1
    ##  -7.473340   43.78935    140     931    179          1
    ##  -6.575039   42.91070     84    1012    247          1
    ##  -5.132756   43.49572    133     822    190          1
    ##  -7.787378   40.39362    137    1143    247          1
    ##  -4.941888   43.35310    128     843    194          1
    ##  -7.621731   40.34170    101    1514    229          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6444  -0.7911  -0.4964   0.8343   2.2132  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  8.957e+00  2.229e+00   4.019 5.86e-05 ***
    ## bio1        -3.578e-02  6.595e-03  -5.425 5.78e-08 ***
    ## bio12       -1.152e-05  7.264e-04  -0.016    0.987    
    ## bio7        -2.120e-02  5.029e-03  -4.216 2.49e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.70  on 719  degrees of freedom
    ## Residual deviance: 511.24  on 716  degrees of freedom
    ## AIC: 250.13
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7307505 
    ## cor            : 0.3106546 
    ## max TPR+TNR at : -0.00905343 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.4181409 
    ## cor            : -0.01823037 
    ## max TPR+TNR at : 0.3930726 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.7401593 
    ## cor            : 0.1923954 
    ## max TPR+TNR at : 0.09541808 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.42735 
    ## cor            : -0.007021744 
    ## max TPR+TNR at : 0.5236008 
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
    ## values     : 0.02493113, 0.9967671  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_github/build_glms1-1.png)

Notice this produces the same formula as:

``` r
monticola.glm <- enmtools.glm(species = monticola, env = env, test.prop = 0.2)
```

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

``` r
monticola.glm
```

    ## 
    ## 
    ## Formula:  presence ~ bio1 + bio12 + bio7
    ## <environment: 0x7fd89739b5b8>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##       Longitude   Latitude   bio1   bio12   bio7   presence
    ## ---  ----------  ---------  -----  ------  -----  ---------
    ## 1     -5.171215   43.06957     78     917    249          1
    ## 2     -6.036635   43.02531     76    1012    246          1
    ## 3     -7.679727   40.38852    137    1143    247          1
    ## 6     -6.575039   42.91070     84    1012    247          1
    ## 8     -7.787378   40.39362    137    1143    247          1
    ## 9     -4.941888   43.35310    128     843    194          1
    ## 10    -7.621731   40.34170    101    1514    229          1
    ## 13    -6.990000   42.57000    107     893    253          1
    ## 14    -7.950000   43.30000    120    1200    194          1
    ## 15    -7.100000   42.93000    115     935    234          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6292  -0.7933  -0.5011   0.8362   2.1837  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  9.0382969  2.2768562   3.970 7.20e-05 ***
    ## bio1        -0.0356784  0.0065688  -5.431 5.59e-08 ***
    ## bio12       -0.0001353  0.0007499  -0.180    0.857    
    ## bio7        -0.0211254  0.0051330  -4.116 3.86e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.70  on 719  degrees of freedom
    ## Residual deviance: 512.49  on 716  degrees of freedom
    ## AIC: 252.03
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7319524 
    ## cor            : 0.3097709 
    ## max TPR+TNR at : 0.03635544 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.4232428 
    ## cor            : -0.01788326 
    ## max TPR+TNR at : 0.4010422 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.7374925 
    ## cor            : 0.1911097 
    ## max TPR+TNR at : 0.01289892 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.4199731 
    ## cor            : -0.00860026 
    ## max TPR+TNR at : 0.5031497 
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
    ## values     : 0.02655582, 0.9963761  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_github/build_glms2-1.png)

If you want a more complicated formula, though (e.g., with interactions
or polynomial effects), you’ll need to supply that manually.

``` r
monticola.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) * poly(bio12, 2), test.prop = 0.2)
```

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

``` r
monticola.glm
```

    ## 
    ## 
    ## Formula:  presence ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2) + poly(bio7, 
    ##     2):poly(bio12, 2)
    ## <environment: 0x7fd887fc2ac0>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##       Longitude   Latitude   bio1   bio12   bio7   presence
    ## ---  ----------  ---------  -----  ------  -----  ---------
    ## 2     -6.036635   43.02531     76    1012    246          1
    ## 4     -7.790437   40.30959    129    1231    242          1
    ## 5     -7.473340   43.78935    140     931    179          1
    ## 6     -6.575039   42.91070     84    1012    247          1
    ## 7     -5.132756   43.49572    133     822    190          1
    ## 8     -7.787378   40.39362    137    1143    247          1
    ## 9     -4.941888   43.35310    128     843    194          1
    ## 10    -7.621731   40.34170    101    1514    229          1
    ## 14    -7.950000   43.30000    120    1200    194          1
    ## 15    -7.100000   42.93000    115     935    234          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3420  -0.7132  -0.3920   0.6893   2.8463  
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -1.0486     0.4298  -2.439   0.0147 *  
    ## poly(bio1, 2)1                  -31.2522     6.0225  -5.189 2.11e-07 ***
    ## poly(bio1, 2)2                  -31.8242     6.8084  -4.674 2.95e-06 ***
    ## poly(bio7, 2)1                  -23.0516    13.1412  -1.754   0.0794 .  
    ## poly(bio7, 2)2                   11.6572    10.4187   1.119   0.2632    
    ## poly(bio12, 2)1                  27.9920    12.7896   2.189   0.0286 *  
    ## poly(bio12, 2)2                 -16.0955    10.3089  -1.561   0.1184    
    ## poly(bio7, 2)1:poly(bio12, 2)1 -208.0519   453.1052  -0.459   0.6461    
    ## poly(bio7, 2)2:poly(bio12, 2)1 -347.8826   276.2943  -1.259   0.2080    
    ## poly(bio7, 2)1:poly(bio12, 2)2  363.5869   268.4933   1.354   0.1757    
    ## poly(bio7, 2)2:poly(bio12, 2)2  202.9025   156.3711   1.298   0.1944    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.7  on 719  degrees of freedom
    ## Residual deviance: 454.4  on 709  degrees of freedom
    ## AIC: 243
    ## 
    ## Number of Fisher Scoring iterations: 5
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7920861 
    ## cor            : 0.4124899 
    ## max TPR+TNR at : 0.05445894 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.7613937 
    ## cor            : 0.1296033 
    ## max TPR+TNR at : 0.4603189 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.7847431 
    ## cor            : 0.258389 
    ## max TPR+TNR at : 0.205913 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.7573923 
    ## cor            : 0.06486546 
    ## max TPR+TNR at : 0.3688106 
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
    ## values     : 2.220446e-16, 0.954622  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_github/build_glms3-1.png)

To check out the marginal response functions, you only need to type

``` r
monticola.glm$response.plots
```

    ## $bio1

![](Readme_files/figure-markdown_github/response_plots-1.png)

    ## 
    ## $bio7

![](Readme_files/figure-markdown_github/response_plots-2.png)

    ## 
    ## $bio12

![](Readme_files/figure-markdown_github/response_plots-3.png)

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

``` r
visualize.enm(monticola.glm, env, layers = c("bio1", "bio12"), plot.test.data = TRUE)
```

    ## $background.plot

    ## Warning: Removed 396 rows containing missing values (geom_raster).

![](Readme_files/figure-markdown_github/visualize.enm-1.png)

    ## 
    ## $suit.plot

![](Readme_files/figure-markdown_github/visualize.enm-2.png)

### GAM, Bioclim, Domain, and Maxent

The procedure for building Bioclim, Domain, and Maxent models is similar
to the procedure for GLMs, with the exception that you do not need to
pass a formula to the model function for Maxent, Domain, and Bioclim
models. Note that running Maxent models requires a bit of extra setup;
see dismo documentation for details.

``` r
monticola.gam <- enmtools.gam(monticola, env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) * poly(bio12, 2), test.prop = 0.2)
monticola.dm <- enmtools.dm(monticola, env, test.prop = 0.2)
monticola.bc <- enmtools.bc(monticola, env, test.prop = 0.2)
monticola.mx <- enmtools.maxent(monticola, env, test.prop = 0.2)
```

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

``` r
raster.breadth(monticola.glm)
```

    ## $B1
    ## [1] 0.9466589
    ## 
    ## $B2
    ## [1] 0.5519553

ENMTools also provides metrics for measuring similarity between ENMs.
These include Schoener’s D (Schoener 1968), I (Warren et al. 2008), and
the Spearman rank correlation coefficient between two rasters. While D
and I are commonly used in the ENM literature, they may tend to
overestimate similarity between ENMs when many grid cells are of similar
values (e.g., when two species prefer different habitat but the region
contains a great deal of habitat that is unsuitable for both).

``` r
monticola.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = 0.2)
```

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

``` r
cyreni.glm <- enmtools.glm(species = cyreni, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = 0.2)
```

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

``` r
raster.overlap(monticola.glm, cyreni.glm)
```

    ## $D
    ## [1] 0.6718855
    ## 
    ## $I
    ## [1] 0.8888624
    ## 
    ## $rank.cor
    ## [1] 0.3138799

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

``` r
monticola.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = 0.2)
```

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

``` r
cyreni.glm <- enmtools.glm(species = monticola, env = env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = 0.2)
```

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

``` r
env.overlap(monticola.glm, cyreni.glm, env, tolerance = .001)
```

    ## [1] "Trying to find starting conditions, attempt 1"
    ## [1] "Building replicates..."

    ## $env.D
    ## [1] 0.9635861
    ## 
    ## $env.I
    ## [1] 0.9980813
    ## 
    ## $env.cor
    ## [1] 0.9981912
    ## 
    ## $env.D.plot

![](Readme_files/figure-markdown_github/env_overlap-1.png)

    ## 
    ## $env.I.plot

![](Readme_files/figure-markdown_github/env_overlap-2.png)

    ## 
    ## $env.cor.plot

![](Readme_files/figure-markdown_github/env_overlap-3.png)

The plots that come out of these environment space functions are used
for diagnosing convergence of the overlap/breadth metric. Ideally what
you want is a relationship between the metric and the number of samples
that shows no clear directional trend.

Hypothesis testing
------------------

### Niche identity or equivalency test

In this example, we will run a niche identity (also called equivalency)
test, as in Warren et al. 2008. This test takes the presence points for
a pair of species and randomly reassigns them to each species, then
builds ENMs for these randomized occurrences. By doing this many times,
we can estimate the probability distribution for ENM overlap between
species under the null hypothesis that the two species’ occurrences in
the environment are effectively a random draw from the same underlying
distribution. Note that niche evolution is only one of many reasons why
two species’ realized environmental distributions might cause departures
from this null hypothesis. See Warren et al. 2014 for details.

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

``` r
id.glm <- identity.test(species.1 = monticola, species.2 = cyreni, env = env, type = "glm", nreps = 4)
```

``` r
id.glm
```

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
    ## empirical    0.3244487   0.5929443   0.1049357   0.3677457   0.6156208   0.1353276
    ## rep 1        0.8970922   0.9904049   0.8965106   0.8720274   0.9759294   0.8666152
    ## rep 2        0.9370161   0.9964323   0.9954697   0.9567992   0.9961165   0.9958503
    ## rep 3        0.9442894   0.9967116   0.9772274   0.9473453   0.9965164   0.9797720
    ## rep 4        0.9329538   0.9955566   0.9391922   0.9105042   0.9884824   0.9273312

![](Readme_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Background or similarity test

The background or similarity test compares the overlap seen between two
species’ ENMs to the overlap expected by chance if one or both species
was effectively choosing habitat at random from within their broad
geographic range. The purpose of this test is to correct for the
availability of habitat and ask whether the observed similarity between
species or populations is significantly more (or less) than expected
given the available set of environments in the regions in which they
occur.

*NOTE:* In order for models to be comparable, both empirical and
pseudoreplicate models for the background test are conducted with
pseudoabsence points pooled for the two species being compared.

In Warren et al. 2008, we developed this test in the context of
comparing one species’ actual occurrence to the random background
occurrences of the other species. This is what we call an “asymmetric”
test, and in our case we did the test in both directions with the idea
that we might compare the results of A vs. B background to the results
of B vs. A background. This may be informative in some cases, but many
people have also found this asymmetry confusing (and indeed it is often
difficult to interpret). For that reason, the background test here can
be conducted against a null hypothesis that is generated from
“asymmetric” (species.1 vs species.2 background) or “symmetric”
(species.1 background vs. species.2 background) comparisons.

Here, for instance, is a Bioclim background test using the classical
asymmetric approach:

``` r
bg.bc.asym <- background.test(species.1 = monticola, species.2 = cyreni, env = env, type = "bc", nreps = 4, test.type = "asymmetric" )
```

``` r
bg.bc.asym
```

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
    ## empirical    0.0242365   0.1455829   -0.0450249   0.0140560   0.1112921   0.1600544
    ## rep 1        0.6058605   0.8582046    0.6501127   0.4774109   0.7406193   0.6308582
    ## rep 2        0.6539330   0.8946606    0.7399613   0.5531113   0.8025018   0.6733972
    ## rep 3        0.6719038   0.9064508    0.7844833   0.5852861   0.8299980   0.7449234
    ## rep 4        0.6584314   0.8855979    0.7264155   0.5268244   0.7687682   0.6512239

![](Readme_files/figure-markdown_github/unnamed-chunk-5-1.png)

And here is a Domain background test using the symmetric approach:

``` r
bg.dm.sym <- background.test(species.1 = monticola, species.2 = cyreni, env = env, type = "dm", nreps = 4, test.type = "symmetric" )
```

``` r
bg.dm.sym
```

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
    ## empirical    0.1974048   0.4272913   -0.1020590   0.0780030   0.2651276   0.2207599
    ## rep 1        0.9377558   0.9963716    0.6378358   0.8362503   0.9572793   0.9210746
    ## rep 2        0.9562179   0.9959520    0.7741840   0.8677725   0.9582127   0.9592936
    ## rep 3        0.9401910   0.9930586    0.6798451   0.7888838   0.9202337   0.9465968
    ## rep 4        0.9729291   0.9976050    0.8570497   0.8889699   0.9676164   0.9590800

![](Readme_files/figure-markdown_github/unnamed-chunk-7-1.png)

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
we’ll just pick a couple of environmental layers, though (bio1 and
bio2). Here’s an equivalency/identity test:

``` r
esp.id <- enmtools.ecospat.id(monticola, cyreni, env[[c("bio1", "bio12")]])
```

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.

``` r
esp.id
```

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

![](Readme_files/figure-markdown_github/ecospat_identity-1.png)![](Readme_files/figure-markdown_github/ecospat_identity-2.png)

    ## NULL

And here’s a symmetric background test. The difference between symmetric
and asymmetric for these tests is the same as for the background tests
presented above.

``` r
esp.bg.sym <- enmtools.ecospat.bg(monticola, cyreni, env[[c("bio1", "bio12")]], test.type = "symmetric")
```

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.

``` r
esp.bg.sym
```

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
    ## 0.42 0.32

![](Readme_files/figure-markdown_github/ecospat_background-1.png)![](Readme_files/figure-markdown_github/ecospat_background-2.png)

    ## NULL

Note that if you provide more than two layers to the enmtools.ecospat
function, it will performa a PCA analysis on the provided layers and
measure overlaps on the first two axes of that PCA space.

``` r
esp.bg.sym <- enmtools.ecospat.bg(monticola, cyreni, env, test.type = "symmetric")
```

    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## 
    ## 
    ## No background points provided, drawing background from range raster.
    ## 
    ## [1] "More than two layers in environment stack and no layers argument passed, performing PCA..."

``` r
esp.bg.sym
```

    ## 
    ## 
    ##  
    ## 
    ## Ecospat background test symmetric monticola vs. cyreni
    ## 
    ## ecospat.bg test empirical overlaps:
    ## $D
    ## [1] 0.01293428
    ## 
    ## $I
    ## [1] 0.109648
    ## 
    ## 
    ## 
    ## ecospat.bg test p-values:
    ##    D    I 
    ## 0.58 0.58

![](Readme_files/figure-markdown_github/ecospat_background2-1.png)![](Readme_files/figure-markdown_github/ecospat_background2-2.png)

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
the identity and background tests. Here’s a linear one using GLM models:

``` r
rbl.glm <- rangebreak.linear(monticola, cyreni, env, type = "glm", nreps = 4)
```

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

``` r
rbl.glm
```

    ## 
    ## 
    ##  
    ## 
    ## Linear rangebreak test monticola vs. cyreni
    ## 
    ## rangebreak test p-values:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.4      0.4      0.4      0.4      0.4      1.0 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I    rank.cor       env.D       env.I      env.cor
    ## ----------  ----------  ----------  ----------  ----------  ----------  -----------
    ## empirical    0.3244487   0.5929443   0.1049357   0.3684403   0.6159160    0.1360303
    ## rep 1        0.7367776   0.9371496   0.3525105   0.6399078   0.8469776   -0.1557731
    ## rep 2        0.2992174   0.5582095   0.0556068   0.3491892   0.5868418    0.0372725
    ## rep 3        0.7314563   0.9346716   0.3261329   0.6236088   0.8356646   -0.2284763
    ## rep 4        0.7234739   0.9294263   0.3020399   0.6436864   0.8429577   -0.1495767

![](Readme_files/figure-markdown_github/rangebreak_linear-1.png)![](Readme_files/figure-markdown_github/rangebreak_linear-2.png)

And here’s a blob test using Bioclim:

``` r
rbb.bc <- rangebreak.blob(monticola, cyreni, env, type = "bc", nreps = 4)
```

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

``` r
rbb.bc
```

    ## 
    ## 
    ##  
    ## 
    ## blob rangebreak test monticola vs. cyreni
    ## 
    ## rangebreak test p-values:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      1.0      1.0      1.0      0.8      0.8      1.0 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I     rank.cor       env.D       env.I     env.cor
    ## ----------  ----------  ----------  -----------  ----------  ----------  ----------
    ## empirical    0.0242365   0.1455829   -0.0450249   0.0138993   0.1115047   0.1613371
    ## rep 1        0.0242365   0.1455829   -0.0450249   0.0136313   0.1098929   0.1570864
    ## rep 2        0.0242365   0.1455829   -0.0450249   0.0140387   0.1120468   0.1593494
    ## rep 3        0.0242365   0.1455829   -0.0450249   0.0134696   0.1092234   0.1598079
    ## rep 4        0.0242365   0.1455829   -0.0450249   0.0137475   0.1104920   0.1580641

![](Readme_files/figure-markdown_github/rangebreak_blob-1.png)

If you want to access the individual replicates (for instance to see how
your ranges are being split up), you can find them in the list named
“replicate.models” inside your rangebreak test object.

``` r
rbl.glm$replicate.models$monticola.rep.1
```

    ## 
    ## 
    ## Formula:  presence ~ bio1 + bio12 + bio7
    ## <environment: 0x7fd887c1c8e8>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##  Longitude   Latitude   bio1   bio12   bio7   presence
    ## ----------  ---------  -----  ------  -----  ---------
    ##   0.130000    42.6400     61    1053    238          1
    ##  -0.020000    42.5900     62    1027    240          1
    ##  -2.450000    42.4600    126     512    265          1
    ##  -4.660000    43.2100    113     877    209          1
    ##  -4.790000    43.2900     90     958    223          1
    ##  -4.790000    43.2900     90     958    223          1
    ##  -4.780000    43.2000     90     958    223          1
    ##  -4.780000    43.2000     90     958    223          1
    ##  -4.941888    43.3531    128     843    194          1
    ##  -4.910000    43.2900     85     975    228          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6659  -0.7681  -0.5174   0.7605   2.0156  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) 15.498098   2.550909   6.076 1.24e-09 ***
    ## bio1        -0.051271   0.006526  -7.856 3.96e-15 ***
    ## bio12       -0.003070   0.000849  -3.617 0.000298 ***
    ## bio7        -0.030088   0.005674  -5.303 1.14e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 720.87  on 901  degrees of freedom
    ## Residual deviance: 624.54  on 898  degrees of freedom
    ## AIC: 305.43
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 260 
    ## n absences     : 642 
    ## AUC            : 0.7537173 
    ## cor            : 0.3403305 
    ## max TPR+TNR at : -0.09402619 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 260 
    ## n absences     : 10000 
    ## AUC            : 0.4336342 
    ## cor            : -0.007358654 
    ## max TPR+TNR at : 0.4653531 
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
    ## values     : 0.01937755, 0.9989029  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_github/rbl_reps-1.png)

``` r
rbl.glm$replicate.models$cyreni.rep.1
```

    ## 
    ## 
    ## Formula:  presence ~ bio1 + bio12 + bio7
    ## <environment: 0x7fd884061b98>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##  Longitude   Latitude   bio1   bio12   bio7   presence
    ## ----------  ---------  -----  ------  -----  ---------
    ##   -7.95000   43.66000    129    1080    176          1
    ##   -5.32176   40.35238     85     625    294          1
    ##   -5.29000   40.31000     89     646    296          1
    ##   -5.29000   40.31000     89     646    296          1
    ##   -7.83000   43.48000    118    1153    193          1
    ##   -7.11000   42.57000     95    1051    246          1
    ##   -6.76000   42.12000     84     980    264          1
    ##   -6.76000   42.12000     84     980    264          1
    ##   -8.07000   43.75000    134    1045    166          1
    ##   -7.95000   43.57000    129    1080    176          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8147  -0.3998  -0.2550  -0.1753   1.6529  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.491326   4.005133  -0.872 0.383365    
    ## bio1        -0.014774   0.012123  -1.219 0.222991    
    ## bio12        0.004464   0.001301   3.431 0.000601 ***
    ## bio7         0.005490   0.008481   0.647 0.517383    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 210.72  on 847  degrees of freedom
    ## Residual deviance: 170.59  on 844  degrees of freedom
    ## AIC: 87.858
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 76 
    ## n absences     : 772 
    ## AUC            : 0.7811222 
    ## cor            : 0.2745855 
    ## max TPR+TNR at : -0.3666092 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 76 
    ## n absences     : 10000 
    ## AUC            : 0.4959961 
    ## cor            : 0.007125081 
    ## max TPR+TNR at : 0.4092848 
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
    ## values     : 0.02669289, 0.9994239  (min, max)
    ## 
    ## 
    ## 
    ## Notes:

![](Readme_files/figure-markdown_github/rbl_reps-2.png)

For the ribbon rangebreak test, you will need one extra thing; a third
enmtools.species object representing the occurrence points (for one or
both species) that fall within the ribbon of putatively unsuitable
habitat. In the case of these two lizards we don’t have such a ribbon,
so we’ll just simulate one based on some random points.

``` r
plot(env[[1]])
points(cyreni$presence.points, col = "red")
points(monticola$presence.points, col = "blue")

ribbon <- enmtools.species(species.name = "ribbon")
ribbon$presence.points <- data.frame(Longitude = runif(n = 10, min = -9, max = 0),
                                      Latitude = runif(n = 10, min = 40.5, max = 42))
points(ribbon$presence.points, pch = 16)
```

![](Readme_files/figure-markdown_github/build_ribbon-1.png)

``` r
ribbon$range <- background.raster.buffer(ribbon$presence.points, 20000, mask = env)
ribbon
```

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
    ##  -0.5190984   41.52450
    ##  -2.9586995   41.33876
    ##  -7.6472751   41.15329
    ##  -6.5151506   40.74633
    ##  -0.1808655   41.69522
    ##  -4.3387245   41.97236
    ##  -5.9279990   40.65496
    ##  -0.2388877   40.95952
    ##  -2.2195772   40.83546
    ##  -2.1471651   41.03296
    ## 
    ## 
    ## Background points not defined.
    ## 
    ## Species name:  ribbon

Now we’ll run a ribbon rangebreak test using GLM models with quadratic
effects. We also need to tell it the width of the ribbons to generate
for the replicates. The units for the width argument are the same units
that the presence points are in; e.g., if the points are in decimal
degrees you should supply the width of the barrier in decimal degrees.

``` r
rbr.glm <- rangebreak.ribbon(monticola, cyreni, ribbon, env, type = "glm", f = pres ~ poly(bio1, 2) + poly(bio12, 2) + poly(bio7, 2), width = 0.5, nreps = 4)
```

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

``` r
rbr.glm
```

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
    ##      0.4      0.4      1.2      0.4      0.4      1.2 
    ## 
    ## Species 1 vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.8      0.8      0.8      0.4      0.4      0.4 
    ## 
    ## Species 2 vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.4      0.4      0.4      0.4      0.4      0.4 
    ## 
    ## Outside vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.8      0.8      0.8      1.2      0.4      0.4 
    ## 
    ## 
    ## Replicates:
    ## 
    ## Species 1 vs. Species 2:
    ##                   D         I   rank.cor     env.D     env.I      env.cor
    ## empirical 0.1131296 0.3179598 -0.1224431 0.0874310 0.2013877  0.002959439
    ## rep 1     0.2010182 0.4500530  0.3163082 0.2048888 0.4458920  0.484436665
    ## rep 2     0.2943660 0.5294455 -0.2285120 0.2578637 0.3856800 -0.096357440
    ## rep 3     0.1698312 0.4039546  0.2583394 0.1726204 0.3985242  0.410736974
    ## rep 4     0.3720688 0.6028250 -0.2084978 0.2429237 0.3641753 -0.210502937
    ## 
    ## Species 1 vs. Ribbon:
    ##                   D         I    rank.cor     env.D     env.I      env.cor
    ## empirical 0.3716422 0.6266750 -0.03257543 0.1127960 0.2359181 -0.179351247
    ## rep 1     0.4046526 0.6794684  0.39255805 0.1570915 0.3846699  0.584792586
    ## rep 2     0.2998159 0.5486657 -0.08934095 0.2810876 0.4101816 -0.003167514
    ## rep 3     0.4042510 0.6746900  0.41716472 0.1649519 0.3998389  0.611950745
    ## rep 4     0.4377208 0.7117987  0.34805111 0.2361334 0.4291054  0.222033642
    ## 
    ## Species 2 vs. Ribbon:
    ##                   D         I  rank.cor      env.D      env.I    env.cor
    ## empirical 0.2445532 0.4389351 0.3889232 0.02802897 0.05897263 -0.2418362
    ## rep 1     0.4883325 0.7643652 0.9199006 0.50459424 0.74490817  0.8603930
    ## rep 2     0.8871702 0.9801727 0.9856084 0.94003083 0.98412736  0.9914659
    ## rep 3     0.4492870 0.7208281 0.9015008 0.54881411 0.77094183  0.8224641
    ## rep 4     0.6931527 0.8641988 0.7477696 0.55390420 0.78233981  0.8499550
    ## 
    ## Outside vs. Ribbon:
    ##                   D         I   rank.cor     env.D     env.I    env.cor
    ## empirical 0.4437471 0.7049832 0.14823652 0.1574158 0.3117742 -0.1425710
    ## rep 1     0.4095001 0.6778612 0.37659838 0.1234613 0.3290120  0.4152267
    ## rep 2     0.3462521 0.6031074 0.05730232 0.3576070 0.4982152  0.0849811
    ## rep 3     0.4160359 0.6835539 0.43567542 0.1377689 0.3580384  0.5108408
    ## rep 4     0.4906136 0.7603765 0.54419642 0.3437760 0.5694442  0.4723018

![](Readme_files/figure-markdown_github/rangebreak_ribbon-1.png)![](Readme_files/figure-markdown_github/rangebreak_ribbon-2.png)![](Readme_files/figure-markdown_github/rangebreak_ribbon-3.png)![](Readme_files/figure-markdown_github/rangebreak_ribbon-4.png)

Note that the output table here has slope, intercept, and intercept
offset.

``` r
rbr.glm$lines.df
```

    ##       slope intercept    offset
    ## 1  2.516692  60.59417 0.6770219
    ## 2 -2.440047  30.64741 0.6592528
    ## 3  1.917778  56.37219 0.5407096
    ## 4 -1.047192  36.98274 0.3619921

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
enmtools.species objects. It’s important that the names of the species
objects and their species.name attributes match the names in the
phylogeny’s tip.labels. For demonstration, we’re going to build an
object for a clade of five anoles from Hispaniola. We have the tree, so
we’re just going to grab occurrence data from GBIF using the rgbif
package.

``` r
library(rgbif)
library(ape)

hisp.anoles <- read.nexus(file = "./testdata/StarBEAST_MCC.species.txt")

keepers <- c("brevirostris", "marron", "caudalis", "websteri", "distichus")

hisp.anoles <- drop.tip(phy = hisp.anoles, tip = hisp.anoles$tip.label[!hisp.anoles$tip.label %in% keepers])
plot(hisp.anoles)
```

![](Readme_files/figure-markdown_github/read_tree-1.png)

So there’s our tree. Now we’re going to grab some environmental data.

``` r
hisp.env <- stack(list.files("./testdata/Hispaniola_Worldclim", full.names = TRUE))
hisp.env <- setMinMax(hisp.env)
```

And then we’ll create a function to build species from GBIF.

``` r
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
```

Now we’ll create five species and add them to a species.clade object
that is called brev.clade.

``` r
brevirostris <- species.from.gbif(genus = "Anolis", species = "brevirostris", env = hisp.env)
marron <- species.from.gbif(genus = "Anolis", species = "marron", env = hisp.env)
caudalis <- species.from.gbif(genus = "Anolis", species = "caudalis", env = hisp.env)
websteri <- species.from.gbif(genus = "Anolis", species = "websteri", env = hisp.env)
distichus <- species.from.gbif(genus = "Anolis", species = "distichus", env = hisp.env)


brev.clade <- enmtools.clade(species = list(brevirostris, marron, caudalis, websteri, distichus), tree = hisp.anoles)
check.clade(brev.clade)
```

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
    ## [1] "brevirostris" "caudalis"     "distichus"    "marron"       "websteri"    
    ## 
    ## Rooted; includes branch lengths.
    ## 
    ## 
    ## Data Summary: 
    ## 
    ## 
    ##                species.names   in.tree   presence   background   range   
    ## -------------  --------------  --------  ---------  -----------  --------
    ## brevirostris   brevirostris    TRUE      187        0            present 
    ## caudalis       caudalis        TRUE      21         0            present 
    ## distichus      distichus       TRUE      780        0            present 
    ## marron         marron          TRUE      13         0            present 
    ## websteri       websteri        TRUE      18         0            present

That’s one way to build a clade object by hand, but there’s already one
built into ENMTools to experiment with so we’ll just use that.

``` r
data(iberolacerta.clade)
```

### Age-overlap correlation tests (AOC)

The AOC tests allow you to examine patterns of range, point, and ENM
overlap in the context of a phylogeny. This is effectively a generalized
version of several analyses: age-range correlation (e.g., Fitzpatrick
and Turelli 2006), ENM overlap in the context of a phylogeny (e.g.,
Knouft et al. 2006, Warren et al. 2008), and point overlaps (e.g.,
Cardillo and Warren 2016).

These tests require the creation of an enmtools.clade object, as above.
AOC tests consist of two steps: first, the average overlap at each node
in the phylogeny is calcualted using a method that takes tree topology
into account (see Fitzpatrick and Turelli 2006), then we perform a
linear regression to measure the relationship between node age and
average overlap. Due to the fact that these overlaps violate many of the
assumptions of a regular linear regression, however (e.g., errors are
not iid), we can’t calculate significance in the typical way. Instead we
performa Monte Carlo test, permuting the identity of the tips of the
tree and repeating the node averaging and modeling steps. Finally we
measure statistical significance by comparing the empirical slope and
intercept to the distribution of slopes and intercepts from the Monte
Carlo replicates.

First, let’s do one using geog.range.overlaps, as in Fitzpatrick and
Turelli 2006. Note that this analysis requires that each of your species
have a range raster stored in their species object (we did that as part
of the function used above).

``` r
range.aoc <- enmtools.aoc(clade = iberolacerta.clade,  nreps = 50, overlap.source = "range")
summary(range.aoc)
```

    ## 
    ## 
    ## Age-Overlap Correlation test
    ## 
    ## 50 replicates 
    ## 
    ## p values:
    ##      (Intercept) empirical.df$age 
    ##       0.03921569       0.07843137

![](Readme_files/figure-markdown_github/range_aoc-1.png)![](Readme_files/figure-markdown_github/range_aoc-2.png)

    ## NULL

Now we can do one using point overlaps just by changing the
overlap.source argument:

``` r
point.aoc <- enmtools.aoc(clade = iberolacerta.clade,  nreps = 50, overlap.source = "points")
summary(point.aoc)
```

    ## 
    ## 
    ## Age-Overlap Correlation test
    ## 
    ## 50 replicates 
    ## 
    ## p values:
    ##      (Intercept) empirical.df$age 
    ##        0.2745098        0.4313725

![](Readme_files/figure-markdown_github/point_aoc-1.png)![](Readme_files/figure-markdown_github/point_aoc-2.png)

    ## NULL

Or we can use similarity between ENMs built for each species. Here we’ll
use GLM models:

``` r
glm.aoc <- enmtools.aoc(clade = iberolacerta.clade,  env = env, nreps = 50, overlap.source = "glm", f = pres ~ poly(bio1, 2) + poly(bio12, 2))
```

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

``` r
summary(glm.aoc)
```

    ## 
    ## 
    ## Age-Overlap Correlation test
    ## 
    ## 50 replicates 
    ## 
    ## p values:
    ##      (Intercept) empirical.df$age 
    ##        0.1960784        0.2745098

![](Readme_files/figure-markdown_github/enm_aoc-1.png)![](Readme_files/figure-markdown_github/enm_aoc-2.png)

    ## NULL

### Literature cited

*Broennimann, O., Fitzpatrick, M. C., Pearman, P. B., Petitpierre, B.,
Pellissier, L., Yoccoz, N. G., Thuiller, W., Fortin, M.-J., Randin, C.,
Zimmermann, N. E., Graham, C. H. and Guisan, A. (2012), Measuring
ecological niche overlap from occurrence and spatial environmental data.
Global Ecology and Biogeography, 21: 481–497.
<a href="doi:10.1111/j.1466-8238.2011.00698.x" class="uri">doi:10.1111/j.1466-8238.2011.00698.x</a>*

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

*Warren, D.L., L. Beaumont, R. Dinnage, and J. Baumgartner. 2019. New
methods for measuring ENM breadth and overlap in environmental space.
Ecography. Doi: 10.1111/ecog.03900*
