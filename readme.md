ENMTools
======================
This package implements various tests, visualizations, and metrics for use with environmental niche models (ENMs) and species distribution models (SDMs). 

-----

# Installation

At present, ENMTools is downloadable from https://github.com/danlwarren/ENMTools.  There are multiple ways to download it.  The easiest is to use devtools and install from GitHub.

### Installing from GitHub using devtools
Run the following code from your R console:


```r
install.packages("devtools")
library(devtools)
install_github("danlwarren/ENMTools")
library(ENMTools)
```

### Install from zip file

A zipped version of the package is available at https://github.com/danlwarren/ENMTools/archive/master.zip.  To install from the zip file, download a copy of it to your system.  Once it's finished downloading, type the following (where PATH is the path to the zip file):


```r
install.packages("devtools")
library(devtools)
install_local("PATH")
library(ENMTools)
```




-----


# Interacting with ENMTools

### Creating enmtools.species objects

First we're going to load in some environmental data.

```r
env.files <- list.files(path = "test/testdata/", pattern = "pc", full.names = TRUE)
env <- stack(env.files)
names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4")
env <- setMinMax(env)
```

ENMTools is primarily designed to examine patterns of similarity and difference between ENMs for different species.  In order to simplify interactions with the functions in ENMTools, you need to put your data for each of your species into an enmtools.species object.  You can create and view an empty enmtools.species object just by typing:


```r
ahli <- enmtools.species()
ahli
```

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
```

You can add data to this object manually:

```r
names(ahli)
```

```
## [1] "range"             "presence.points"   "background.points"
## [4] "models"            "species.name"
```

```r
ahli$species.name <- "ahli"
ahli$presence.points <- read.csv("test/testdata/ahli.csv")[,3:4]
ahli$range <- background.raster.buffer(ahli$presence.points, 50000, mask = env)
ahli$background.points <- background.points.buffer(points = ahli$presence.points,
                                                   radius = 20000, n = 1000, mask = env[[1]])

ahli
```

```
## 
## 
## Range raster: 
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer.1 
## values      : 1, 1  (min, max)
## 
## 
## 
## Presence points (first ten only): 
## 
## | Longitude| Latitude|
## |---------:|--------:|
## |  -80.0106|  21.8744|
## |  -79.9086|  21.8095|
## |  -79.8065|  21.7631|
## |  -79.8251|  21.8095|
## |  -79.8807|  21.8374|
## |  -79.9550|  21.8374|
## |  -80.3446|  22.0136|
## |  -80.2983|  21.9951|
## |  -80.1776|  21.9023|
## |  -80.1591|  21.9673|
## 
## 
## Background points (first ten only): 
## 
## | Longitude| Latitude|
## |---------:|--------:|
## | -79.98726| 22.12087|
## | -79.97892| 22.07920|
## | -79.87059| 22.02920|
## | -79.65392| 21.71254|
## | -80.09559| 21.87920|
## | -79.82059| 21.89587|
## | -80.37892| 22.00420|
## | -79.88726| 21.83754|
## | -79.77892| 21.93754|
## | -79.77892| 21.74587|
## 
## 
## Species name:  ahli
```


Or you can add bits of it when the object is created:

```r
allogus <- enmtools.species(species.name = "allogus", 
                            presence.points = read.csv("test/testdata/allogus.csv")[,3:4])

allogus$range <- background.raster.buffer(allogus$presence.points, 50000, mask = env)
allogus$background.points <- background.points.buffer(points = allogus$presence.points,
                                                      radius = 20000, n = 1000, mask = env[[1]])

allogus
```

```
## 
## 
## Range raster: 
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer.1 
## values      : 1, 1  (min, max)
## 
## 
## 
## Presence points (first ten only): 
## 
## | Longitude| Latitude|
## |---------:|--------:|
## |  -79.2527|  22.2109|
## |  -78.7774|  22.2241|
## |  -78.6189|  22.2373|
## |  -78.1039|  21.1809|
## |  -78.0247|  21.1809|
## |  -77.9983|  20.9301|
## |  -77.9719|  21.7091|
## |  -77.9719|  21.5507|
## |  -77.9323|  21.6167|
## |  -77.9323|  20.7320|
## 
## 
## Background points (first ten only): 
## 
## | Longitude| Latitude|
## |---------:|--------:|
## | -78.65392| 22.29587|
## | -75.55392| 20.17087|
## | -74.93726| 20.00420|
## | -74.22892| 20.15420|
## | -74.92892| 20.48754|
## | -77.93726| 21.78754|
## | -78.47059| 22.18754|
## | -75.60392| 20.95420|
## | -75.41226| 20.62087|
## | -78.17892| 21.30420|
## 
## 
## Species name:  allogus
```


## Building an ENM

ENMTools contains functions to simplify the ENM construction process.  Using enmtools.species objects and the correct modeling commands, we can build models very quickly.  These commands are primarily wrappers to dismo model construction and projection functions, and at present are only available for GLM, Maxent, Domain, and Bioclim models.  One of the nice bits about this setup is that it allows enmtools to automatically generate suitability maps, do model evaluation, and plot the marginal suitability of habitat for each variable separately.

### GLM

GLMs usually require the user to supply a formula, an enmtools.species object, and some environmental data.  If your formula is a strictly additive function of all of the environmental layers in env, though, enmtools.glm will build a formula automatically.

```r
ahli.glm <- enmtools.glm(species = ahli, env = env, f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, test.prop = 0.2)
```

```
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```r
allogus.glm <- enmtools.glm(allogus, env, f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, test.prop = 0.2)
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
```

```r
ahli.glm
```

```
## 
## 
## Formula:  presence ~ layer.1 + layer.2 + layer.3 + layer.4
## <environment: 0x2505cbca0>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |5  |  -79.8807|  21.8374|    2244|    1828|     945|     249|        1|
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |12 |  -80.1220|  21.9301|    2898|    1033|    1231|     242|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5139  -0.1813  -0.1200  -0.0731   3.1784  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) 56.288914  29.086750   1.935   0.0530 .
## layer.1     -0.015859   0.007669  -2.068   0.0386 *
## layer.2     -0.014360   0.007763  -1.850   0.0643 .
## layer.3      0.003298   0.007794   0.423   0.6721  
## layer.4     -0.010491   0.025604  -0.410   0.6820  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 117.94  on 1007  degrees of freedom
## AIC: 127.94
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7607083 
## cor            : 0.100331 
## max TPR+TNR at : -4.461572 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.708 
## cor            : 0.04612596 
## max TPR+TNR at : -5.193976 
## 
## 
## Suitability:  
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer 
## values      : 1.431795e-08, 0.999998  (min, max)
```

![plot of chunk build_glms](figure/build_glms-1.png)

Notice this produces the same formula as:


```r
ahli.glm <- enmtools.glm(species = ahli, env = env, test.prop = 0.2)
```

```
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```r
ahli.glm
```

```
## 
## 
## Formula:  presence ~ layer.1 + layer.2 + layer.3 + layer.4
## <environment: 0x1571daa08>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
## |5  |  -79.8807|  21.8374|    2244|    1828|     945|     249|        1|
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |12 |  -80.1220|  21.9301|    2898|    1033|    1231|     242|        1|
## |13 |  -80.1776|  21.9673|    2914|    1020|    1256|     237|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5219  -0.1766  -0.1206  -0.0718   3.2886  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) 58.109793  29.264591   1.986   0.0471 *
## layer.1     -0.017720   0.007746  -2.288   0.0222 *
## layer.2     -0.016163   0.007745  -2.087   0.0369 *
## layer.3      0.004607   0.007244   0.636   0.5248  
## layer.4      0.005798   0.024889   0.233   0.8158  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 117.46  on 1007  degrees of freedom
## AIC: 127.46
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7518333 
## cor            : 0.1013814 
## max TPR+TNR at : -4.258702 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.705125 
## cor            : 0.0398232 
## max TPR+TNR at : -4.697794 
## 
## 
## Suitability:  
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer 
## values      : 4.949685e-07, 0.9999993  (min, max)
```

![plot of chunk build_glms2](figure/build_glms2-1.png)


If you want a more complicated formula, though (e.g., with interactions or polynomial effects), you'll need to supply that manually.


```r
ahli.glm <- enmtools.glm(species = ahli, env = env, f = pres ~ poly(layer.1, 2) + poly(layer.2, 2) + poly(layer.3, 2) + poly(layer.4, 2), test.prop = 0.2)
```

```
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```r
ahli.glm
```

```
## 
## 
## Formula:  presence ~ poly(layer.1, 2) + poly(layer.2, 2) + poly(layer.3, 
##     2) + poly(layer.4, 2)
## <environment: 0x10b2ce608>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |1  |  -80.0106|  21.8744|    2765|    1235|    1174|     252|        1|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
## |5  |  -79.8807|  21.8374|    2244|    1828|     945|     249|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |12 |  -80.1220|  21.9301|    2898|    1033|    1231|     242|        1|
## |13 |  -80.1776|  21.9673|    2914|    1020|    1256|     237|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.72239  -0.16804  -0.11519  -0.06171   3.15701  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -5.4444     0.7074  -7.697  1.4e-14 ***
## poly(layer.1, 2)1 -61.9525    78.7871  -0.786    0.432    
## poly(layer.1, 2)2 -18.5683    30.3447  -0.612    0.541    
## poly(layer.2, 2)1 -46.5785    81.6012  -0.571    0.568    
## poly(layer.2, 2)2  17.3345    29.3118   0.591    0.554    
## poly(layer.3, 2)1  19.6329    40.1197   0.489    0.625    
## poly(layer.3, 2)2  11.7008    17.1611   0.682    0.495    
## poly(layer.4, 2)1 -42.3658    31.9606  -1.326    0.185    
## poly(layer.4, 2)2 -27.6009    23.0291  -1.199    0.231    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 110.42  on 1003  degrees of freedom
## AIC: 128.42
## 
## Number of Fisher Scoring iterations: 9
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.81025 
## cor            : 0.1143963 
## max TPR+TNR at : -4.615961 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.406125 
## cor            : -0.01038554 
## max TPR+TNR at : -7.407483 
## 
## 
## Suitability:  
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer 
## values      : 2.220446e-16, 0.9999985  (min, max)
```

![plot of chunk build_glms3](figure/build_glms3-1.png)

To check out the marginal response functions, you only need to type


```r
ahli.glm$response.plots
```

```
## $layer.1
```

![plot of chunk response_plots](figure/response_plots-1.png)

```
## 
## $layer.2
```

![plot of chunk response_plots](figure/response_plots-2.png)

```
## 
## $layer.3
```

![plot of chunk response_plots](figure/response_plots-3.png)

```
## 
## $layer.4
```

![plot of chunk response_plots](figure/response_plots-4.png)

You can also visualize your models and data in a 2D environment space using any pair of layers from your environment stack.  These plots hold all non-plotted variables (layer.2 and layer.3 in this case) constant at their mean value across all presence points, then vary the plotted variables between the minimum and maximum values in env.

The suit.plot shows you suitability in environment space as a function of your two variables, with brighter colors representing variable combinations predicted to be more suitable.  The points represent the occurrence points for your species in that environment space.  

The colored raster of the background.plot shows you the density of background points in environment space, while the white points again represent your occurrence points in environment space.


```r
visualize.enm(ahli.glm, env, layers = c("layer.2", "layer.4"))
```

```
## $suit.plot
```

![plot of chunk visualize.enm](figure/visualize.enm-1.png)

```
## 
## $background.plot
```

![plot of chunk visualize.enm](figure/visualize.enm-2.png)

### Bioclim, Domain, and Maxent

The procedure for building Bioclim, Domain, and Maxent models is similar to the procedure for GLMs, with the exception that you do not need to pass a formula to the model function.  Note that running Maxent models requires a bit of extra setup; see dismo documentation for details.


```r
ahli.dm <- enmtools.dm(ahli, env, test.prop = 0.2)
ahli.bc <- enmtools.bc(ahli, env, test.prop = 0.2)
ahli.mx <- enmtools.maxent(ahli, env, test.prop = 0.2)
```


## Metrics: breadth, correlation, and overlap

ENMTools provides a number of metrics for ENMs and for similarities between ENMs.  These include measures of niche breadth, based on Levins(1968).  An important caveat when interpreting these metrics is that they are driven to some (variable) extent by the availability of different combinations of environmental variables.  As such they are more accurately interpreted as a measurment of the smoothness of the geographic distribution of suitability scores than as an estimate of the breadth of the fundamental niche; an orgamism with narrow fundamental niche breadth that nonetheless encompasses a set of environmental conditions that is quite common will have a high breadth when measured using ENMs, while having a low breadth in environment space.


```r
raster.breadth(ahli.glm)
```

```
## $B1
## [1] 0.7645422
## 
## $B2
## [1] 0.02820374
```


ENMTools also provides metrics for measuring similarity between ENMs.  These include Schoener's D (Schoener 1968), I (Warren et al. 2008), and the Spearman rank correlation coefficient between two rasters.  While D and I are commonly used in the ENM literature, they may tend to overestimate similarity between ENMs when many grid cells are of similar values (e.g., when two species prefer different habitat but the region contains a great deal of habitat that is unsuitable for both).  


```r
raster.overlap(ahli.glm, allogus.glm)
```

```
## $D
## [1] 0.2038754
## 
## $I
## [1] 0.3830403
## 
## $rank.cor
## [1] -0.07397363
```


A new feature of the R version of ENMTools is that you can now use these same metrics in the n-dimensional space of all combinations of environmental variables, instead of restricting your measures of model similarity to those sets of conditions that appear in the training region.  This is done by repeatedly drawing Latin hypercube samples from the space of all possible combinations of environmental variables given the min and max of each variable within the training region.  ENMTools continues to draw samples until subsequent iterations differ by less than a specified tolerance value.  Lower tolerance values result in more precise estimates of overlap, but can take much longer to calculate.


```r
env.overlap(ahli.glm, allogus.glm, env, tolerance = .001)
```

```
## $env.D
## [1] 0.09278205
## 
## $env.I
## [1] 0.2786692
## 
## $env.cor
## [1] -0.2990784
```

## Hypothesis testing

### Niche identity or equivalency test

In this example, we will run a niche identity (also called equivalency) test, as in Warren et al. 2008.  This test takes the presence points for a pair of species and randomly reassigns them to each species, then builds ENMs for these randomized occurrences.  By doing this many times, we can estimate the probability distribution for ENM overlap between species under the null hypothesis that the two species' occurrences in the environment are effectively a random draw from the same underlying distribution.  Note that niche evolution is only one of many reasons why two species' realized environmental distributions might cause departures from this null hypothesis.  See Warren et al. 2014 for details.

To run an identity test, we need to decide what type of models we will build, how many replicates we will run, and (in the case of GLM) a model formula to use for empirical models and the Monte Carlo replicates.  The resulting object contains the replicate models, p values, and plots of the results.  Typically idenity tests are run with at least 99 replicates, but we are using a smaller number here for the sake of execution time.

_NOTE:_ In order for models to be comparable, both empirical and pseudoreplicate models for the identity test are conducted with pseudoabsence points pooled for the two species being compared.




```r
id.glm <- identity.test(species.1 = ahli, species.2 = allogus, env = env, type = "glm", nreps = 4)
```


```r
id.glm
```

```
## 
## 
##  
## 
## Identity test ahli vs. allogus
## 
## Identity test p-values:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|    env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|----------:|
## |empirical | 0.2262034| 0.4708885| -0.4872071| 0.0044981| 0.0302807| -0.6169960|
## |rep 1     | 0.8892489| 0.9912822|  0.7989764| 0.7089759| 0.9324822|  0.8025442|
## |rep 2     | 0.6140096| 0.8782533|  0.6981950| 0.5704973| 0.8195712|  0.6342199|
## |rep 3     | 0.7033211| 0.9347752|  0.7921435| 0.6620408| 0.8907446|  0.8061450|
## |rep 4     | 0.7165237| 0.9354195|  0.7580947| 0.6573036| 0.8895321|  0.7771236|
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


### Background or similarity test

The background or similarity test compares the overlap seen between two species' ENMs to the overlap expected by chance if one or both species was effectively choosing habitat at random from within their broad geographic range.  The purpose of this test is to correct for the availability of habitat and ask whether the observed similarity between species or populations is significantly more (or less) than expected given the available set of environments in the regions in which they occur.  

_NOTE:_ In order for models to be comparable, both empirical and pseudoreplicate models for the background test are conducted with pseudoabsence points pooled for the two species being compared.

In Warren et al. 2008, we developed this test in the context of comparing one species' actual occurrence to the random background occurrences of the other species.  This is what we call an "asymmetric" test, and in our case we did the test in both directions with the idea that we might compare the results of A vs. B background to the results of B vs. A background.  This may be informative in some cases, but many people have also found this asymmetry confusing (and indeed it is often difficult to interpret).  For that reason, the background test here can be conducted against a null hypothesis that is generated from "asymmetric" (species.1 vs species.2 background) or "symmetric" (species.1 background vs. species.2 background) comparisons.

Here, for instance, is a Bioclim background test using the classical asymmetric approach:



```r
bg.bc.asym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "bc", nreps = 4, test.type = "asymmetric" )
```


```r
bg.bc.asym
```

```
## 
## 
##  
## 
## Asymmetric background test ahli vs. allogus background
## 
## background test p-values:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|---------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0200192| 0.1070523| 0.0815410|
## |rep 1     | 0.1722278| 0.3767803| 0.2013117| 0.0446688| 0.1887930| 0.1291281|
## |rep 2     | 0.1754337| 0.3817851| 0.2246735| 0.0855174| 0.2454551| 0.1861942|
## |rep 3     | 0.1561754| 0.3477057| 0.1683514| 0.0833172| 0.2334385| 0.1819827|
## |rep 4     | 0.1708847| 0.3646199| 0.2141453| 0.0685576| 0.2296056| 0.1867105|
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


And here is a Domain background test using the symmetric approach:



```r
bg.dm.sym <- background.test(species.1 = ahli, species.2 = allogus, env = env, type = "dm", nreps = 4, test.type = "symmetric" )
```


```r
bg.dm.sym
```

```
## 
## 
##  
## 
## Symmetric background test ahli background vs. allogus background
## 
## background test p-values:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|---------:|---------:|---------:|---------:|
## |empirical | 0.4929334| 0.7052122| 0.2916150| 0.1063426| 0.3092627| 0.2265385|
## |rep 1     | 0.9538946| 0.9946018| 0.8752484| 0.5785714| 0.7782083| 0.6975665|
## |rep 2     | 0.7218166| 0.8819242| 0.5556985| 0.2202422| 0.4559985| 0.4091196|
## |rep 3     | 0.9129506| 0.9836344| 0.6500346| 0.3481877| 0.5849541| 0.5150455|
## |rep 4     | 0.9624327| 0.9986316| 0.7658009| 0.6731729| 0.8489127| 0.7959412|
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


### Rangebreak tests

ENMTools also allows you to perform linear, blob, and ribbon rangebreak tests as developed in Glor and Warren 2011.  The linear and blob tests are two versions of a test that permit one to ask whether the geographic regions occupied by two species are more environmentally different than expected by chance. The ribbon test, meanwhile, is designed to test whether the ranges of two species are divided by a region that is relatively unsuitable to one or both forms. 

For the linear and blob tests, you call them very much like you would the identity and background tests.  Here's a linear one using GLM models:

```r
rbl.glm <- rangebreak.linear(ahli, allogus, env, type = "glm", nreps = 4)
```

```
## 
## Building empirical models...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## 
## Building replicate models...
## 
## Replicate 1 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## 
## Replicate 2 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## 
## Replicate 3 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## 
## Replicate 4 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
```

```r
rbl.glm
```

```
## 
## 
##  
## 
## Linear rangebreak test ahli vs. allogus
## 
## rangebreak test p-values:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.8      0.8      0.8      0.2      0.4      0.4 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|    env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|----------:|
## |empirical | 0.2262034| 0.4708885| -0.4872071| 0.0044151| 0.0300172| -0.6228726|
## |rep 1     | 0.2561058| 0.5621719| -0.7832923| 0.0480899| 0.1469480| -0.7209518|
## |rep 2     | 0.2262034| 0.4708885| -0.4872071| 0.0045067| 0.0295305| -0.6186302|
## |rep 3     | 0.2262034| 0.4708885| -0.4872071| 0.0046422| 0.0302261| -0.6191070|
## |rep 4     | 0.0745355| 0.2444119|  0.8965420| 0.6954968| 0.8620201|  0.9164855|
```

![plot of chunk rangebreak_linear](figure/rangebreak_linear-1.png)![plot of chunk rangebreak_linear](figure/rangebreak_linear-2.png)


And here's a blob test using Bioclim:

```r
rbb.bc <- rangebreak.blob(ahli, allogus, env, type = "bc", nreps = 4)
```

```
## 
## Building empirical models...
## 
## Building replicate models...
## 
## Replicate 1 ...
## 
## Replicate 2 ...
## 
## Replicate 3 ...
## 
## Replicate 4 ...
```

```r
rbb.bc
```

```
## 
## 
##  
## 
## blob rangebreak test ahli vs. allogus
## 
## rangebreak test p-values:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.8      0.8      0.8      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|---------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0203401| 0.1077849| 0.0835770|
## |rep 1     | 0.1328502| 0.3177390| 0.0706201| 0.0244954| 0.1232335| 0.0992183|
## |rep 2     | 0.1328502| 0.3177390| 0.0706201| 0.0218114| 0.1159544| 0.0895872|
## |rep 3     | 0.1328502| 0.3177390| 0.0706201| 0.0230523| 0.1129200| 0.0886949|
## |rep 4     | 0.1812642| 0.3860176| 0.1926002| 0.3716949| 0.4966262| 0.4086966|
```

![plot of chunk rangebreak_blob](figure/rangebreak_blob-1.png)


If you want to access the individual replicates (for instance to see how your ranges are being split up), you can find them in the list named "replicate.models" inside your rangebreak test object.

```r
rbl.glm$replicate.models$ahli.rep.1
```

```
## 
## 
## Formula:  presence ~ layer.1 + layer.2 + layer.3 + layer.4
## <environment: 0x2401862a8>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |19 |  -78.6189|  22.2373|    2317|    1980|    1065|     374|        1|
## |18 |  -78.7774|  22.2241|    2373|    1967|    1067|     375|        1|
## |17 |  -79.2527|  22.2109|    2656|    1683|    1097|     325|        1|
## |23 |  -77.9719|  21.7091|    2394|    1789|     966|     364|        1|
## |25 |  -77.9323|  21.6167|    2384|    1666|    1017|     324|        1|
## |16 |  -79.9972|  21.9792|    2861|    1150|    1194|     259|        1|
## |24 |  -77.9719|  21.5507|    2402|    1708|     992|     325|        1|
## |15 |  -80.0437|  21.9720|    2712|    1285|    1126|     250|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.3761  -0.1295  -0.0895  -0.0544   3.3815  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -28.539784   6.282304  -4.543 5.55e-06 ***
## layer.1       0.002879   0.001410   2.042  0.04116 *  
## layer.2       0.002741   0.001166   2.350  0.01875 *  
## layer.3       0.009986   0.003671   2.720  0.00653 ** 
## layer.4       0.006244   0.004234   1.475  0.14034    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 186.63  on 2015  degrees of freedom
## Residual deviance: 165.44  on 2011  degrees of freedom
## AIC: 175.44
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 16 
## n absences     : 2000 
## AUC            : 0.7971875 
## cor            : 0.09920931 
## max TPR+TNR at : -5.008189 
## 
## 
## Proportion of data wittheld for model fitting:  0
## 
## Model fit (test data):  [1] NA
## 
## 
## Suitability:  
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer 
## values      : 1.012742e-05, 0.1698476  (min, max)
```

![plot of chunk rbl_reps](figure/rbl_reps-1.png)

```r
rbl.glm$replicate.models$allogus.rep.1
```

```
## 
## 
## Formula:  presence ~ layer.1 + layer.2 + layer.3 + layer.4
## <environment: 0x254fe10b0>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |1  |  -80.0106|  21.8744|    2765|    1235|    1174|     252|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |5  |  -79.8807|  21.8374|    2244|    1828|     945|     249|        1|
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |59 |  -74.9877|  20.6528|    2328|    1492|     779|     664|        1|
## |61 |  -74.9481|  20.6264|    2345|    1410|     738|     656|        1|
## |64 |  -74.8161|  20.5867|    2558|    1292|     660|     706|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6810  -0.2423  -0.1375  -0.1078   3.2709  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  9.6239000  2.8022793   3.434 0.000594 ***
## layer.1     -0.0030213  0.0006183  -4.887 1.03e-06 ***
## layer.2     -0.0029991  0.0006374  -4.705 2.54e-06 ***
## layer.3     -0.0028529  0.0009979  -2.859 0.004252 ** 
## layer.4      0.0017404  0.0011818   1.473 0.140861    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 629.56  on 3064  degrees of freedom
## Residual deviance: 568.95  on 3060  degrees of freedom
## AIC: 578.95
## 
## Number of Fisher Scoring iterations: 7
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 65 
## n absences     : 3000 
## AUC            : 0.7674795 
## cor            : 0.1411887 
## max TPR+TNR at : -3.55335 
## 
## 
## Proportion of data wittheld for model fitting:  0
## 
## Model fit (test data):  [1] NA
## 
## 
## Suitability:  
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer 
## values      : 0.001140772, 0.2719248  (min, max)
```

![plot of chunk rbl_reps](figure/rbl_reps-2.png)


For the ribbon rangebreak test, you will need one extra thing; a third enmtools.species object representing the occurrence points (for one or both species) that fall within the ribbon of putatively unsuitable habitat.  In the case of these two anoles we don't have such a ribbon, so we'll just simulate one based on some random points.


```r
ribbon <- enmtools.species(species.name = "ribbon")
ribbon$presence.points <- data.frame(Longitude = runif(n = 10, min = -79, max = -78.5),
                                      Latitude = runif(n = 10, min = 21.7, max = 22.1))
plot(env[[1]])
points(ribbon$presence.points, pch = 16)
```

![plot of chunk build_ribbon](figure/build_ribbon-1.png)

```r
ribbon$range <- background.raster.buffer(ribbon$presence.points, 20000, mask = env)
ribbon
```

```
## 
## 
## Range raster: 
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer.1 
## values      : 1, 1  (min, max)
## 
## 
## 
## Presence points (first ten only): 
## 
## | Longitude| Latitude|
## |---------:|--------:|
## | -78.77098| 21.71781|
## | -78.73283| 21.93862|
## | -78.87494| 22.05509|
## | -78.66918| 21.71191|
## | -78.71321| 21.86456|
## | -78.97961| 22.03491|
## | -78.68143| 21.91787|
## | -78.65581| 21.84490|
## | -78.90635| 22.00500|
## | -78.53583| 21.98150|
## 
## 
## Background points not defined.
## 
## Species name:  ribbon
```


Now we'll run a ribbon rangebreak test using GLM models with quadratic effects.  We also need to tell it the width of the ribbons to generate for the replicates.  The units for the width argument are the same units that the presence points are in; e.g., if the points are in decimal degrees you should supply the width of the barrier in decimal degrees. 

```r
rbr.glm <- rangebreak.ribbon(ahli, allogus, ribbon, env, type = "glm", f = pres ~ poly(layer.1, 2) + poly(layer.2, 2) + poly(layer.3, 2) + poly(layer.4, 2), width = 0.5, nreps = 4)
```

```
## 
## 
## No background points provided, drawing background from range raster.
```

```
## Warning in couldBeLonLat(mask): CRS is NA. Assuming it is longitude/
## latitude
```

```
## 
## Building empirical models...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## Adding environmental data to species ribbon 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species outside 
## 	Processing presence points...
## 	Processing background points...
## 
## Building replicate models...
## 
## Replicate 1 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## Adding environmental data to species ribbon 
## 	Processing presence points...
## 	Processing background points...
## Adding environmental data to species outside 
## 	Processing presence points...
## 	Processing background points...
## 
## Replicate 2 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## Adding environmental data to species ribbon 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species outside 
## 	Processing presence points...
## 	Processing background points...
## 
## Replicate 3 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## Adding environmental data to species ribbon 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species outside 
## 	Processing presence points...
## 	Processing background points...
## 
## Replicate 4 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## Adding environmental data to species ribbon 
## 	Processing presence points...
## 	Processing background points...
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Adding environmental data to species outside 
## 	Processing presence points...
## 	Processing background points...
```

```r
rbr.glm
```

```
## 
## 
##  
## 
## ribbon rangebreak test ahli vs. allogus
## 
## rangebreak test p-values...
## 
## Species 1 vs. Species 2:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.8      0.8      0.6      0.6      0.6      0.6 
## 
## Species 1 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.6      0.6      0.8      1.0      0.8      0.8 
## 
## Species 2 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.8 
## 
## Outside vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.4 
## 
## 
## Replicates:
## 
## Species 1 vs. Species 2:
##                    D         I   rank.cor       env.D      env.I
## empirical 0.25812325 0.4850087  0.1101457 0.018352266 0.08321738
## rep 1     0.11604900 0.3144694 -0.1973876 0.002079593 0.01432123
## rep 2     0.30300794 0.5617413 -0.1955582 0.014565396 0.07902943
## rep 3     0.06552279 0.1942202  0.3656895 0.226111044 0.38525425
## rep 4     0.05749832 0.2185577  0.7346515 0.460711680 0.70873904
##              env.cor
## empirical -0.3576761
## rep 1     -0.8539261
## rep 2     -0.5745025
## rep 3     -0.1026258
## rep 4      0.3669066
## 
## Species 1 vs. Ribbon:
##                     D          I  rank.cor       env.D      env.I
## empirical 0.201689112 0.39765999 0.5398191 0.536223178 0.61750090
## rep 1     0.252234126 0.52524064 0.6595674 0.409317604 0.67346904
## rep 2     0.317570462 0.59009576 0.3647137 0.005233068 0.02067609
## rep 3     0.007924322 0.03572731 0.1815511 0.010058812 0.03342443
## rep 4     0.014391664 0.07271534 0.4493336 0.078829615 0.12629314
##              env.cor
## empirical  0.6284557
## rep 1      0.6872295
## rep 2      0.4587112
## rep 3     -0.2890500
## rep 4     -0.5328970
## 
## Species 2 vs. Ribbon:
##                    D         I     rank.cor      env.D      env.I
## empirical 0.08323038 0.2330318 -0.462622268 0.01000906 0.06817591
## rep 1     0.44818725 0.7500708 -0.129164112 0.03997167 0.12254914
## rep 2     0.46488847 0.7264440  0.359990590 0.03575613 0.13355550
## rep 3     0.46084149 0.7504407 -0.005122753 0.01662889 0.07761425
## rep 4     0.17155251 0.3669507  0.812708514 0.07810689 0.23208291
##               env.cor
## empirical -0.30164229
## rep 1     -0.63855766
## rep 2     -0.55527281
## rep 3      0.08971172
## rep 4     -0.33068381
## 
## Outside vs. Ribbon:
##                   D         I    rank.cor      env.D      env.I    env.cor
## empirical 0.1061292 0.2746159 -0.48356508 0.00422193 0.04716013 -0.4985676
## rep 1     0.6060433 0.8719935  0.70973077 0.08944397 0.22046640 -0.4750522
## rep 2     0.4677213 0.7423240  0.29954783 0.07364729 0.22722234 -0.4978151
## rep 3     0.4205555 0.7040570  0.06440695 0.01550866 0.07713722 -0.1587531
## rep 4     0.1665376 0.3670362  0.67524730 0.04911121 0.16379744 -0.6066322
```

![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-1.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-2.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-3.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-4.png)


Note that the output table here has slope, intercept, and intercept offset.  

```r
rbr.glm$lines.df
```

```
##        slope    intercept    offset
## 1 -0.2634490    0.4917226 0.2585301
## 2 -2.0647724 -138.9672854 0.5735463
## 3 -0.5958876  -24.7530816 0.2910200
## 4 -1.6434420 -104.8878639 0.4809432
```
The intercept denotes the intercept corresponding to the CENTER of each ribbon.  To get the lines denoting the edges of the ribbons (for example if you want to plot the ribbons on a map), you add and substract the offset.  In other words, the top edge of the ribbon is given by y = (slope * x) + intercept + offset, while the bottom edge is given by y = (slope * x) + intercept - offset.  


### Literature cited


*Levins, R. 1968. Evolution In Changing Environments. Monographs in Population Biology, volume 2. Princeton University Press, Princeton, New Jersey, USA.*

*Schoener, T. W. 1968. Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49:704- 726.*

*Warren, D.L., R.E. Glor, and M. Turelli.  2008. Environmental niche identity versus conservatism: quantitative approaches to niche evolution.  Evolution 62:2868-2883. doi: 10.1111/j.1558-5646.2008.00482.x*

*Warren, D.L., M. Cardillo, D.F. Rosauer, and D.I. Bolnick. 2014. Mistaking geography for biology: inferring processes from species distributions. Trends in Ecology and Evolution 29 (10), 572-580. doi: 10.1016/j.tree.2014.08.003*
