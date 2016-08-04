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
## | -79.97892| 22.05420|
## | -80.32892| 22.02920|
## | -79.92892| 22.08754|
## | -80.33726| 22.03754|
## | -79.74559| 21.83754|
## | -79.67892| 21.70420|
## | -79.72059| 21.74587|
## | -79.92892| 21.92920|
## | -79.77059| 21.86254|
## | -80.31226| 22.12087|
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
## | -79.31226| 22.31254|
## | -77.18726| 20.05420|
## | -77.94559| 21.28754|
## | -76.21226| 20.03754|
## | -77.55392| 21.07920|
## | -78.69559| 22.23754|
## | -77.10392| 19.89587|
## | -75.05392| 20.01254|
## | -77.95392| 20.99587|
## | -77.33726| 20.93754|
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
ahli.glm
```

```
## 
## 
## Formula:  presence ~ layer.1 + layer.2 + layer.3 + layer.4
## <environment: 0x2d833cbc0>
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
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
## |12 |  -80.1220|  21.9301|    2898|    1033|    1231|     242|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.41500  -0.17791  -0.12781  -0.09856   3.09318  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept)  3.441e+01  2.496e+01   1.379    0.168
## layer.1     -8.580e-03  6.252e-03  -1.373    0.170
## layer.2     -8.319e-03  6.794e-03  -1.225    0.221
## layer.3     -7.615e-05  8.134e-03  -0.009    0.993
## layer.4     -1.759e-02  2.512e-02  -0.700    0.484
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 122.93  on 1007  degrees of freedom
## AIC: 132.93
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.745625 
## cor            : 0.08323185 
## max TPR+TNR at : -4.77558 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.801 
## cor            : 0.07509128 
## max TPR+TNR at : -4.519812 
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
## values      : 1.78801e-08, 0.9977902  (min, max)
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
## <environment: 0x2908cb2d0>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
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
## -0.6367  -0.1779  -0.1152  -0.0651   3.2546  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) 62.302542  29.499311   2.112   0.0347 *
## layer.1     -0.019476   0.007865  -2.476   0.0133 *
## layer.2     -0.016799   0.007768  -2.163   0.0306 *
## layer.3      0.005911   0.006362   0.929   0.3528  
## layer.4      0.004654   0.024018   0.194   0.8464  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 115.78  on 1007  degrees of freedom
## AIC: 125.78
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.790875 
## cor            : 0.1049627 
## max TPR+TNR at : -4.283548 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.59175 
## cor            : 0.02386482 
## max TPR+TNR at : -5.365433 
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
## values      : 2.189607e-07, 0.9999997  (min, max)
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
## <environment: 0x2908d6830>
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
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |12 |  -80.1220|  21.9301|    2898|    1033|    1231|     242|        1|
## |14 |  -80.2148|  21.9394|    2329|    1692|    1018|     269|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.65706  -0.16894  -0.12100  -0.06731   3.06085  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -5.2214     0.5805  -8.995   <2e-16 ***
## poly(layer.1, 2)1 -94.1439    62.7216  -1.501    0.133    
## poly(layer.1, 2)2  -6.0598    30.2442  -0.200    0.841    
## poly(layer.2, 2)1 -62.3633    69.5668  -0.896    0.370    
## poly(layer.2, 2)2 -16.5671    32.8127  -0.505    0.614    
## poly(layer.3, 2)1  39.5307    41.9102   0.943    0.346    
## poly(layer.3, 2)2  26.0294    18.5412   1.404    0.160    
## poly(layer.4, 2)1 -21.8137    24.0338  -0.908    0.364    
## poly(layer.4, 2)2 -25.7782    19.9908  -1.290    0.197    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 114.93  on 1003  degrees of freedom
## AIC: 132.93
## 
## Number of Fisher Scoring iterations: 9
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7842917 
## cor            : 0.1056662 
## max TPR+TNR at : -4.675216 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.60575 
## cor            : 0.02658734 
## max TPR+TNR at : -5.781522 
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
## values      : 2.220446e-16, 0.9999992  (min, max)
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

You can also visualize your models and data in a 2D environment space using any pair of layers from your environment stack.  These plots hold all non-plotted variables (layer.1 and layer.3 in this case) constant at their mean value across all presence points, then vary the plotted variables between the minimum and maximum values in env.

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

### GAM, Bioclim, Domain, and Maxent

The procedure for building Bioclim, Domain, and Maxent models is similar to the procedure for GLMs, with the exception that you do not need to pass a formula to the model function.  Note that running Maxent models requires a bit of extra setup; see dismo documentation for details.


```r
ahli.gam <- enmtools.gam(ahli, env, test.prop = 0.2)
ahli.dm <- enmtools.dm(ahli, env, test.prop = 0.2)
ahli.bc <- enmtools.bc(ahli, env, test.prop = 0.2)
ahli.mx <- enmtools.maxent(ahli, env, test.prop = 0.2)
```


## Metrics: breadth, correlation, and overlap

ENMTools provides a number of metrics for ENMs and for similarities between ENMs.  These include measures of niche breadth, based on Levins(1968).  An important caveat when interpreting these metrics is that they are driven to some (variable) extent by the availability of different combinations of environmental variables.  As such they are more accurately interpreted as a measurment of the smoothness of the geographic distribution of suitability scores than as an estimate of the breadth of the fundamental niche; an organism with narrow fundamental niche breadth that nonetheless encompasses a set of environmental conditions that is quite common will have a high breadth when measured using ENMs, while having a low breadth in environment space.


```r
raster.breadth(ahli.glm)
```

```
## $B1
## [1] 0.8267062
## 
## $B2
## [1] 0.09645683
```


ENMTools also provides metrics for measuring similarity between ENMs.  These include Schoener's D (Schoener 1968), I (Warren et al. 2008), and the Spearman rank correlation coefficient between two rasters.  While D and I are commonly used in the ENM literature, they may tend to overestimate similarity between ENMs when many grid cells are of similar values (e.g., when two species prefer different habitat but the region contains a great deal of habitat that is unsuitable for both).  


```r
raster.overlap(ahli.glm, allogus.glm)
```

```
## $D
## [1] 0.2142527
## 
## $I
## [1] 0.4608975
## 
## $rank.cor
## [1] 0.04578893
```


A new feature of the R version of ENMTools is that you can now use these same metrics in the n-dimensional space of all combinations of environmental variables, instead of restricting your measures of model similarity to those sets of conditions that appear in the training region.  This is done by repeatedly drawing Latin hypercube samples from the space of all possible combinations of environmental variables given the min and max of each variable within the training region.  ENMTools continues to draw samples until subsequent iterations differ by less than a specified tolerance value.  Lower tolerance values result in more precise estimates of overlap, but can take much longer to calculate.


```r
env.overlap(ahli.glm, allogus.glm, env, tolerance = .001)
```

```
## $env.D
## [1] 0.1035241
## 
## $env.I
## [1] 0.2893945
## 
## $env.cor
## [1] -0.316955
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
## |empirical | 0.2510115| 0.5054745| -0.5399230| 0.0054655| 0.0343047| -0.6567199|
## |rep 1     | 0.7898390| 0.9689705|  0.9141686| 0.7564208| 0.9471171|  0.9272695|
## |rep 2     | 0.7355077| 0.9514230|  0.9565340| 0.7647018| 0.9471666|  0.9447897|
## |rep 3     | 0.7491115| 0.9509117|  0.3354544| 0.3526102| 0.6545552|  0.1472238|
## |rep 4     | 0.7161985| 0.9395274|  0.6452556| 0.5173588| 0.8062828|  0.5773150|
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
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0236120| 0.1231041| 0.0949290|
## |rep 1     | 0.1730516| 0.3733035| 0.1975328| 0.0394479| 0.1803084| 0.1225563|
## |rep 2     | 0.2205881| 0.4248851| 0.3416814| 0.0966895| 0.2565000| 0.1442687|
## |rep 3     | 0.1598674| 0.3535200| 0.1874877| 0.0733675| 0.2179979| 0.1910101|
## |rep 4     | 0.1501424| 0.3369599| 0.1434171| 0.0503523| 0.1905436| 0.1456811|
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
## |empirical | 0.4929334| 0.7052122| 0.2916150| 0.1021708| 0.3028006| 0.2133162|
## |rep 1     | 0.8749601| 0.9778469| 0.5497781| 0.2341620| 0.4690428| 0.4260698|
## |rep 2     | 0.9188577| 0.9844278| 0.9447951| 0.3733732| 0.6049064| 0.4910687|
## |rep 3     | 0.9823621| 0.9996219| 0.9240443| 0.8372062| 0.9492429| 0.9027849|
## |rep 4     | 0.8875470| 0.9740548| 0.6062741| 0.3174260| 0.5572430| 0.4895902|
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
## Adding environmental data to species allogus 
## 	Processing presence points...
## 	Processing background points...
## 
## Replicate 4 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
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
##      0.6      0.6      0.8      0.4      0.4      0.6 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|    env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|----------:|
## |empirical | 0.2510115| 0.5054745| -0.5399230| 0.0057385| 0.0346831| -0.6531713|
## |rep 1     | 0.2812367| 0.5934983| -0.8478177| 0.0151075| 0.0619896| -0.7476931|
## |rep 2     | 0.2510115| 0.5054745| -0.5399230| 0.0055414| 0.0340545| -0.6541494|
## |rep 3     | 0.1618797| 0.3819943|  0.7852141| 0.6708712| 0.8489328|  0.8403343|
## |rep 4     | 0.2796547| 0.5664237| -0.6600540| 0.1359935| 0.3155255| -0.5589103|
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
##      1.0      0.8      0.8      0.6      0.6      0.6 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390|  0.0706201| 0.0250656| 0.1258614| 0.1004012|
## |rep 1     | 0.0327940| 0.1544608| -0.0172882| 0.2084441| 0.2978203| 0.2473155|
## |rep 2     | 0.1222911| 0.3345075|  0.1829563| 0.1997239| 0.4296979| 0.3385917|
## |rep 3     | 0.1328502| 0.3177390|  0.0706201| 0.0227099| 0.1156631| 0.0898299|
## |rep 4     | 0.1328502| 0.3177390|  0.0706201| 0.0237241| 0.1172635| 0.0919682|
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
## <environment: 0x2ab6dab58>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |19 |  -78.6189|  22.2373|    2317|    1980|    1065|     374|        1|
## |18 |  -78.7774|  22.2241|    2373|    1967|    1067|     375|        1|
## |17 |  -79.2527|  22.2109|    2656|    1683|    1097|     325|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |16 |  -79.9972|  21.9792|    2861|    1150|    1194|     259|        1|
## |15 |  -80.0437|  21.9720|    2712|    1285|    1126|     250|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
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
## -0.4873  -0.1426  -0.0970  -0.0497   3.4084  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -27.716806   7.759751  -3.572 0.000354 ***
## layer.1       0.002767   0.001592   1.738 0.082244 .  
## layer.2       0.003990   0.001968   2.028 0.042565 *  
## layer.3       0.011243   0.004758   2.363 0.018132 *  
## layer.4      -0.007206   0.008378  -0.860 0.389731    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 186.63  on 2015  degrees of freedom
## Residual deviance: 166.46  on 2011  degrees of freedom
## AIC: 176.46
## 
## Number of Fisher Scoring iterations: 10
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 16 
## n absences     : 2000 
## AUC            : 0.7711875 
## cor            : 0.0732104 
## max TPR+TNR at : -4.227436 
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
## values      : 1.58032e-10, 0.1127977  (min, max)
```

![plot of chunk rbl_reps](figure/rbl_reps-1.png)

```r
rbl.glm$replicate.models$allogus.rep.1
```

```
## 
## 
## Formula:  presence ~ layer.1 + layer.2 + layer.3 + layer.4
## <environment: 0x2baec6008>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |23 |  -77.9719|  21.7091|    2394|    1789|     966|     364|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |25 |  -77.9323|  21.6167|    2384|    1666|    1017|     324|        1|
## |24 |  -77.9719|  21.5507|    2402|    1708|     992|     325|        1|
## |21 |  -78.0247|  21.1809|    2437|    1684|     877|     264|        1|
## |20 |  -78.1039|  21.1809|    2461|    1669|     846|     252|        1|
## |50 |  -75.6611|  20.9961|    1828|    1777|    1041|     497|        1|
## |27 |  -77.4569|  20.9829|    2417|    1646|     845|     279|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6153  -0.2391  -0.1342  -0.1083   3.1574  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  7.3783288  2.6775265   2.756  0.00586 ** 
## layer.1     -0.0028977  0.0005987  -4.840 1.30e-06 ***
## layer.2     -0.0026045  0.0006081  -4.283 1.84e-05 ***
## layer.3     -0.0018787  0.0009681  -1.941  0.05231 .  
## layer.4      0.0028989  0.0011658   2.487  0.01290 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 629.56  on 3064  degrees of freedom
## Residual deviance: 569.98  on 3060  degrees of freedom
## AIC: 579.98
## 
## Number of Fisher Scoring iterations: 7
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 65 
## n absences     : 3000 
## AUC            : 0.7643256 
## cor            : 0.1415062 
## max TPR+TNR at : -3.931098 
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
## values      : 0.00152408, 0.1951718  (min, max)
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
## | -78.74513| 22.03980|
## | -78.99873| 21.94821|
## | -78.68825| 21.71272|
## | -78.87588| 21.76518|
## | -78.63736| 21.87580|
## | -78.84597| 21.88548|
## | -78.69645| 21.80224|
## | -78.84373| 21.89958|
## | -78.76201| 21.99562|
## | -78.97177| 21.93413|
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
## 
## Replicate 4 ...
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
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
##      1.0      0.6      0.6      0.4      0.4      0.8 
## 
## Species 1 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.6      0.6      0.4      0.2      0.2      0.8 
## 
## Species 2 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.4 
## 
## Outside vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.6 
## 
## 
## Replicates:
## 
## Species 1 vs. Species 2:
##                    D         I    rank.cor      env.D      env.I
## empirical 0.24903323 0.4775608  0.08768137 0.01787900 0.08648792
## rep 1     0.22708436 0.4850290  0.75232652 0.41146360 0.69345209
## rep 2     0.22377199 0.4661292 -0.31592605 0.01418302 0.07922539
## rep 3     0.05109587 0.1875988  0.24793008 0.13126735 0.27161653
## rep 4     0.23755356 0.5300586 -0.78975818 0.08623897 0.22575192
##              env.cor
## empirical -0.3327538
## rep 1      0.4677690
## rep 2     -0.4542478
## rep 3     -0.3784471
## rep 4     -0.4659210
## 
## Species 1 vs. Ribbon:
##                    D          I    rank.cor        env.D       env.I
## empirical 0.10346651 0.23241334  0.08984116 0.0007993843 0.008971796
## rep 1     0.03423873 0.11361692  0.34241995 0.0152973674 0.105354959
## rep 2     0.42378356 0.70485315  0.64765132 0.0485579326 0.147354576
## rep 3     0.01078654 0.04920766  0.19477770 0.0101132104 0.035029428
## rep 4     0.31313708 0.55154641 -0.07551641 0.0414964480 0.086561251
##               env.cor
## empirical  0.14856191
## rep 1      0.05065368
## rep 2      0.74662717
## rep 3     -0.22245497
## rep 4     -0.31702855
## 
## Species 2 vs. Ribbon:
##                    D         I     rank.cor        env.D       env.I
## empirical 0.05866651 0.1910202 -0.770907555 0.0001729014 0.004375852
## rep 1     0.09375871 0.2710504  0.315196977 0.0027596747 0.032824510
## rep 2     0.38742466 0.6440959 -0.074332712 0.0138685124 0.054751523
## rep 3     0.47327329 0.7560900  0.001693365 0.0281496458 0.117356894
## rep 4     0.28003785 0.5700282  0.065595491 0.0891437269 0.231846326
##               env.cor
## empirical -0.12575202
## rep 1      0.04579372
## rep 2     -0.64222758
## rep 3      0.23356557
## rep 4      0.06664871
## 
## Outside vs. Ribbon:
##                    D         I    rank.cor        env.D       env.I
## empirical 0.07658075 0.2265256 -0.79878223 0.0002296288 0.006247992
## rep 1     0.08691426 0.2575740  0.23868669 0.0131663856 0.095346864
## rep 2     0.46387091 0.7127093  0.10825104 0.0229465126 0.075102926
## rep 3     0.44714665 0.7225964  0.10640236 0.0157411461 0.076561249
## rep 4     0.33514529 0.6249116  0.01497017 0.0422001698 0.160974023
##               env.cor
## empirical -0.15339790
## rep 1      0.04314764
## rep 2     -0.64865423
## rep 3     -0.04857705
## rep 4     -0.30470525
```

![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-1.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-2.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-3.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-4.png)


Note that the output table here has slope, intercept, and intercept offset.  

```r
rbr.glm$lines.df
```

```
##        slope intercept    offset
## 1  1.3289224 123.01535 0.4157850
## 2 -0.5328649 -20.21066 0.2832782
## 3 -0.5921045 -24.46515 0.2905370
## 4 -0.1259796  11.34823 0.2519760
```
The intercept denotes the intercept corresponding to the CENTER of each ribbon.  To get the lines denoting the edges of the ribbons (for example if you want to plot the ribbons on a map), you add and substract the offset.  In other words, the top edge of the ribbon is given by y = (slope * x) + intercept + offset, while the bottom edge is given by y = (slope * x) + intercept - offset.  


### Literature cited


*Levins, R. 1968. Evolution In Changing Environments. Monographs in Population Biology, volume 2. Princeton University Press, Princeton, New Jersey, USA.*

*Schoener, T. W. 1968. Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49:704- 726.*

*Warren, D.L., R.E. Glor, and M. Turelli.  2008. Environmental niche identity versus conservatism: quantitative approaches to niche evolution.  Evolution 62:2868-2883. doi: 10.1111/j.1558-5646.2008.00482.x*

*Warren, D.L., M. Cardillo, D.F. Rosauer, and D.I. Bolnick. 2014. Mistaking geography for biology: inferring processes from species distributions. Trends in Ecology and Evolution 29 (10), 572-580. doi: 10.1016/j.tree.2014.08.003*
