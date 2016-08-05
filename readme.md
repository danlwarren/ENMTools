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
## | -80.32892| 22.16254|
## | -79.97059| 22.05420|
## | -80.43726| 22.11254|
## | -79.97892| 22.13754|
## | -79.88726| 21.99587|
## | -79.77059| 21.96254|
## | -80.19559| 21.91254|
## | -80.02059| 22.05420|
## | -80.18726| 22.02920|
## | -79.81226| 21.98754|
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
## | -76.53726| 20.10420|
## | -78.12892| 21.22087|
## | -75.17892| 20.26254|
## | -75.87059| 20.62920|
## | -78.44559| 22.24587|
## | -75.82892| 20.65420|
## | -75.98726| 20.20420|
## | -77.78726| 21.73754|
## | -79.08726| 22.23754|
## | -77.41226| 20.92920|
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
allogus.glm <- enmtools.glm(species = allogus, env = env, f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, test.prop = 0.2)
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
## <environment: 0x126cfba00>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
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
## -0.5641  -0.1765  -0.1026  -0.0646   3.2132  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  7.255e+01  3.063e+01   2.368   0.0179 *
## layer.1     -1.863e-02  7.880e-03  -2.364   0.0181 *
## layer.2     -1.877e-02  8.151e-03  -2.303   0.0213 *
## layer.3     -1.676e-03  7.994e-03  -0.210   0.8339  
## layer.4      4.345e-06  2.518e-02   0.000   0.9999  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 115.23  on 1007  degrees of freedom
## AIC: 125.23
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.8052917 
## cor            : 0.1115561 
## max TPR+TNR at : -4.309774 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.64525 
## cor            : 0.02951144 
## max TPR+TNR at : -4.406271 
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
## values      : 9.119923e-08, 0.9999996  (min, max)
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
## <environment: 0x126d6c200>
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
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
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
## -0.4961  -0.1873  -0.1188  -0.0754   3.1778  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) 60.166862  28.426562   2.117   0.0343 *
## layer.1     -0.017340   0.007425  -2.335   0.0195 *
## layer.2     -0.015547   0.007528  -2.065   0.0389 *
## layer.3      0.002409   0.006569   0.367   0.7139  
## layer.4     -0.000335   0.024024  -0.014   0.9889  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 119.08  on 1007  degrees of freedom
## AIC: 129.08
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.75625 
## cor            : 0.09375249 
## max TPR+TNR at : -4.23679 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.728875 
## cor            : 0.04689053 
## max TPR+TNR at : -4.534152 
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
## values      : 4.36546e-07, 0.9999972  (min, max)
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
## <environment: 0x1028ae9f0>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |1  |  -80.0106|  21.8744|    2765|    1235|    1174|     252|        1|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |5  |  -79.8807|  21.8374|    2244|    1828|     945|     249|        1|
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
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
## -0.46128  -0.18181  -0.11657  -0.07633   3.02286  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -4.9603     0.4503 -11.015   <2e-16 ***
## poly(layer.1, 2)1  -99.1816    58.4062  -1.698   0.0895 .  
## poly(layer.1, 2)2   12.0899    28.5497   0.423   0.6720    
## poly(layer.2, 2)1 -125.0058    69.3761  -1.802   0.0716 .  
## poly(layer.2, 2)2  -11.5871    32.4988  -0.357   0.7214    
## poly(layer.3, 2)1  -18.7544    43.8061  -0.428   0.6686    
## poly(layer.3, 2)2    9.3931    17.5907   0.534   0.5934    
## poly(layer.4, 2)1    3.8132    15.0308   0.254   0.7997    
## poly(layer.4, 2)2   -8.4261    13.0550  -0.645   0.5186    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 119.23  on 1003  degrees of freedom
## AIC: 137.23
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7816667 
## cor            : 0.09959862 
## max TPR+TNR at : -4.558508 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.772875 
## cor            : 0.06049041 
## max TPR+TNR at : -4.973974 
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
## values      : 2.220446e-16, 0.9998847  (min, max)
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
## [1] 0.8667353
## 
## $B2
## [1] 0.1821073
```


ENMTools also provides metrics for measuring similarity between ENMs.  These include Schoener's D (Schoener 1968), I (Warren et al. 2008), and the Spearman rank correlation coefficient between two rasters.  While D and I are commonly used in the ENM literature, they may tend to overestimate similarity between ENMs when many grid cells are of similar values (e.g., when two species prefer different habitat but the region contains a great deal of habitat that is unsuitable for both).  


```r
raster.overlap(ahli.glm, allogus.glm)
```

```
## $D
## [1] 0.3020464
## 
## $I
## [1] 0.5745882
## 
## $rank.cor
## [1] 0.4459607
```


A new feature of the R version of ENMTools is that you can now use these same metrics in the n-dimensional space of all combinations of environmental variables, instead of restricting your measures of model similarity to those sets of conditions that appear in the training region.  This is done by repeatedly drawing Latin hypercube samples from the space of all possible combinations of environmental variables given the min and max of each variable within the training region.  ENMTools continues to draw samples until subsequent iterations differ by less than a specified tolerance value.  Lower tolerance values result in more precise estimates of overlap, but can take much longer to calculate.


```r
env.overlap(ahli.glm, allogus.glm, env, tolerance = .001)
```

```
## $env.D
## [1] 0.2526322
## 
## $env.I
## [1] 0.4690892
## 
## $env.cor
## [1] -0.05584625
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
## |empirical | 0.2421897| 0.4912788| -0.4481580| 0.0050797| 0.0313259| -0.6040165|
## |rep 1     | 0.7699621| 0.9591744|  0.1996044| 0.4565387| 0.7564844|  0.3263174|
## |rep 2     | 0.7768911| 0.9616524|  0.8075279| 0.6209186| 0.8853587|  0.7916487|
## |rep 3     | 0.8641407| 0.9836735|  0.8644880| 0.8195573| 0.9712081|  0.9178691|
## |rep 4     | 0.7607513| 0.9581997|  0.7794379| 0.6317426| 0.8746227|  0.7414764|
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
##      0.2      0.4      0.4      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|---------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0191535| 0.1049789| 0.0799434|
## |rep 1     | 0.1516056| 0.3344584| 0.1246645| 0.0703324| 0.2252455| 0.2090850|
## |rep 2     | 0.1351171| 0.3134528| 0.0628380| 0.0500380| 0.1608283| 0.1454528|
## |rep 3     | 0.1496238| 0.3463450| 0.1635619| 0.0805077| 0.2605317| 0.2226771|
## |rep 4     | 0.1716499| 0.3671396| 0.2024887| 0.0696645| 0.2016786| 0.1408024|
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
## |empirical | 0.4929334| 0.7052122| 0.2916150| 0.1044532| 0.3070321| 0.2211176|
## |rep 1     | 0.7425946| 0.8968979| 0.5250392| 0.3852931| 0.6057366| 0.4812629|
## |rep 2     | 0.9565011| 0.9961401| 0.8583812| 0.6294749| 0.8173473| 0.7446815|
## |rep 3     | 0.8489744| 0.9551823| 0.6463534| 0.4240551| 0.6485303| 0.5423923|
## |rep 4     | 0.9502609| 0.9947096| 0.8405960| 0.6999417| 0.8652437| 0.7924397|
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


### Ecospat tests

Using enmtools.species objects also provides a simplified interface to the niche equivalency and similarity tests (or identity and background tests, respectively) that were developed by Broennimann et al. (2012).  These tests do not rely on ENMs, instead using kernel density smoothing to estimate density of the species in environment space.  Ecospat also uses the density of the available environment to correct for availability when measuring overlaps, so that overlaps are not strictly driven by availability of combinations of environmental variables.  

These tests only work with two environmental axes, so they are often done with the top two PC axes of a set of environments.  In our case we'll just pick a couple of environmental layers, though (layer.1 and layer.2).  Here's an equivalency/identity test:


```r
esp.id <- enmtools.ecospat.id(ahli, allogus, env[[c("layer.1", "layer.2")]])
```

```
## ===========================================================================
```

```r
esp.id
```

```
## 
## 
##  
## 
## Ecospat identity test ahli vs. allogus
## 
## |Species | Longitude| Latitude| layer.1| layer.2|
## |:-------|---------:|--------:|-------:|-------:|
## |ahli    |  -80.0106|  21.8744|    2765|    1235|
## |ahli    |  -79.9086|  21.8095|    2289|    1732|
## |ahli    |  -79.8065|  21.7631|    2158|    1870|
## |ahli    |  -79.8251|  21.8095|    2207|    1877|
## |ahli    |  -79.8807|  21.8374|    2244|    1828|
## |ahli    |  -79.9550|  21.8374|    2250|    1766|
## 
## 
## |Species | Longitude| Latitude| layer.1| layer.2|
## |:-------|---------:|--------:|-------:|-------:|
## |ahli.bg | -80.32892| 22.16254|    2337|    1804|
## |ahli.bg | -79.97059| 22.05420|    2822|    1256|
## |ahli.bg | -80.43726| 22.11254|    2243|    1867|
## |ahli.bg | -79.97892| 22.13754|    2544|    1664|
## |ahli.bg | -79.88726| 21.99587|    2564|    1562|
## |ahli.bg | -79.77059| 21.96254|    2451|    1766|
## 
## 
## |Species    | Longitude| Latitude| layer.1| layer.2|
## |:----------|---------:|--------:|-------:|-------:|
## |allogus.bg |  -79.2527|  22.2109|    2656|    1683|
## |allogus.bg |  -78.7774|  22.2241|    2373|    1967|
## |allogus.bg |  -78.6189|  22.2373|    2317|    1980|
## |allogus.bg |  -78.1039|  21.1809|    2461|    1669|
## |allogus.bg |  -78.0247|  21.1809|    2437|    1684|
## |allogus.bg |  -77.9983|  20.9301|    2315|    1746|
## 
## 
## |Species | Longitude| Latitude| layer.1| layer.2|
## |:-------|---------:|--------:|-------:|-------:|
## |allogus | -76.53726| 20.10420|    2356|    1014|
## |allogus | -78.12892| 21.22087|    2514|    1659|
## |allogus | -75.17892| 20.26254|    2104|    1465|
## |allogus | -75.87059| 20.62920|    2098|    1787|
## |allogus | -78.44559| 22.24587|    2200|    2028|
## |allogus | -75.82892| 20.65420|    2103|    1774|
## 
## 
## |Species    | Longitude| Latitude| layer.1| layer.2|
## |:----------|---------:|--------:|-------:|-------:|
## |background | -80.91226| 23.27087|    2495|    1993|
## |background | -80.90392| 23.27087|    2504|    2000|
## |background | -80.93726| 23.26254|    2500|    1993|
## |background | -80.92892| 23.26254|    2500|    1993|
## |background | -80.92059| 23.26254|    2509|    2002|
## |background | -80.91226| 23.26254|    2504|    2000|
## 
## 
## ecospat.id test p-values:
##    D    I 
## 0.02 0.02
```

![plot of chunk ecospat_identity](figure/ecospat_identity-1.png)![plot of chunk ecospat_identity](figure/ecospat_identity-2.png)

```
## TableGrob (3 x 2) "arrange": 6 grobs
##   z     cells    name           grob
## 1 1 (1-1,1-1) arrange gtable[layout]
## 2 2 (1-1,2-2) arrange gtable[layout]
## 3 3 (2-2,1-1) arrange gtable[layout]
## 4 4 (2-2,2-2) arrange gtable[layout]
## 5 5 (3-3,1-1) arrange gtable[layout]
## 6 6 (3-3,2-2) arrange gtable[layout]
```

And here's a symmetric background test.  The difference between symmetric and asymmetric for these tests is the same as for the background tests presented above.


```r
esp.bg.sym <- enmtools.ecospat.bg(ahli, allogus, env[[c("layer.1", "layer.3")]], test.type = "symmetric")
```

```
## ===========================================================================
```

```r
esp.bg.sym
```

```
## 
## 
##  
## 
## Ecospat background test symmetric ahli vs. allogus
## 
## |Species | Longitude| Latitude| layer.1| layer.3|
## |:-------|---------:|--------:|-------:|-------:|
## |ahli    |  -80.0106|  21.8744|    2765|    1174|
## |ahli    |  -79.9086|  21.8095|    2289|     957|
## |ahli    |  -79.8065|  21.7631|    2158|     983|
## |ahli    |  -79.8251|  21.8095|    2207|     967|
## |ahli    |  -79.8807|  21.8374|    2244|     945|
## |ahli    |  -79.9550|  21.8374|    2250|     919|
## 
## 
## |Species | Longitude| Latitude| layer.1| layer.3|
## |:-------|---------:|--------:|-------:|-------:|
## |ahli.bg | -80.32892| 22.16254|    2337|     995|
## |ahli.bg | -79.97059| 22.05420|    2822|    1170|
## |ahli.bg | -80.43726| 22.11254|    2243|     955|
## |ahli.bg | -79.97892| 22.13754|    2544|     982|
## |ahli.bg | -79.88726| 21.99587|    2564|    1030|
## |ahli.bg | -79.77059| 21.96254|    2451|     991|
## 
## 
## |Species    | Longitude| Latitude| layer.1| layer.3|
## |:----------|---------:|--------:|-------:|-------:|
## |allogus.bg |  -79.2527|  22.2109|    2656|    1097|
## |allogus.bg |  -78.7774|  22.2241|    2373|    1067|
## |allogus.bg |  -78.6189|  22.2373|    2317|    1065|
## |allogus.bg |  -78.1039|  21.1809|    2461|     846|
## |allogus.bg |  -78.0247|  21.1809|    2437|     877|
## |allogus.bg |  -77.9983|  20.9301|    2315|     907|
## 
## 
## |Species | Longitude| Latitude| layer.1| layer.3|
## |:-------|---------:|--------:|-------:|-------:|
## |allogus | -76.53726| 20.10420|    2356|    1153|
## |allogus | -78.12892| 21.22087|    2514|     850|
## |allogus | -75.17892| 20.26254|    2104|    1033|
## |allogus | -75.87059| 20.62920|    2098|     968|
## |allogus | -78.44559| 22.24587|    2200|    1120|
## |allogus | -75.82892| 20.65420|    2103|     944|
## 
## 
## |Species    | Longitude| Latitude| layer.1| layer.3|
## |:----------|---------:|--------:|-------:|-------:|
## |background | -80.91226| 23.27087|    2495|    1075|
## |background | -80.90392| 23.27087|    2504|    1082|
## |background | -80.93726| 23.26254|    2500|    1077|
## |background | -80.92892| 23.26254|    2500|    1077|
## |background | -80.92059| 23.26254|    2509|    1082|
## |background | -80.91226| 23.26254|    2504|    1080|
## 
## 
## ecospat.bg test p-values:
##    D    I 
## 0.48 0.48
```

![plot of chunk ecospat_background](figure/ecospat_background-1.png)![plot of chunk ecospat_background](figure/ecospat_background-2.png)

```
## TableGrob (3 x 2) "arrange": 6 grobs
##   z     cells    name           grob
## 1 1 (1-1,1-1) arrange gtable[layout]
## 2 2 (1-1,2-2) arrange gtable[layout]
## 3 3 (2-2,1-1) arrange gtable[layout]
## 4 4 (2-2,2-2) arrange gtable[layout]
## 5 5 (3-3,1-1) arrange gtable[layout]
## 6 6 (3-3,2-2) arrange gtable[layout]
```


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
##      0.8      0.6      0.8      0.4      0.2      0.8 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|    env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|----------:|
## |empirical | 0.2421897| 0.4912788| -0.4481580| 0.0049309| 0.0301009| -0.5994753|
## |rep 1     | 0.2666887| 0.5747290| -0.8932739| 0.0121569| 0.0507923| -0.8109927|
## |rep 2     | 0.2260824| 0.5347762| -0.8882999| 0.0307884| 0.1074834| -0.8108706|
## |rep 3     | 0.0797506| 0.2547149|  0.8787676| 0.7007963| 0.8673111|  0.9102556|
## |rep 4     | 0.2421897| 0.4912788| -0.4481580| 0.0048855| 0.0304383| -0.6000014|
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
##      0.8      0.8      0.8      0.2      0.4      0.6 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|---------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0214908| 0.1160630| 0.0928442|
## |rep 1     | 0.1328502| 0.3177390| 0.0706201| 0.0221415| 0.1115121| 0.0852177|
## |rep 2     | 0.5187961| 0.7164635| 0.2365479| 0.0940953| 0.2739076| 0.1777219|
## |rep 3     | 0.0305200| 0.1470108| 0.0164929| 0.2044958| 0.3065521| 0.2511591|
## |rep 4     | 0.1328502| 0.3177390| 0.0706201| 0.0228182| 0.1172822| 0.0869061|
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
## <environment: 0x102a2b578>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |13 |  -80.1776|  21.9673|    2914|    1020|    1256|     237|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
## |14 |  -80.2148|  21.9394|    2329|    1692|    1018|     269|        1|
## |15 |  -80.0437|  21.9720|    2712|    1285|    1126|     250|        1|
## |17 |  -79.2527|  22.2109|    2656|    1683|    1097|     325|        1|
## |16 |  -79.9972|  21.9792|    2861|    1150|    1194|     259|        1|
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
## -0.4772  -0.1414  -0.0987  -0.0520   3.3968  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -26.928799   7.716885  -3.490 0.000484 ***
## layer.1       0.002886   0.001568   1.840 0.065738 .  
## layer.2       0.003792   0.001950   1.944 0.051872 .  
## layer.3       0.010475   0.004579   2.287 0.022172 *  
## layer.4      -0.007038   0.008420  -0.836 0.403263    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 186.63  on 2015  degrees of freedom
## Residual deviance: 166.67  on 2011  degrees of freedom
## AIC: 176.67
## 
## Number of Fisher Scoring iterations: 10
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 16 
## n absences     : 2000 
## AUC            : 0.7689844 
## cor            : 0.07635893 
## max TPR+TNR at : -4.210373 
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
## values      : 5.328757e-10, 0.1086947  (min, max)
```

![plot of chunk rbl_reps](figure/rbl_reps-1.png)

```r
rbl.glm$replicate.models$allogus.rep.1
```

```
## 
## 
## Formula:  presence ~ layer.1 + layer.2 + layer.3 + layer.4
## <environment: 0x13c5077c8>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |23 |  -77.9719|  21.7091|    2394|    1789|     966|     364|        1|
## |25 |  -77.9323|  21.6167|    2384|    1666|    1017|     324|        1|
## |24 |  -77.9719|  21.5507|    2402|    1708|     992|     325|        1|
## |20 |  -78.1039|  21.1809|    2461|    1669|     846|     252|        1|
## |21 |  -78.0247|  21.1809|    2437|    1684|     877|     264|        1|
## |22 |  -77.9983|  20.9301|    2315|    1746|     907|     265|        1|
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
## -0.6340  -0.2435  -0.1341  -0.1057   3.1876  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  8.9152046  2.7725103   3.216   0.0013 ** 
## layer.1     -0.0029496  0.0006170  -4.781 1.75e-06 ***
## layer.2     -0.0029550  0.0006244  -4.733 2.22e-06 ***
## layer.3     -0.0025738  0.0010108  -2.546   0.0109 *  
## layer.4      0.0023205  0.0011526   2.013   0.0441 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 629.56  on 3064  degrees of freedom
## Residual deviance: 565.77  on 3060  degrees of freedom
## AIC: 575.77
## 
## Number of Fisher Scoring iterations: 7
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 65 
## n absences     : 3000 
## AUC            : 0.7727538 
## cor            : 0.1463704 
## max TPR+TNR at : -3.974762 
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
## values      : 0.001214865, 0.2975575  (min, max)
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
## | -78.73160| 21.91680|
## | -78.51031| 21.82282|
## | -78.58511| 21.73829|
## | -78.56856| 21.97465|
## | -78.66797| 22.05643|
## | -78.82965| 21.87814|
## | -78.65851| 22.07963|
## | -78.61665| 21.77664|
## | -78.98106| 21.90732|
## | -78.93337| 21.75673|
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
## 
## Replicate 1 ...
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
##      0.6      0.6      0.8      0.4      0.4      0.8 
## 
## Species 1 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.4      0.4      0.2      0.6      0.6      0.6 
## 
## Species 2 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.6 
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
## empirical 0.25505166 0.4847307  0.07962718 0.03345400 0.11206906
## rep 1     0.31492801 0.6414927 -0.90949205 0.03546522 0.11562729
## rep 2     0.43716368 0.7290511 -0.29238059 0.11312029 0.27162324
## rep 3     0.09105604 0.2912342  0.75887141 0.31481204 0.53728637
## rep 4     0.24101114 0.4710123  0.04712485 0.01074355 0.05315645
##               env.cor
## empirical -0.24920585
## rep 1     -0.82114375
## rep 2     -0.50786011
## rep 3      0.09622198
## rep 4     -0.46873091
## 
## Species 1 vs. Ribbon:
##                     D          I  rank.cor        env.D        env.I
## empirical 0.071041118 0.18979319 0.1198372 2.766545e-04 0.0047991467
## rep 1     0.335347430 0.58175582 0.1784809 2.709997e-01 0.4314297444
## rep 2     0.085421347 0.25456716 0.4452348 9.069432e-05 0.0045728833
## rep 3     0.001650135 0.01672329 0.3130459 3.942542e-05 0.0005825454
## rep 4     0.246179647 0.48406697 0.1545095 1.255956e-03 0.0060241369
##                env.cor
## empirical  0.136271478
## rep 1      0.290371427
## rep 2     -0.009939555
## rep 3     -0.040307617
## rep 4      0.516838974
## 
## Species 2 vs. Ribbon:
##                    D         I     rank.cor        env.D       env.I
## empirical 0.04594640 0.1541063 -0.731593063 6.005712e-05 0.001663312
## rep 1     0.22372878 0.4365391 -0.018563448 4.982705e-02 0.148272328
## rep 2     0.07140640 0.2206139  0.063164962 2.673806e-04 0.007668446
## rep 3     0.09026148 0.2531706  0.309535720 1.195730e-03 0.018675390
## rep 4     0.35891830 0.6318136 -0.005079991 7.670282e-02 0.222669419
##               env.cor
## empirical -0.11102810
## rep 1     -0.37305617
## rep 2     -0.08417758
## rep 3      0.02780402
## rep 4     -0.35968925
## 
## Outside vs. Ribbon:
##                    D         I     rank.cor        env.D       env.I
## empirical 0.06058673 0.1855039 -0.738585583 0.0001162381 0.003051796
## rep 1     0.27074151 0.5076287  0.122279650 0.0277775286 0.133345065
## rep 2     0.07512532 0.2277689 -0.071877904 0.0003333168 0.007801890
## rep 3     0.07627138 0.2275844  0.228551831 0.0004567300 0.010268188
## rep 4     0.40895346 0.6938309 -0.003088314 0.0785530532 0.240438833
##              env.cor
## empirical -0.1237694
## rep 1     -0.6058604
## rep 2     -0.1118406
## rep 3     -0.0470752
## rep 4     -0.4176316
```

![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-1.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-2.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-3.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-4.png)


Note that the output table here has slope, intercept, and intercept offset.  

```r
rbr.glm$lines.df
```

```
##          slope   intercept    offset
## 1   0.06521924    26.11545 0.2505311
## 2   0.99150308    97.94741 0.3520545
## 3 -29.52548320 -2243.28807 7.3856032
## 4  -0.75468894   -37.41399 0.3132047
```
The intercept denotes the intercept corresponding to the CENTER of each ribbon.  To get the lines denoting the edges of the ribbons (for example if you want to plot the ribbons on a map), you add and substract the offset.  In other words, the top edge of the ribbon is given by y = (slope * x) + intercept + offset, while the bottom edge is given by y = (slope * x) + intercept - offset.  




### Literature cited

*Broennimann, O., Fitzpatrick, M. C., Pearman, P. B., Petitpierre, B., Pellissier, L., Yoccoz, N. G., Thuiller, W., Fortin, M.-J., Randin, C., Zimmermann, N. E., Graham, C. H. and Guisan, A. (2012), Measuring ecological niche overlap from occurrence and spatial environmental data. Global Ecology and Biogeography, 21: 481–497. doi:10.1111/j.1466-8238.2011.00698.x*

*Levins, R. 1968. Evolution In Changing Environments. Monographs in Population Biology, volume 2. Princeton University Press, Princeton, New Jersey, USA.*

*Schoener, T. W. 1968. Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49:704- 726.*

*Warren, D.L., R.E. Glor, and M. Turelli.  2008. Environmental niche identity versus conservatism: quantitative approaches to niche evolution.  Evolution 62:2868-2883. doi: 10.1111/j.1558-5646.2008.00482.x*

*Warren, D.L., M. Cardillo, D.F. Rosauer, and D.I. Bolnick. 2014. Mistaking geography for biology: inferring processes from species distributions. Trends in Ecology and Evolution 29 (10), 572-580. doi: 10.1016/j.tree.2014.08.003*
