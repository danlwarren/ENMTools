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
## | -80.21226| 22.00420|
## | -79.97059| 21.97920|
## | -80.01226| 21.91254|
## | -79.70392| 21.78754|
## | -80.02892| 21.87920|
## | -79.93726| 21.90420|
## | -79.96226| 21.97920|
## | -80.49559| 22.10420|
## | -80.20392| 22.12920|
## | -80.26226| 22.16254|
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
## | -76.92892| 20.02087|
## | -77.99559| 21.15420|
## | -77.90392| 20.90420|
## | -76.67892| 20.08754|
## | -78.01226| 20.87087|
## | -74.86226| 20.34587|
## | -78.87059| 22.32920|
## | -74.59559| 20.13754|
## | -75.89559| 20.74587|
## | -77.38726| 20.83754|
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
## <environment: 0x14879aeb0>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |1  |  -80.0106|  21.8744|    2765|    1235|    1174|     252|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
## |5  |  -79.8807|  21.8374|    2244|    1828|     945|     249|        1|
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
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
## -0.34792  -0.18472  -0.13575  -0.09527   3.16035  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) 45.483281  26.051048   1.746   0.0808 .
## layer.1     -0.012871   0.006510  -1.977   0.0480 *
## layer.2     -0.012953   0.006966  -1.859   0.0630 .
## layer.3     -0.000731   0.007737  -0.094   0.9247  
## layer.4      0.011409   0.023795   0.479   0.6316  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 123.44  on 1007  degrees of freedom
## AIC: 133.44
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7134583 
## cor            : 0.07592817 
## max TPR+TNR at : -4.346055 
## 
## 
## Environment space model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 10000 
## AUC            : 0.4518917 
## cor            : -0.02473048 
## max TPR+TNR at : 0.00673785 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.83125 
## cor            : 0.07184534 
## max TPR+TNR at : -4.476669 
## 
## 
## Environment space model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 10000 
## AUC            : 0.47615 
## cor            : -0.0138132 
## max TPR+TNR at : 0.0111822 
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
## values      : 1.135386e-05, 0.9998385  (min, max)
## 
## 
## 
## Notes:
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
## <environment: 0x12b9acb18>
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
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
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
## -0.7610  -0.1668  -0.1164  -0.0759   3.2656  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept) 24.589337  29.051338   0.846    0.397
## layer.1     -0.007163   0.007483  -0.957    0.338
## layer.2     -0.005493   0.008019  -0.685    0.493
## layer.3      0.006248   0.007708   0.811    0.418
## layer.4     -0.037013   0.030248  -1.224    0.221
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 115.18  on 1007  degrees of freedom
## AIC: 125.18
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7904583 
## cor            : 0.1253115 
## max TPR+TNR at : -4.363448 
## 
## 
## Environment space model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 10000 
## AUC            : 0.522075 
## cor            : -0.01471507 
## max TPR+TNR at : 0.00482652 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.4895 
## cor            : -0.0007880612 
## max TPR+TNR at : -5.431829 
## 
## 
## Environment space model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 10000 
## AUC            : 0.370675 
## cor            : -0.0111435 
## max TPR+TNR at : 0.004328 
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
## values      : 2.220446e-16, 0.9995777  (min, max)
## 
## 
## 
## Notes:
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
## <environment: 0x12b91de28>
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
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
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
##     Min       1Q   Median       3Q      Max  
## -0.6696  -0.1775  -0.1168  -0.0836   3.1828  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -4.9456     0.4525 -10.930   <2e-16 ***
## poly(layer.1, 2)1 -87.0814    63.4756  -1.372    0.170    
## poly(layer.1, 2)2   7.8501    28.0669   0.280    0.780    
## poly(layer.2, 2)1 -73.8789    72.7241  -1.016    0.310    
## poly(layer.2, 2)2 -17.6682    32.9336  -0.536    0.592    
## poly(layer.3, 2)1  22.8530    43.2015   0.529    0.597    
## poly(layer.3, 2)2  14.7550    17.9024   0.824    0.410    
## poly(layer.4, 2)1  -2.9445    16.9686  -0.174    0.862    
## poly(layer.4, 2)2  -5.8123    13.7265  -0.423    0.672    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 117.64  on 1003  degrees of freedom
## AIC: 135.64
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7587917 
## cor            : 0.1103606 
## max TPR+TNR at : -4.480382 
## 
## 
## Environment space model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 10000 
## AUC            : 0.4652833 
## cor            : -0.02168107 
## max TPR+TNR at : 0.00625 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.7305 
## cor            : 0.04842947 
## max TPR+TNR at : -4.795759 
## 
## 
## Environment space model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 10000 
## AUC            : 0.44065 
## cor            : -0.01337351 
## max TPR+TNR at : 0.00812629 
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
## values      : 2.220446e-16, 0.9998468  (min, max)
## 
## 
## 
## Notes:
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
## $background.plot
```

![plot of chunk visualize.enm](figure/visualize.enm-1.png)

```
## 
## $suit.plot
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
## [1] 0.8561426
## 
## $B2
## [1] 0.1503983
```


ENMTools also provides metrics for measuring similarity between ENMs.  These include Schoener's D (Schoener 1968), I (Warren et al. 2008), and the Spearman rank correlation coefficient between two rasters.  While D and I are commonly used in the ENM literature, they may tend to overestimate similarity between ENMs when many grid cells are of similar values (e.g., when two species prefer different habitat but the region contains a great deal of habitat that is unsuitable for both).  


```r
raster.overlap(ahli.glm, allogus.glm)
```

```
## $D
## [1] 0.2969003
## 
## $I
## [1] 0.5693663
## 
## $rank.cor
## [1] 0.3715564
```


A new feature of the R version of ENMTools is that you can now use these same metrics in the n-dimensional space of all combinations of environmental variables, instead of restricting your measures of model similarity to those sets of conditions that appear in the training region.  This is done by repeatedly drawing Latin hypercube samples from the space of all possible combinations of environmental variables given the min and max of each variable within the training region.  ENMTools continues to draw samples until subsequent iterations differ by less than a specified tolerance value.  Lower tolerance values result in more precise estimates of overlap, but can take much longer to calculate.


```r
env.overlap(ahli.glm, allogus.glm, env, tolerance = .001)
```

```
## $env.D
## [1] 0.2094537
## 
## $env.I
## [1] 0.4200186
## 
## $env.cor
## [1] -0.1655452
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
## |empirical | 0.2423946| 0.4914628| -0.4676267| 0.0048664| 0.0304449| -0.6236641|
## |rep 1     | 0.7314653| 0.9498920|  0.0857171| 0.5973769| 0.8712844|  0.5808595|
## |rep 2     | 0.8802709| 0.9868598|  0.9124070| 0.8299757| 0.9774250|  0.9354586|
## |rep 3     | 0.7988205| 0.9708954|  0.9095257| 0.7858604| 0.9581561|  0.9253785|
## |rep 4     | 0.6827519| 0.9205069|  0.1885558| 0.5554490| 0.8337030|  0.5411092|
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
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0187870| 0.1041863| 0.0818951|
## |rep 1     | 0.1781858| 0.3804103| 0.2279026| 0.0498421| 0.1948455| 0.1337430|
## |rep 2     | 0.1620186| 0.3484133| 0.1628987| 0.0361928| 0.1433519| 0.0931138|
## |rep 3     | 0.1522817| 0.3290357| 0.1259936| 0.0365429| 0.1472028| 0.1003657|
## |rep 4     | 0.1837394| 0.3856388| 0.2586160| 0.0512299| 0.1923614| 0.1220691|
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
## |empirical | 0.4929334| 0.7052122| 0.2916150| 0.1050988| 0.3072914| 0.2254372|
## |rep 1     | 0.9793292| 0.9991630| 0.9227508| 0.6885426| 0.8561588| 0.7866524|
## |rep 2     | 0.9303951| 0.9936955| 0.3657643| 0.3737846| 0.6086565| 0.5743199|
## |rep 3     | 0.9820853| 0.9997207| 0.9140189| 0.8824155| 0.9757635| 0.9445721|
## |rep 4     | 0.6708239| 0.8421850| 0.5097788| 0.2748165| 0.5074674| 0.4160928|
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
## |ahli.bg | -80.21226| 22.00420|    2635|    1376|
## |ahli.bg | -79.97059| 21.97920|    2770|    1270|
## |ahli.bg | -80.01226| 21.91254|    2899|    1072|
## |ahli.bg | -79.70392| 21.78754|    2252|    1846|
## |ahli.bg | -80.02892| 21.87920|    2497|    1506|
## |ahli.bg | -79.93726| 21.90420|    2439|    1651|
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
## |allogus | -76.92892| 20.02087|    2209|    1166|
## |allogus | -77.99559| 21.15420|    2418|    1656|
## |allogus | -77.90392| 20.90420|    2334|    1718|
## |allogus | -76.67892| 20.08754|    2200|    1285|
## |allogus | -78.01226| 20.87087|    2246|    1776|
## |allogus | -74.86226| 20.34587|    2262|    1136|
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
## |ahli.bg | -80.21226| 22.00420|    2635|    1116|
## |ahli.bg | -79.97059| 21.97920|    2770|    1152|
## |ahli.bg | -80.01226| 21.91254|    2899|    1209|
## |ahli.bg | -79.70392| 21.78754|    2252|    1052|
## |ahli.bg | -80.02892| 21.87920|    2497|    1048|
## |ahli.bg | -79.93726| 21.90420|    2439|    1009|
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
## |allogus | -76.92892| 20.02087|    2209|    1100|
## |allogus | -77.99559| 21.15420|    2418|     882|
## |allogus | -77.90392| 20.90420|    2334|     889|
## |allogus | -76.67892| 20.08754|    2200|    1042|
## |allogus | -78.01226| 20.87087|    2246|     909|
## |allogus | -74.86226| 20.34587|    2262|    1202|
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
##   D   I 
## 0.5 0.5
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
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
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
##      0.8      0.8      0.8      0.6      0.6      0.4 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|    env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|----------:|
## |empirical | 0.2423946| 0.4914628| -0.4676267| 0.0051010| 0.0306233| -0.6208082|
## |rep 1     | 0.2423946| 0.4914628| -0.4676267| 0.0049956| 0.0302450| -0.6146452|
## |rep 2     | 0.2598881| 0.5626930| -0.8607183| 0.0112605| 0.0487483| -0.7830435|
## |rep 3     | 0.2423946| 0.4914628| -0.4676267| 0.0050125| 0.0303632| -0.6146420|
## |rep 4     | 0.0745316| 0.2442929|  0.8541111| 0.6879798| 0.8606347|  0.8840035|
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
##      1.0      1.0      1.0      0.6      0.6      0.6 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390|  0.0706201| 0.0230064| 0.1193639| 0.0959469|
## |rep 1     | 0.1328502| 0.3177390|  0.0706201| 0.0229022| 0.1165019| 0.0954811|
## |rep 2     | 0.1328502| 0.3177390|  0.0706201| 0.0206187| 0.1109440| 0.0842675|
## |rep 3     | 0.0327940| 0.1544608| -0.0172882| 0.2098371| 0.2981679| 0.2495742|
## |rep 4     | 0.0327940| 0.1544608| -0.0172882| 0.2064782| 0.2913209| 0.2423995|
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
## <environment: 0x1487db388>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
## |14 |  -80.2148|  21.9394|    2329|    1692|    1018|     269|        1|
## |13 |  -80.1776|  21.9673|    2914|    1020|    1256|     237|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |12 |  -80.1220|  21.9301|    2898|    1033|    1231|     242|        1|
## |15 |  -80.0437|  21.9720|    2712|    1285|    1126|     250|        1|
## |16 |  -79.9972|  21.9792|    2861|    1150|    1194|     259|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5561  -0.1626  -0.0914  -0.0322   3.4119  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -20.710290   9.001583  -2.301   0.0214 *
## layer.1       0.003194   0.002019   1.582   0.1136  
## layer.2       0.005833   0.002728   2.138   0.0325 *
## layer.3       0.010255   0.005017   2.044   0.0409 *
## layer.4      -0.044748   0.018734  -2.389   0.0169 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 186.63  on 2015  degrees of freedom
## Residual deviance: 164.55  on 2011  degrees of freedom
## AIC: 174.55
## 
## Number of Fisher Scoring iterations: 11
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 16 
## n absences     : 2000 
## AUC            : 0.7862031 
## cor            : 0.05424597 
## max TPR+TNR at : -4.38544 
## 
## 
## Environment space model fit (training data):  class          : ModelEvaluation 
## n presences    : 16 
## n absences     : 10000 
## AUC            : 0.9398125 
## cor            : 0.002225047 
## max TPR+TNR at : 0.00293505 
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
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer 
## values      : 2.220446e-16, 0.1922012  (min, max)
## 
## 
## 
## Notes:
```

![plot of chunk rbl_reps](figure/rbl_reps-1.png)

```r
rbl.glm$replicate.models$allogus.rep.1
```

```
## 
## 
## Formula:  presence ~ layer.1 + layer.2 + layer.3 + layer.4
## <environment: 0x14fda29b8>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |17 |  -79.2527|  22.2109|    2656|    1683|    1097|     325|        1|
## |18 |  -78.7774|  22.2241|    2373|    1967|    1067|     375|        1|
## |19 |  -78.6189|  22.2373|    2317|    1980|    1065|     374|        1|
## |23 |  -77.9719|  21.7091|    2394|    1789|     966|     364|        1|
## |25 |  -77.9323|  21.6167|    2384|    1666|    1017|     324|        1|
## |24 |  -77.9719|  21.5507|    2402|    1708|     992|     325|        1|
## |20 |  -78.1039|  21.1809|    2461|    1669|     846|     252|        1|
## |21 |  -78.0247|  21.1809|    2437|    1684|     877|     264|        1|
## |22 |  -77.9983|  20.9301|    2315|    1746|     907|     265|        1|
## |26 |  -77.9323|  20.7320|    2174|    1811|     918|     267|        1|
## 
## 
## Model:  
## Call:
## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
##     2)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5689  -0.2343  -0.1348  -0.1123   3.3795  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  6.8031428  2.7285041   2.493  0.01265 *  
## layer.1     -0.0027188  0.0006126  -4.438 9.07e-06 ***
## layer.2     -0.0025656  0.0006307  -4.068 4.74e-05 ***
## layer.3     -0.0018200  0.0009845  -1.849  0.06453 .  
## layer.4      0.0031550  0.0011725   2.691  0.00713 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 629.56  on 3064  degrees of freedom
## Residual deviance: 569.72  on 3060  degrees of freedom
## AIC: 579.72
## 
## Number of Fisher Scoring iterations: 7
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 65 
## n absences     : 3000 
## AUC            : 0.7566462 
## cor            : 0.1442777 
## max TPR+TNR at : -4.036004 
## 
## 
## Environment space model fit (training data):  class          : ModelEvaluation 
## n presences    : 65 
## n absences     : 10000 
## AUC            : 0.3554923 
## cor            : -0.04843934 
## max TPR+TNR at : 0.00726629 
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
## class       : RasterLayer 
## dimensions  : 418, 1535, 641630  (nrow, ncol, ncell)
## resolution  : 0.008333333, 0.008333333  (x, y)
## extent      : -86.90809, -74.11642, 19.80837, 23.2917  (xmin, xmax, ymin, ymax)
## coord. ref. : NA 
## data source : in memory
## names       : layer 
## values      : 0.001732725, 0.2473691  (min, max)
## 
## 
## 
## Notes:
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
## | -78.53969| 21.82411|
## | -78.56243| 21.74528|
## | -78.53090| 21.95192|
## | -78.68338| 21.87045|
## | -78.82232| 21.75876|
## | -78.68419| 21.82020|
## | -78.63204| 21.79578|
## | -78.50488| 21.74135|
## | -78.98068| 21.80565|
## | -78.69952| 21.88417|
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
##      0.8      0.8      0.4      0.4      0.4      0.4 
## 
## Species 1 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.8      0.8      0.6      0.2      0.4      1.0 
## 
## Species 2 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.8 
## 
## Outside vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.6 
## 
## 
## Replicates:
## 
## Species 1 vs. Species 2:
##                   D         I   rank.cor      env.D      env.I
## empirical 0.2555170 0.4839483  0.1006524 0.01651305 0.07364266
## rep 1     0.1478658 0.3683608  0.2865413 0.01669839 0.10277525
## rep 2     0.1168419 0.3126445  0.9445843 0.54112401 0.77730674
## rep 3     0.2133447 0.4563937  0.7623256 0.47344549 0.74759235
## rep 4     0.2596372 0.5639426 -0.9173708 0.01566474 0.06188841
##                env.cor
## empirical -0.322227467
## rep 1      0.007414908
## rep 2      0.733304755
## rep 3      0.546925466
## rep 4     -0.843140497
## 
## Species 1 vs. Ribbon:
##                    D          I   rank.cor        env.D        env.I
## empirical 0.11438556 0.27351783  0.2944950 7.869589e-05 0.0031328552
## rep 1     0.09186975 0.21173809  0.2521648 1.798992e-04 0.0005840813
## rep 2     0.02055015 0.07058198  0.4196195 6.802688e-02 0.1118551228
## rep 3     0.04078986 0.11383227  0.3254987 5.158646e-03 0.0220129887
## rep 4     0.31072983 0.57203330 -0.1517009 7.031867e-02 0.1590768246
##                env.cor
## empirical  0.065862874
## rep 1     -0.262256972
## rep 2     -0.196850133
## rep 3      0.002135542
## rep 4      0.032997943
## 
## Species 2 vs. Ribbon:
##                    D          I    rank.cor        env.D       env.I
## empirical 0.01635843 0.09424207 -0.52418536 1.502532e-05 0.001929839
## rep 1     0.35311627 0.61633498  0.09753933 8.040136e-02 0.169482519
## rep 2     0.17038422 0.36393891  0.30157097 2.770732e-02 0.123069210
## rep 3     0.10156259 0.27685185  0.40202949 3.025220e-03 0.024108437
## rep 4     0.27280160 0.53642632  0.02758099 4.373540e-02 0.121315526
##               env.cor
## empirical -0.05675320
## rep 1     -0.22692339
## rep 2     -0.46025427
## rep 3      0.03226611
## rep 4     -0.22214655
## 
## Outside vs. Ribbon:
##                    D         I    rank.cor        env.D       env.I
## empirical 0.02191999 0.1121664 -0.55428272 1.728237e-05 0.001956698
## rep 1     0.39283150 0.6446962  0.24101517 2.564506e-02 0.112483684
## rep 2     0.15546867 0.3509850  0.36222217 2.507394e-01 0.471928678
## rep 3     0.09821694 0.2711951  0.26464777 2.982444e-03 0.024732838
## rep 4     0.32406362 0.6023544  0.06025467 4.662479e-02 0.166836150
##               env.cor
## empirical -0.05643613
## rep 1     -0.63179140
## rep 2      0.23331919
## rep 3     -0.03211970
## rep 4     -0.29791979
```

![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-1.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-2.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-3.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-4.png)


Note that the output table here has slope, intercept, and intercept offset.  

```r
rbr.glm$lines.df
```

```
##         slope intercept    offset
## 1  0.12736250  30.75222 0.2520195
## 2  5.30570533 427.76876 1.3497803
## 3  1.27465911 118.76634 0.4050275
## 4 -0.08422275  14.55145 0.2508851
```
The intercept denotes the intercept corresponding to the CENTER of each ribbon.  To get the lines denoting the edges of the ribbons (for example if you want to plot the ribbons on a map), you add and substract the offset.  In other words, the top edge of the ribbon is given by y = (slope * x) + intercept + offset, while the bottom edge is given by y = (slope * x) + intercept - offset.  


### Building an enmtools.clade object

Some of the tests in ENMTools, including some really neat ones that are still in development, require you to build an enmtools.clade object.  These objects are simply lists that contain a phylogeny and a set of enmtools.species objects.  It's important that the names of the species objects and their species.name attributes match the names in the phylogeny's tip.labels.  For demonstration, we're going to build an object for a clade of five anoles from Hispaniola.  We have the tree, so we're just going to grab occurrence data from GBIF using the rgbif package.


```r
library(rgbif)

hisp.anoles <- read.nexus(file = "test/testdata/StarBEAST_MCC.species.txt")

keepers <- c("brevirostris", "marron", "caudalis", "websteri", "distichus")

hisp.anoles <- drop.tip(phy = hisp.anoles, tip = hisp.anoles$tip.label[!hisp.anoles$tip.label %in% keepers])
plot(hisp.anoles)
```

![plot of chunk read_tree](figure/read_tree-1.png)

So there's our tree.  Now we're going to grab some environmental data.


```r
hisp.env <- stack(list.files("test/testdata/Hispaniola_Worldclim", full.names = TRUE))
hisp.env <- setMinMax(hisp.env)
```

And then we'll create a function to build species from GBIF.

```r
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


Now we'll create five species and add them to a species.clade object that is called brev.clade.

```r
brevirostris <- species.from.gbif(genus = "Anolis", species = "brevirostris", env = hisp.env)
marron <- species.from.gbif(genus = "Anolis", species = "marron", env = hisp.env)
caudalis <- species.from.gbif(genus = "Anolis", species = "caudalis", env = hisp.env)
websteri <- species.from.gbif(genus = "Anolis", species = "websteri", env = hisp.env)
distichus <- species.from.gbif(genus = "Anolis", species = "distichus", env = hisp.env)


brev.clade <- enmtools.clade(species = list(brevirostris, marron, caudalis, websteri, distichus), tree = hisp.anoles)
check.clade(brev.clade)
```

```
## 
## 
## An enmtools.clade object with 5 species
## 
## Species names: 
## 	 brevirostris 	 caudalis 	 distichus 	 marron 	 websteri
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
##              species.names  in.tree presence background range    
## brevirostris "brevirostris" TRUE    175      0          "present"
## caudalis     "caudalis"     TRUE    16       0          "present"
## distichus    "distichus"    TRUE    628      0          "present"
## marron       "marron"       TRUE    11       0          "present"
## websteri     "websteri"     TRUE    17       0          "present"
```


### Age-overlap correlation tests (AOC)

The AOC tests allow you to examine patterns of range, point, and ENM overlap in the context of a phylogeny.  This is effectively a generalized version of several analyses: age-range correlation (e.g., Fitzpatrick and Turelli 2006), ENM overlap in the context of a phylogeny (e.g., Knouft et al. 2006, Warren et al. 2008), and point overlaps (e.g., Cardillo and Warren 2016).

These tests require the creation of an enmtools.clade object, as above.  AOC tests consist of two steps: first, the average overlap at each node in the phylogeny is calcualted using a method that takes tree topology into account (see Fitzpatrick and Turelli 2006), then we perform a linear regression to measure the relationship between node age and average overlap.  Due to the fact that these overlaps violate many of the assumptions of a regular linear regression, however (e.g., errors are not iid), we can't calculate significance in the typical way.  Instead we performa  Monte Carlo test, permuting the identity of the tips of the tree and repeating the node averaging and modeling steps.  Finally we measure statistical significance by comparing the empirical slope and intercept to the distribution of slopes and intercepts from the Monte Carlo replicates.

First, let's do one using range overlaps, as in Fitzpatrick and Turelli 2006.  Note that this analysis requires that each of your species have a range raster stored in their species object (we did that as part of the function used above).  


```r
range.aoc <- enmtools.aoc(clade = brev.clade,  nreps = 50, overlap.source = "range")
summary(range.aoc)
```

```
## 
## 
## Age-Overlap Correlation test
## 
## 50 replicates 
## 
## p values:
## (Intercept)         age 
##  0.07843137  0.00000000
```

![plot of chunk range_aoc](figure/range_aoc-1.png)


Now we can do one using point overlaps just by changing the overlap.source argument:


```r
point.aoc <- enmtools.aoc(clade = brev.clade,  nreps = 50, overlap.source = "points")
summary(point.aoc)
```

```
## 
## 
## Age-Overlap Correlation test
## 
## 50 replicates 
## 
## p values:
## (Intercept)         age 
##  0.11764706  0.07843137
```

![plot of chunk point_aoc](figure/point_aoc-1.png)



Or we can use similarity between ENMs built for each species.  Here we'll use Bioclim models:


```r
bc.aoc <- enmtools.aoc(clade = brev.clade,  env = hisp.env, nreps = 50, overlap.source = "bc")
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
## 
## No background points provided, drawing background from range raster.
```

```
## Warning in couldBeLonLat(mask): CRS is NA. Assuming it is longitude/
## latitude
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
## 
## No background points provided, drawing background from range raster.
```

```
## Warning in couldBeLonLat(mask): CRS is NA. Assuming it is longitude/
## latitude
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

```r
summary(bc.aoc)
```

```
## 
## 
## Age-Overlap Correlation test
## 
## 50 replicates 
## 
## p values:
## (Intercept)         age 
##   0.7058824   0.6274510
```

![plot of chunk enm_aoc](figure/enm_aoc-1.png)


### Literature cited

*Broennimann, O., Fitzpatrick, M. C., Pearman, P. B., Petitpierre, B., Pellissier, L., Yoccoz, N. G., Thuiller, W., Fortin, M.-J., Randin, C., Zimmermann, N. E., Graham, C. H. and Guisan, A. (2012), Measuring ecological niche overlap from occurrence and spatial environmental data. Global Ecology and Biogeography, 21: 481–497. doi:10.1111/j.1466-8238.2011.00698.x*

*Fitzpatrick, B. M., & Turelli, M. (2006). The geography of mammalian speciation: mixed signals from phylogenies and range maps. Evolution, 60(3), 601-615.*

*Knouft, J. H., Losos, J. B., Glor, R. E., & Kolbe, J. J. (2006). Phylogenetic analysis of the evolution of the niche in lizards of the Anolis sagrei group. Ecology, 87(sp7).*

*Levins, R. 1968. Evolution In Changing Environments. Monographs in Population Biology, volume 2. Princeton University Press, Princeton, New Jersey, USA.*

*Schoener, T. W. 1968. Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49:704- 726.*

*Warren, D.L., R.E. Glor, and M. Turelli.  2008. Environmental niche identity versus conservatism: quantitative approaches to niche evolution.  Evolution 62:2868-2883. doi: 10.1111/j.1558-5646.2008.00482.x*

*Warren, D.L., M. Cardillo, D.F. Rosauer, and D.I. Bolnick. 2014. Mistaking geography for biology: inferring processes from species distributions. Trends in Ecology and Evolution 29 (10), 572-580. doi: 10.1016/j.tree.2014.08.003*
