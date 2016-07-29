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
## | -79.82892| 21.81254|
## | -80.02892| 22.08754|
## | -80.19559| 21.96254|
## | -80.07059| 22.04587|
## | -80.28726| 22.01254|
## | -80.25392| 21.92087|
## | -80.07059| 21.93754|
## | -79.89559| 22.03754|
## | -80.10392| 22.04587|
## | -79.86226| 21.69587|
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
## | -77.85392| 21.59587|
## | -75.16226| 20.13754|
## | -74.62059| 20.48754|
## | -76.82892| 20.87087|
## | -76.98726| 20.20420|
## | -76.98726| 20.74587|
## | -75.77892| 20.83754|
## | -77.88726| 21.50420|
## | -76.77892| 20.79587|
## | -77.01226| 20.25420|
## 
## 
## Species name:  allogus
```


## Building an ENM

ENMTools contains functions to simplify the ENM construction process.  Using enmtools.species objects and the correct modeling commands, we can build models very quickly.  These commands are primarily wrappers to dismo model construction and projection functions, and at present are only available for GLM, Maxent, Domain, and Bioclim models.  One of the nice bits about this setup is that it allows enmtools to automatically generate suitability maps, do model evaluation, and plot the marginal suitability of habitat for each variable separately.

### GLM

GLMs require the user to supply a formula, an enmtools.species object, and some environmental data.

```r
ahli.glm <- enmtools.glm(f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, species = ahli, env = env, test.prop = 0.2)
```

```
## Adding environmental data to species ahli 
## 	Processing presence points...
## 	Processing background points...
```

```r
allogus.glm <- enmtools.glm(pres ~ layer.1 + layer.2 + layer.3 + layer.4, allogus, env, test.prop = 0.2)
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
## <environment: 0x2103cd4e8>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
## |5  |  -79.8807|  21.8374|    2244|    1828|     945|     249|        1|
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
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
##      Min        1Q    Median        3Q       Max  
## -0.58601  -0.17490  -0.12099  -0.08644   3.09415  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept) 37.541179  27.276168   1.376    0.169
## layer.1     -0.010814   0.006899  -1.567    0.117
## layer.2     -0.008627   0.007341  -1.175    0.240
## layer.3      0.004601   0.007307   0.630    0.529
## layer.4     -0.025907   0.025746  -1.006    0.314
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 119.24  on 1007  degrees of freedom
## AIC: 129.24
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7644167 
## cor            : 0.101033 
## max TPR+TNR at : -4.778595 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.76075 
## cor            : 0.0535415 
## max TPR+TNR at : -4.526366 
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
## values      : 2.220446e-16, 0.9998754  (min, max)
```

![plot of chunk build_glms](figure/build_glms-1.png)

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
## [1] 0.1208329
## 
## $B2
## [1] 0.8500083
```


ENMTools also provides metrics for measuring similarity between ENMs.  These include Schoener's D (Schoener 1968), I (Warren et al. 2008), and the Spearman rank correlation coefficient between two rasters.  While D and I are commonly used in the ENM literature, they may tend to overestimate similarity between ENMs when many grid cells are of similar values (e.g., when two species prefer different habitat but the region contains a great deal of habitat that is unsuitable for both).  


```r
raster.overlap(ahli.glm, allogus.glm)
```

```
## $D
## [1] 0.4327687
## 
## $I
## [1] 0.7081629
## 
## $rank.cor
## [1] 0.7796255
```


A new feature of the R version of ENMTools is that you can now use these same metrics in the n-dimensional space of all combinations of environmental variables, instead of restricting your measures of model similarity to those sets of conditions that appear in the training region.  This is done by repeatedly drawing Latin hypercube samples from the space of all possible combinations of environmental variables given the min and max of each variable within the training region.  ENMTools continues to draw samples until subsequent iterations differ by less than a specified tolerance value.  Lower tolerance values result in more precise estimates of overlap, but can take much longer to calculate.


```r
env.overlap(ahli.glm, allogus.glm, env, tolerance = .001)
```

```
## $env.D
## [1] 0.408245
## 
## $env.I
## [1] 0.6347189
## 
## $env.cor
## [1] 0.4926132
```

## Hypothesis testing

### Niche identity or equivalency test

In this example, we will run a niche identity (also called equivalency) test, as in Warren et al. 2008.  This test takes the presence points for a pair of species and randomly reassigns them to each species, then builds ENMs for these randomized occurrences.  By doing this many times, we can estimate the probability distribution for ENM overlap between species under the null hypothesis that the two species' occurrences in the environment are effectively a random draw from the same underlying distribution.  Note that niche evolution is only one of many reasons why two species' realized environmental distributions might cause departures from this null hypothesis.  See Warren et al. 2014 for details.

To run an identity test, we need to decide what type of models we will build, how many replicates we will run, and (in the case of GLM) a model formula to use for empirical models and the Monte Carlo replicates.  The resulting object contains the replicate models, p values, and plots of the results.  Typically idenity tests are run with at least 99 replicates, but we are using a smaller number here for the sake of execution time.

_NOTE:_ In order for models to be comparable, both empirical and pseudoreplicate models for the identity test are conducted with pseudoabsence points pooled for the two species being compared.




```r
id.glm <- identity.test(species.1 = ahli, species.2 = allogus, env = env, type = "glm", f = presence ~ layer.1 + layer.2 + layer.3 + layer.4, nreps = 4)
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
## |empirical | 0.2378660| 0.4850409| -0.4715794| 0.0049535| 0.0315152| -0.6177946|
## |rep 1     | 0.8490902| 0.9820837|  0.8197686| 0.7323928| 0.9416170|  0.8436749|
## |rep 2     | 0.7870164| 0.9675795|  0.8920446| 0.7539766| 0.9485887|  0.9182700|
## |rep 3     | 0.7515529| 0.9551694|  0.8484025| 0.6979272| 0.9231188|  0.8655942|
## |rep 4     | 0.7970116| 0.9685659|  0.9540890| 0.7954755| 0.9590802|  0.9448596|
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
##      0.4      0.4      0.2      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|---------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0218492| 0.1102281| 0.0816677|
## |rep 1     | 0.1325339| 0.3146247| 0.0799273| 0.0556328| 0.1685755| 0.1281808|
## |rep 2     | 0.1837833| 0.3460088| 0.1740875| 0.0646558| 0.1279621| 0.0889076|
## |rep 3     | 0.1697960| 0.3629889| 0.1786796| 0.0314632| 0.1582063| 0.1078854|
## |rep 4     | 0.1759489| 0.3761206| 0.2438685| 0.0371328| 0.1625177| 0.1075408|
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
## |empirical | 0.4929334| 0.7052122| 0.2916150| 0.1076568| 0.3109523| 0.2264639|
## |rep 1     | 0.9384611| 0.9936844| 0.8828236| 0.2960355| 0.5334062| 0.6470862|
## |rep 2     | 0.9646204| 0.9985142| 0.8597518| 0.8373124| 0.9570774| 0.9067289|
## |rep 3     | 0.8975298| 0.9769059| 0.4773807| 0.5466378| 0.6882876| 0.5323970|
## |rep 4     | 0.9314990| 0.9910116| 0.5200937| 0.3268299| 0.5654645| 0.5409893|
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


### Rangebreak tests

ENMTools also allows you to perform linear, blob, and ribbon rangebreak tests as developed in Glor and Warren 2011.  The linear and blob tests are two versions of a test that permit one to ask whether the geographic regions occupied by two species are more environmentally different than expected by chance. The ribbon test, meanwhile, is designed to test whether the ranges of two species are divided by a region that is relatively unsuitable to one or both forms. 

For the linear and blob tests, you call them very much like you would the identity and background tests.  Here's a linear one using GLM models:

```r
rbl.glm <- rangebreak.linear(ahli, allogus, env, type = "bc", f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, nreps = 4)
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
##      0.8      0.8      1.0      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390|  0.0706201| 0.0183596| 0.1045352| 0.0875783|
## |rep 1     | 0.0327940| 0.1544608| -0.0172882| 0.2091975| 0.2954721| 0.2462488|
## |rep 2     | 0.0260562| 0.1273407| -0.0779272| 0.1689572| 0.2328281| 0.1862373|
## |rep 3     | 0.0260562| 0.1273407| -0.0779272| 0.1649028| 0.2265067| 0.1812801|
## |rep 4     | 0.1756662| 0.3362473| -0.2074562| 0.0522553| 0.1706804| 0.1240795|
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
##      0.8      0.8      0.8      0.4      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390|  0.0706201| 0.0210538| 0.1103805| 0.0877216|
## |rep 1     | 0.3447122| 0.5480220|  0.4481625| 0.0586744| 0.2221169| 0.1561172|
## |rep 2     | 0.0327940| 0.1544608| -0.0172882| 0.2081115| 0.2972730| 0.2474656|
## |rep 3     | 0.0327940| 0.1544608| -0.0172882| 0.2060799| 0.2952444| 0.2503954|
## |rep 4     | 0.1328502| 0.3177390|  0.0706201| 0.0209647| 0.1130318| 0.0924969|
```

![plot of chunk rangebreak_blob](figure/rangebreak_blob-1.png)


If you want to access the individual replicates (for instance to see how your ranges are being split up), you can find them in the list named "replicate.models" inside your rangebreak test object.

```r
rbl.glm$replicate.models$ahli.rep.1
```

```
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude|
## |:--|---------:|--------:|
## |81 |  -74.1691|  20.2170|
## |80 |  -74.2351|  20.2302|
## |76 |  -74.2747|  20.1114|
## |78 |  -74.2483|  20.2434|
## |77 |  -74.2628|  20.2134|
## |79 |  -74.2483|  20.2963|
## |75 |  -74.3143|  20.2434|
## |74 |  -74.3803|  20.1378|
## |72 |  -74.4595|  20.1906|
## |73 |  -74.4331|  20.2963|
## 
## 
## Model:  class    : Bioclim 
## 
## variables: layer.1 layer.2 layer.3 layer.4 
## 
## 
## presence points: 16 
##    layer.1 layer.2 layer.3 layer.4
## 1     1608    1722     972     445
## 2     1932    1423     998     485
## 3     1972    1243    1112     422
## 4     1933    1431     954     485
## 5     1987    1336    1027     474
## 6     1935    1503     923     521
## 7     2064    1330     901     528
## 8     2060    1291     918     503
## 9     2190    1196     994     502
## 10    2690    1120     385     775
##   (... ...  ...)
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 16 
## n absences     : 2000 
## AUC            : 0.9670781 
## cor            : 0.2822498 
## max TPR+TNR at : 0.0624 
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
## values      : 0, 0.9375  (min, max)
```

![plot of chunk rbl_reps](figure/rbl_reps-1.png)

```r
rbl.glm$replicate.models$allogus.rep.1
```

```
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude|
## |:--|---------:|--------:|
## |65 |  -74.8029|  20.3359|
## |64 |  -74.8161|  20.5867|
## |63 |  -74.8293|  20.5471|
## |60 |  -74.9745|  20.1114|
## |56 |  -75.0537|  20.0454|
## |57 |  -75.0273|  20.1510|
## |62 |  -74.9349|  20.5339|
## |61 |  -74.9481|  20.6264|
## |55 |  -75.1198|  20.1114|
## |59 |  -74.9877|  20.6528|
## 
## 
## Model:  class    : Bioclim 
## 
## variables: layer.1 layer.2 layer.3 layer.4 
## 
## 
## presence points: 65 
##    layer.1 layer.2 layer.3 layer.4
## 1     2268    1102    1199     419
## 2     2558    1292     660     706
## 3     2418    1234     903     604
## 4     2018    1467    1012     355
## 5     1812    1709    1025     325
## 6     2022    1563     972     330
## 7     2312    1107    1243     427
## 8     2345    1410     738     656
## 9     1748    1782     957     332
## 10    2328    1492     779     664
##   (... ...  ...)
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 65 
## n absences     : 3000 
## AUC            : 0.492659 
## cor            : 0.001651773 
## max TPR+TNR at : 0.01528462 
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
## values      : 0, 0.9538462  (min, max)
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
## | -78.83705| 21.99665|
## | -78.59119| 22.09228|
## | -78.95260| 21.71829|
## | -78.66938| 22.07409|
## | -78.68961| 22.02972|
## | -78.65699| 21.75968|
## | -78.66084| 21.81788|
## | -78.54483| 22.07213|
## | -78.55435| 21.89034|
## | -78.50720| 21.96893|
## 
## 
## Background points not defined.
## 
## Species name:  ribbon
```


Now we'll run a ribbon rangebreak test using GLM models.  We also need to tell it the width of the ribbons to generate for the replicates.  The units for the width argument are the same units that the presence points are in; e.g., if the points are in decimal degrees you should supply the width of the barrier in decimal degrees. 

```r
rbr.glm <- rangebreak.ribbon(ahli, allogus, ribbon, env, type = "glm", f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, width = 0.5, nreps = 4)
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
## Adding environmental data to species outside 
## 	Processing presence points...
## 	Processing background points...
##           D           I    rank.cor       env.D       env.I     env.cor 
##  0.19527458  0.50562964 -0.90477792  0.03724451  0.12609301 -0.81955226 
##           D           I    rank.cor       env.D       env.I     env.cor 
##  0.29312502  0.58965173 -0.68624986  0.01827795  0.06419498 -0.61811239 
##         D         I  rank.cor     env.D     env.I   env.cor 
## 0.5788207 0.8427147 0.5165809 0.3066937 0.5437089 0.3345480 
##         D         I  rank.cor     env.D     env.I   env.cor 
## 0.6146587 0.8560236 0.3831645 0.2551422 0.4910825 0.1379002 
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
##         D         I  rank.cor     env.D     env.I   env.cor 
## 0.2372828 0.5059153 0.6954225 0.6388609 0.8617643 0.7510491 
##           D           I    rank.cor       env.D       env.I     env.cor 
##  0.07310801  0.18879309  0.38402180  0.07308795  0.22170284 -0.02303897 
##          D          I   rank.cor      env.D      env.I    env.cor 
## 0.16648518 0.40016390 0.46595893 0.08112808 0.24821285 0.08783486 
##           D           I    rank.cor       env.D       env.I     env.cor 
##  0.16659999  0.39613954  0.32182147  0.05858606  0.20762379 -0.11895617 
## 
## Replicate 3 ...
## 
## Replicate 3 ...
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
## Adding environmental data to species outside 
## 	Processing presence points...
## 	Processing background points...
##           D           I    rank.cor       env.D       env.I     env.cor 
##  0.25872433  0.51384108 -0.23320181  0.02103718  0.06495797 -0.46361170 
##          D          I   rank.cor      env.D      env.I    env.cor 
## 0.56377161 0.79685812 0.64161834 0.03914447 0.13466079 0.13985393 
##         D         I  rank.cor     env.D     env.I   env.cor 
## 0.4476423 0.7785359 0.0837762 0.3151663 0.5757206 0.1527072 
##         D         I  rank.cor     env.D     env.I   env.cor 
## 0.5376308 0.8419810 0.1279619 0.3358928 0.6098148 0.1511592 
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
## Adding environmental data to species outside 
## 	Processing presence points...
## 	Processing background points...
##           D           I    rank.cor       env.D       env.I     env.cor 
##  0.38483016  0.68216936 -0.65447802  0.03724944  0.11678042 -0.65934768 
##           D           I    rank.cor       env.D       env.I     env.cor 
##  0.36616417  0.64434174  0.31692793  0.01388609  0.06139567 -0.31471308 
##            D            I     rank.cor        env.D        env.I 
##  0.246605210  0.524498550 -0.239820389  0.152929538  0.327864828 
##      env.cor 
## -0.009918235 
##          D          I   rank.cor      env.D      env.I    env.cor 
##  0.2786229  0.5608507 -0.3187131  0.1407759  0.3273702 -0.1402993
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
##      0.4      0.2      0.8      0.2      0.2      0.8 
## 
## Species 1 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.4      0.4      0.4      0.8      0.8      0.8 
## 
## Species 2 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.2 
## 
## Outside vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.2      0.2      0.2      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## Species 1 vs. Species 2:
##                   D         I   rank.cor       env.D      env.I    env.cor
## empirical 0.2196104 0.4574754 -0.2115557 0.009587585 0.03821930 -0.3943961
## rep 1     0.1952746 0.5056296 -0.9047779 0.037244508 0.12609301 -0.8195523
## rep 2     0.2372828 0.5059153  0.6954225 0.638860869 0.86176431  0.7510491
## rep 3     0.2587243 0.5138411 -0.2332018 0.021037183 0.06495797 -0.4636117
## rep 4     0.3848302 0.6821694 -0.6544780 0.037249441 0.11678042 -0.6593477
## 
## Species 1 vs. Ribbon:
##                    D         I   rank.cor      env.D      env.I
## empirical 0.22740611 0.4240895  0.2494784 0.07035660 0.15690291
## rep 1     0.29312502 0.5896517 -0.6862499 0.01827795 0.06419498
## rep 2     0.07310801 0.1887931  0.3840218 0.07308795 0.22170284
## rep 3     0.56377161 0.7968581  0.6416183 0.03914447 0.13466079
## rep 4     0.36616417 0.6443417  0.3169279 0.01388609 0.06139567
##               env.cor
## empirical  0.09271614
## rep 1     -0.61811239
## rep 2     -0.02303897
## rep 3      0.13985393
## rep 4     -0.31471308
## 
## Species 2 vs. Ribbon:
##                    D         I   rank.cor      env.D     env.I
## empirical 0.08858545 0.2347509 -0.8449656 0.04476485 0.1473593
## rep 1     0.57882069 0.8427147  0.5165809 0.30669370 0.5437089
## rep 2     0.16648518 0.4001639  0.4659589 0.08112808 0.2482129
## rep 3     0.44764231 0.7785359  0.0837762 0.31516628 0.5757206
## rep 4     0.24660521 0.5244986 -0.2398204 0.15292954 0.3278648
##                env.cor
## empirical -0.493020588
## rep 1      0.334547993
## rep 2      0.087834857
## rep 3      0.152707228
## rep 4     -0.009918235
## 
## Outside vs. Ribbon:
##                   D         I   rank.cor      env.D     env.I    env.cor
## empirical 0.1168835 0.2850018 -0.8501621 0.05003038 0.1724796 -0.5470251
## rep 1     0.6146587 0.8560236  0.3831645 0.25514222 0.4910825  0.1379002
## rep 2     0.1666000 0.3961395  0.3218215 0.05858606 0.2076238 -0.1189562
## rep 3     0.5376308 0.8419810  0.1279619 0.33589275 0.6098148  0.1511592
## rep 4     0.2786229 0.5608507 -0.3187131 0.14077592 0.3273702 -0.1402993
```

![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-1.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-2.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-3.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-4.png)


Note that the output table here has slope, intercept, and intercept offset.  

```r
rbr.glm$lines.df
```

```
##        slope intercept    offset
## 1 -0.0757273  15.22453 0.2507158
## 2  1.7947545 158.68026 0.5136356
## 3 -0.9591510 -53.28086 0.3464075
## 4  0.9273222  92.98000 0.3409478
```
The intercept denotes the intercept corresponding to the CENTER of each ribbon.  To get the lines denoting the edges of the ribbons (for example if you want to plot the ribbons on a map), you add and substract the offset.  In other words, the top edge of the ribbon is given by y = (slope * x) + intercept + offset, while the bottom edge is given by y = (slope * x) + intercept - offset.  


### Literature cited


*Levins, R. 1968. Evolution In Changing Environments. Monographs in Population Biology, volume 2. Princeton University Press, Princeton, New Jersey, USA.*

*Schoener, T. W. 1968. Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49:704- 726.*

*Warren, D.L., R.E. Glor, and M. Turelli.  2008. Environmental niche identity versus conservatism: quantitative approaches to niche evolution.  Evolution 62:2868-2883. doi: 10.1111/j.1558-5646.2008.00482.x*

*Warren, D.L., M. Cardillo, D.F. Rosauer, and D.I. Bolnick. 2014. Mistaking geography for biology: inferring processes from species distributions. Trends in Ecology and Evolution 29 (10), 572-580. doi: 10.1016/j.tree.2014.08.003*
