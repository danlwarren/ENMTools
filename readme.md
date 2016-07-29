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
## | -80.22059| 22.14587|
## | -80.17892| 21.88754|
## | -79.77892| 21.87087|
## | -80.24559| 22.02087|
## | -80.41226| 22.06254|
## | -80.06226| 22.08754|
## | -80.01226| 21.78754|
## | -80.49559| 22.05420|
## | -80.13726| 21.98754|
## | -80.14559| 21.95420|
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
## | -76.51226| 20.18754|
## | -77.76226| 21.61254|
## | -77.87059| 20.74587|
## | -76.42059| 20.07087|
## | -75.65392| 21.00420|
## | -77.88726| 21.46254|
## | -74.49559| 20.34587|
## | -75.60392| 20.56254|
## | -75.18726| 20.57920|
## | -77.95392| 21.77920|
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
## <environment: 0x10991f6d0>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |4  |  -79.8251|  21.8095|    2207|    1877|     967|     259|        1|
## |5  |  -79.8807|  21.8374|    2244|    1828|     945|     249|        1|
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
## |12 |  -80.1220|  21.9301|    2898|    1033|    1231|     242|        1|
## |13 |  -80.1776|  21.9673|    2914|    1020|    1256|     237|        1|
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
## -0.4723  -0.1771  -0.1187  -0.0864   3.1750  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) 48.324928  28.447535   1.699   0.0894 .
## layer.1     -0.011812   0.007098  -1.664   0.0961 .
## layer.2     -0.012208   0.007724  -1.581   0.1140  
## layer.3     -0.001807   0.008563  -0.211   0.8328  
## layer.4     -0.010149   0.025886  -0.392   0.6950  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 120.17  on 1007  degrees of freedom
## AIC: 130.17
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7609167 
## cor            : 0.09658931 
## max TPR+TNR at : -4.731509 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.7685 
## cor            : 0.05286074 
## max TPR+TNR at : -4.337697 
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
## values      : 2.578613e-06, 0.9998522  (min, max)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

To check out the marginal response functions, you only need to type


```r
ahli.glm$response.plots
```

```
## $layer.1
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```
## 
## $layer.2
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-2.png)

```
## 
## $layer.3
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-3.png)

```
## 
## $layer.4
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-4.png)

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
## [1] 0.1791893
## 
## $B2
## [1] 0.8739704
```


ENMTools also provides metrics for measuring similarity between ENMs.  These include Schoener's D (Schoener 1968), I (Warren et al. 2008), and the Spearman rank correlation coefficient between two rasters.  While D and I are commonly used in the ENM literature, they may tend to overestimate similarity between ENMs when many grid cells are of similar values (e.g., when two species prefer different habitat but the region contains a great deal of habitat that is unsuitable for both).  


```r
raster.overlap(ahli.glm, allogus.glm)
```

```
## $D
## [1] 0.3746357
## 
## $I
## [1] 0.6671822
## 
## $rank.cor
## [1] 0.7289413
```


A new feature of the R version of ENMTools is that you can now use these same metrics in the n-dimensional space of all combinations of environmental variables, instead of restricting your measures of model similarity to those sets of conditions that appear in the training region.  This is done by repeatedly drawing Latin hypercube samples from the space of all possible combinations of environmental variables given the min and max of each variable within the training region.  ENMTools continues to draw samples until subsequent iterations differ by less than a specified tolerance value.  Lower tolerance values result in more precise estimates of overlap, but can take much longer to calculate.


```r
env.overlap(ahli.glm, allogus.glm, env, tolerance = .001)
```

```
## $env.D
## [1] 0.615898
## 
## $env.I
## [1] 0.8325892
## 
## $env.cor
## [1] 0.6045314
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
## |empirical | 0.2549379| 0.5087145| -0.4741229| 0.0060709| 0.0360273| -0.6157128|
## |rep 1     | 0.7900935| 0.9652607|  0.8280853| 0.6755881| 0.9057882|  0.8137148|
## |rep 2     | 0.9216260| 0.9955097|  0.9355298| 0.8137224| 0.9734332|  0.9357490|
## |rep 3     | 0.7747140| 0.9596141|  0.8221390| 0.6805351| 0.9073324|  0.8346341|
## |rep 4     | 0.8580119| 0.9858273|  0.8779465| 0.7260961| 0.9420533|  0.8057560|
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)


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
##      0.2      0.2      0.2      0.2      0.4      0.4 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|---------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0238515| 0.1191308| 0.0930449|
## |rep 1     | 0.1711719| 0.3736953| 0.2326080| 0.0347124| 0.1713737| 0.1176208|
## |rep 2     | 0.1406305| 0.3203625| 0.0900909| 0.0303894| 0.1127785| 0.0753982|
## |rep 3     | 0.1900069| 0.3930840| 0.2557182| 0.0952964| 0.2720201| 0.2195976|
## |rep 4     | 0.1703573| 0.3720913| 0.1906212| 0.0551188| 0.2130892| 0.1569874|
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)


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
## |empirical | 0.4929334| 0.7052122| 0.2916150| 0.1061057| 0.3093705| 0.2240613|
## |rep 1     | 0.9431892| 0.9920177| 0.8248359| 0.4535320| 0.6778889| 0.6114495|
## |rep 2     | 0.9082070| 0.9838918| 0.8337122| 0.2160470| 0.4500973| 0.4272877|
## |rep 3     | 0.9298709| 0.9918487| 0.6456102| 0.4494544| 0.6715506| 0.5761145|
## |rep 4     | 0.7901667| 0.9264743| 0.7276165| 0.3835038| 0.6104119| 0.5198316|
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)


### Literature cited


*Levins, R. 1968. Evolution In Changing Environments. Monographs in Population Biology, volume 2. Princeton University Press, Princeton, New Jersey, USA.*

*Schoener, T. W. 1968. Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49:704- 726.*

*Warren, D.L., R.E. Glor, and M. Turelli.  2008. Environmental niche identity versus conservatism: quantitative approaches to niche evolution.  Evolution 62:2868-2883. doi: 10.1111/j.1558-5646.2008.00482.x*

*Warren, D.L., M. Cardillo, D.F. Rosauer, and D.I. Bolnick. 2014. Mistaking geography for biology: inferring processes from species distributions. Trends in Ecology and Evolution 29 (10), 572-580. doi: 10.1016/j.tree.2014.08.003*
