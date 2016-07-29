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
## | -79.97892| 22.12920|
## | -80.44559| 22.09587|
## | -80.11226| 22.13754|
## | -80.27892| 21.96254|
## | -80.49559| 22.10420|
## | -79.72059| 21.88754|
## | -80.08726| 21.82920|
## | -80.07059| 22.13754|
## | -79.68726| 21.92920|
## | -80.09559| 22.08754|
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
## | -77.53726| 20.92920|
## | -75.88726| 20.14587|
## | -76.81226| 20.13754|
## | -76.87892| 20.32087|
## | -76.57059| 20.43754|
## | -75.54559| 20.03754|
## | -74.85392| 20.61254|
## | -78.57892| 22.28754|
## | -77.92892| 21.06254|
## | -76.77059| 20.74587|
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
## <environment: 0x1a7953840>
## 
## 
## Data table (top ten lines): 
## 
## |   | Longitude| Latitude| layer.1| layer.2| layer.3| layer.4| presence|
## |:--|---------:|--------:|-------:|-------:|-------:|-------:|--------:|
## |2  |  -79.9086|  21.8095|    2289|    1732|     957|     231|        1|
## |3  |  -79.8065|  21.7631|    2158|    1870|     983|     253|        1|
## |6  |  -79.9550|  21.8374|    2250|    1766|     919|     235|        1|
## |7  |  -80.3446|  22.0136|    2201|    1822|     978|     277|        1|
## |8  |  -80.2983|  21.9951|    2214|    1786|     986|     284|        1|
## |9  |  -80.1776|  21.9023|    2287|    1722|     992|     266|        1|
## |10 |  -80.1591|  21.9673|    2984|     965|    1311|     237|        1|
## |11 |  -80.1498|  21.9858|    3042|     841|    1371|     221|        1|
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
##      Min        1Q    Median        3Q       Max  
## -0.67963  -0.17774  -0.11252  -0.06408   3.01502  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) 70.562554  31.023292   2.275   0.0229 *
## layer.1     -0.020232   0.008104  -2.497   0.0125 *
## layer.2     -0.018889   0.008226  -2.296   0.0217 *
## layer.3      0.002563   0.007337   0.349   0.7268  
## layer.4      0.006388   0.024233   0.264   0.7921  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 130.29  on 1011  degrees of freedom
## Residual deviance: 115.70  on 1007  degrees of freedom
## AIC: 125.7
## 
## Number of Fisher Scoring iterations: 8
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.7925417 
## cor            : 0.1060738 
## max TPR+TNR at : -4.534611 
## 
## 
## Proportion of data wittheld for model fitting:  0.2
## 
## Model fit (test data):  class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.64725 
## cor            : 0.03446052 
## max TPR+TNR at : -5.409985 
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
## values      : 1.023326e-07, 0.9999999  (min, max)
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
## [1] 0.3022032
## 
## $B2
## [1] 0.9044366
```


ENMTools also provides metrics for measuring similarity between ENMs.  These include Schoener's D (Schoener 1968), I (Warren et al. 2008), and the Spearman rank correlation coefficient between two rasters.  While D and I are commonly used in the ENM literature, they may tend to overestimate similarity between ENMs when many grid cells are of similar values (e.g., when two species prefer different habitat but the region contains a great deal of habitat that is unsuitable for both).  


```r
raster.overlap(ahli.glm, allogus.glm)
```

```
## $D
## [1] 0.5354678
## 
## $I
## [1] 0.7613164
## 
## $rank.cor
## [1] 0.9348416
```


A new feature of the R version of ENMTools is that you can now use these same metrics in the n-dimensional space of all combinations of environmental variables, instead of restricting your measures of model similarity to those sets of conditions that appear in the training region.  This is done by repeatedly drawing Latin hypercube samples from the space of all possible combinations of environmental variables given the min and max of each variable within the training region.  ENMTools continues to draw samples until subsequent iterations differ by less than a specified tolerance value.  Lower tolerance values result in more precise estimates of overlap, but can take much longer to calculate.


```r
env.overlap(ahli.glm, allogus.glm, env, tolerance = .001)
```

```
## $env.D
## [1] 0.6818962
## 
## $env.I
## [1] 0.9024934
## 
## $env.cor
## [1] 0.8116958
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
## |empirical | 0.2265931| 0.4727261| -0.5028670| 0.0045335| 0.0293038| -0.6193932|
## |rep 1     | 0.7128979| 0.9378403|  0.8646347| 0.7467454| 0.9362524|  0.8870956|
## |rep 2     | 0.6938261| 0.9350205|  0.9812832| 0.8280514| 0.9667688|  0.9859385|
## |rep 3     | 0.8208393| 0.9756773|  0.9121740| 0.7128751| 0.9292004|  0.8417965|
## |rep 4     | 0.8098479| 0.9741119|  0.6244502| 0.5356986| 0.8270608|  0.5463563|
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
##      0.2      0.2      0.2      0.2      0.2      0.4 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|---------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390| 0.0706201| 0.0227783| 0.1167832| 0.0934381|
## |rep 1     | 0.1550902| 0.3533570| 0.1906565| 0.0780825| 0.2399021| 0.2051838|
## |rep 2     | 0.1828433| 0.3846974| 0.2693164| 0.0503326| 0.2024470| 0.1453569|
## |rep 3     | 0.1672873| 0.3587530| 0.1822309| 0.0474253| 0.1418588| 0.0869541|
## |rep 4     | 0.1889390| 0.3943172| 0.2347999| 0.0947458| 0.2757694| 0.2309692|
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
## |empirical | 0.4929334| 0.7052122| 0.2916150| 0.1046276| 0.3067086| 0.2178931|
## |rep 1     | 0.9280731| 0.9898774| 0.9018449| 0.4818515| 0.7024771| 0.6093948|
## |rep 2     | 0.8482934| 0.9551373| 0.5189810| 0.4225439| 0.6500821| 0.5517947|
## |rep 3     | 0.8370481| 0.9480778| 0.7078994| 0.2768454| 0.5162213| 0.4565322|
## |rep 4     | 0.9285330| 0.9881880| 0.7769693| 0.2569012| 0.4947107| 0.4618874|
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
##      0.6      0.6      0.8      0.2      0.2      0.2 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390|  0.0706201| 0.0256577| 0.1209806| 0.0927112|
## |rep 1     | 0.2460585| 0.4587668|  0.2386199| 0.0325718| 0.1380910| 0.1073118|
## |rep 2     | 0.0291281| 0.1375999| -0.0482669| 0.1772115| 0.2428127| 0.1882375|
## |rep 3     | 0.0205862| 0.0999132| -0.1366843| 0.1220002| 0.1733212| 0.1450079|
## |rep 4     | 0.2665037| 0.4373561| -0.1468876| 0.0586286| 0.1760595| 0.1330784|
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
##      0.8      0.8      0.8      0.2      0.4      0.4 
## 
## 
## Replicates:
## 
## 
## |          |         D|         I|   rank.cor|     env.D|     env.I|   env.cor|
## |:---------|---------:|---------:|----------:|---------:|---------:|---------:|
## |empirical | 0.1328502| 0.3177390|  0.0706201| 0.0208916| 0.1137265| 0.0912456|
## |rep 1     | 0.0327940| 0.1544608| -0.0172882| 0.2131787| 0.3037046| 0.2583176|
## |rep 2     | 0.1328502| 0.3177390|  0.0706201| 0.0220642| 0.1127494| 0.0868366|
## |rep 3     | 0.0327940| 0.1544608| -0.0172882| 0.2155562| 0.3082100| 0.2624517|
## |rep 4     | 0.3164711| 0.5401069|  0.4739655| 0.0982013| 0.2974742| 0.2060141|
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
## |28 |  -77.3117|  19.9397|
## |9  |  -80.1776|  21.9023|
## |7  |  -80.3446|  22.0136|
## |14 |  -80.2148|  21.9394|
## |8  |  -80.2983|  21.9951|
## |29 |  -76.9948|  19.9529|
## |12 |  -80.1220|  21.9301|
## |13 |  -80.1776|  21.9673|
## |6  |  -79.9550|  21.8374|
## |10 |  -80.1591|  21.9673|
## 
## 
## Model:  class    : Bioclim 
## 
## variables: layer.1 layer.2 layer.3 layer.4 
## 
## 
## presence points: 16 
##    layer.1 layer.2 layer.3 layer.4
## 1     1909    1460    1018     267
## 2     2287    1722     992     266
## 3     2201    1822     978     277
## 4     2329    1692    1018     269
## 5     2214    1786     986     284
## 6     2252    1018    1136     237
## 7     2898    1033    1231     242
## 8     2914    1020    1256     237
## 9     2250    1766     919     235
## 10    2984     965    1311     237
##   (... ...  ...)
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 16 
## n absences     : 2000 
## AUC            : 0.6778906 
## cor            : 0.03021734 
## max TPR+TNR at : 0.03165 
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
## values      : 0, 0.75  (min, max)
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
## |4  |  -79.8251|  21.8095|
## |15 |  -80.0437|  21.9720|
## |33 |  -76.9155|  20.0058|
## |16 |  -79.9972|  21.9792|
## |35 |  -76.7439|  19.9661|
## |26 |  -77.9323|  20.7320|
## |34 |  -76.9155|  20.1510|
## |31 |  -76.9288|  20.2434|
## |22 |  -77.9983|  20.9301|
## |38 |  -76.5062|  20.1510|
## 
## 
## Model:  class    : Bioclim 
## 
## variables: layer.1 layer.2 layer.3 layer.4 
## 
## 
## presence points: 65 
##    layer.1 layer.2 layer.3 layer.4
## 1     2207    1877     967     259
## 2     2712    1285    1126     250
## 3     2379     893    1197     222
## 4     2861    1150    1194     259
## 5     2069    1366    1004     279
## 6     2174    1811     918     267
## 7     2050    1573     894     237
## 8     2085    1646     809     223
## 9     2315    1746     907     265
## 10    2156    1463     983     265
##   (... ...  ...)
## 
## 
## 
## Model fit (training data):  class          : ModelEvaluation 
## n presences    : 65 
## n absences     : 3000 
## AUC            : 0.5808308 
## cor            : 0.03293112 
## max TPR+TNR at : 0.1383615 
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
## values      : 0, 0.8615385  (min, max)
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
## | -78.61948| 21.87254|
## | -78.94291| 21.99450|
## | -78.97536| 21.75384|
## | -78.56673| 21.85846|
## | -78.65424| 21.77149|
## | -78.65350| 22.06545|
## | -78.71555| 21.96673|
## | -78.65991| 21.88149|
## | -78.61960| 22.07608|
## | -78.55408| 21.91535|
## 
## 
## Background points not defined.
## 
## Species name:  ribbon
```


Now we'll run a ribbon rangebreak test using Domain models.  We also need to tell it the width of the ribbons to generate for the replicates.  The units for the width argument are the same units that the presence points are in; e.g., if the points are in decimal degrees you should supply the width of the barrier in decimal degrees. 

```r
rbr.dm <- rangebreak.ribbon(ahli, allogus, ribbon, env, type = "dm", width = 0.5, nreps = 4)
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
## 
## Building replicate models...
## 
## Replicate 1 ...
## 
## Replicate 2 ...
## 
## Replicate 2 ...
## 
## Replicate 2 ...
## 
## Replicate 3 ...
## 
## Replicate 3 ...
## 
## Replicate 3 ...
## 
## Replicate 4 ...
```

```r
rbr.dm
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
##      0.2      0.2      0.8      0.2      0.2      0.2 
## 
## Species 1 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.4      0.4      1.0      0.4      0.2      0.2 
## 
## Species 2 vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.4      0.4      0.2      0.2      0.2      0.2 
## 
## Outside vs. Ribbon:
##        D        I rank.cor    env.D    env.I  env.cor 
##      0.4      0.4      0.2      0.4      0.4      0.4 
## 
## 
## Replicates:
## 
## Species 1 vs. Species 2:
##                   D         I    rank.cor     env.D     env.I   env.cor
## empirical 0.4929334 0.7052122 0.291614976 0.1062017 0.3090294 0.2240017
## rep 1     0.6846521 0.8665909 0.460035710 0.3226541 0.5601823 0.4845990
## rep 2     0.6611263 0.8513009 0.249306647 0.2330747 0.4667884 0.4004520
## rep 3     0.8031523 0.9404225 0.003535681 0.2024961 0.4350764 0.3290000
## rep 4     0.7920106 0.9304913 0.213040756 0.2237546 0.4600826 0.3507531
## 
## Species 1 vs. Ribbon:
##                    D         I  rank.cor      env.D      env.I    env.cor
## empirical 0.31762587 0.4722237 0.2410013 0.01747076 0.08200333 0.06917822
## rep 1     0.65588702 0.8408897 0.1263618 0.09884288 0.29839634 0.24024892
## rep 2     0.65569381 0.8411919 0.2069590 0.10757855 0.31169086 0.25771564
## rep 3     0.47841438 0.6884352 0.1233109 0.07135872 0.21891820 0.17329843
## rep 4     0.08093423 0.2652462 0.1430492 0.01134944 0.09584924 0.07321640
## 
## Species 2 vs. Ribbon:
##                    D         I    rank.cor       env.D      env.I
## empirical 0.25120112 0.4845404 -0.16542354 0.001368129 0.03439314
## rep 1     0.90507788 0.9832932  0.12120419 0.340902966 0.57169003
## rep 2     0.88940108 0.9879116 -0.06664244 0.537039604 0.74362739
## rep 3     0.49001859 0.7082055  0.60878920 0.026154274 0.14977493
## rep 4     0.07048917 0.2514286  0.35260598 0.001685880 0.03879267
##              env.cor
## empirical 0.03649136
## rep 1     0.37562859
## rep 2     0.57947296
## rep 3     0.15881126
## rep 4     0.04002204
## 
## Outside vs. Ribbon:
##                    D         I   rank.cor       env.D      env.I
## empirical 0.25183687 0.4852926 -0.1703564 0.001923515 0.03981878
## rep 1     0.91116515 0.9830882  0.3093816 0.378411742 0.60279612
## rep 2     0.90102500 0.9895761  0.2088421 0.531739677 0.73808768
## rep 3     0.48480520 0.7037712  0.6676491 0.024579781 0.14500747
## rep 4     0.06889686 0.2482425  0.3364394 0.001418316 0.03628302
##              env.cor
## empirical 0.04389385
## rep 1     0.37492948
## rep 2     0.55737227
## rep 3     0.16053109
## rep 4     0.03769911
```

![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-1.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-2.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-3.png)![plot of chunk rangebreak_ribbon](figure/rangebreak_ribbon-4.png)


Note that the output table here has slope, intercept, and intercept offset.  

```r
rbr.dm$lines.df
```

```
##         slope   intercept    offset
## 1 -0.14007294   10.076429 0.2524406
## 2 -0.20906145    4.711065 0.2554049
## 3 -0.05783969   16.606869 0.2504178
## 4 -6.57908249 -488.699203 1.6636617
```
The intercept denotes the intercept corresponding to the CENTER of the ribbon.  To get the lines denoting the edges of the ribbons (for example if you want to plot the ribbons on a map), you add and substract the offset.  In other words, the top edge of the ribbon is given by y = (slope * x) + intercept + offset, while the bottom edge is given by y = (slope * x) + intercept - offset.  


### Literature cited


*Levins, R. 1968. Evolution In Changing Environments. Monographs in Population Biology, volume 2. Princeton University Press, Princeton, New Jersey, USA.*

*Schoener, T. W. 1968. Anolis lizards of Bimini: resource partitioning in a complex fauna. Ecology 49:704- 726.*

*Warren, D.L., R.E. Glor, and M. Turelli.  2008. Environmental niche identity versus conservatism: quantitative approaches to niche evolution.  Evolution 62:2868-2883. doi: 10.1111/j.1558-5646.2008.00482.x*

*Warren, D.L., M. Cardillo, D.F. Rosauer, and D.I. Bolnick. 2014. Mistaking geography for biology: inferring processes from species distributions. Trends in Ecology and Evolution 29 (10), 572-580. doi: 10.1016/j.tree.2014.08.003*
