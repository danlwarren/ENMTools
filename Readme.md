ENMTools
========

<!-- badges: start -->

[![R build
status](https://github.com/danlwarren/ENMTools/workflows/R-CMD-check/badge.svg)](https://github.com/danlwarren/ENMTools/actions)
<!-- badges: end -->

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

### Installing extras

ENMTools uses functionality from a *LOT* of other R packages, and it’s
possible that you don’t want to install them all. For that reason many
of the packages are not automatically installed with ENMTools, but
instead “suggested”. If you want to install all of the suggested
packages, we have a function for that. You should only need to use it
after you first install ENMTools or update R. If you choose not to
install the extra packages, you will get warnings when you try to use
functions that require them.

``` r
install.extras()
```

------------------------------------------------------------------------

Interacting with ENMTools
=========================

### Creating enmtools.species objects

We’re going to load in some environmental data. You can do this from
local rasters, like so:

``` r
env.files <- list.files(path = "./env_pca/", pattern = "pc", full.names = TRUE)
env <- stack(env.files)
names(env) <- c("pc1", "pc2", "pc3", "pc4")
env <- setMinMax(env)
```

Or you can load them from the internet using the raster package’s
getData() function.

``` r
library(raster)
env <- raster::getData('worldclim', var='bio', res=10)
env <- crop(env, extent(-10, 17, 39, 48))
plot(env[[1]])
```

![](Readme_files/figure-markdown_github/getdata-1.png)

ENMTools is primarily designed to examine patterns of similarity and
difference between ENMs for different species. In order to simplify
interactions with the functions in ENMTools, you need to put your data
for each of your species into an enmtools.species object. You can create
and view an empty enmtools.species object just by typing:

``` r
monticola <- enmtools.species()
monticola
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

You can add bits of it when the object is created:

``` r
monticola.path <- paste(system.file(package="ENMTools"), "/monticola.csv", sep='')
monticola <- enmtools.species(species.name = "monticola", 
                            presence.points = read.csv(monticola.path))
monticola$range <- background.raster.buffer(monticola$presence.points, 50000, mask = env)
monticola$background.points <- background.points.buffer(points = monticola$presence.points,
                                                   radius = 20000, n = 1000, mask = env[[1]])
```

Or you can add data to this object after it’s created:

``` r
names(monticola)
monticola$species.name <- "monticola"
monticola$presence.points <- read.csv(monticola.path)
monticola$range <- background.raster.buffer(monticola$presence.points, 50000, mask = env)
monticola$background.points <- background.points.buffer(points = monticola$presence.points,
                                                   radius = 20000, n = 1000, mask = env[[1]])
```

It’s always a very good idea to run the check.species() function on an
enmtools.species object after you build or modify it.

``` r
monticola <- check.species(monticola)
```

And now we can take a look at our species!

``` r
interactive.plot.enmtools.species(monticola)
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-7ab8cac82609a2c577b8">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldPhysical",null,"Base map",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addRasterImage","args":["data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAKIAAAA2CAYAAABEBUJOAAABNUlEQVR4nO3aUXKCMBRA0bTTBbOULrlftkiJvpCHAjnnT2BGGO8EQiwFAAAAAAAAAAAAAAAAAAAAgNf4aDz+e2XblHEijK0lxLUItxIvdx6FeAtvKrkRPiLQQa2F+KroasQ4oK/F53dHuJfadYn+IM42Ii7PLRLSs+sR4wF8vvsEFloirG1r2c9BREOcyv4jR2uEkX1Gu5NYPiOW8jdLzvwR5zNvcfBPxgvtiIz4ekY+k5WDO1OIa98vpItoDbGU9hjFwlNbQpwzUSBFb4il5N4u3XoHlRFiFhOKgR0lRKsfg9srxMgt1qSHX72vbzICixLihWX8MXYKHNNLhBcXXWveutYLIdERMRLbbdTKCtMoOJCsEJfR9MQowAFlPyNGjq8R4MB61pqj4cyDrE1sRAgAANz5AcpYLLKmw6GKAAAAAElFTkSuQmCC",[[48,-9.99999999999999],[39,17]],1,null,null,"Range"]},{"method":"addCircleMarkers","args":[[43.75,43.75,43.75,43.75,43.75,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.5833333333333,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.4166666666667,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.25,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,43.0833333333333,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.9166666666667,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.75,42.5833333333333,42.5833333333333,42.5833333333333,42.5833333333333,42.5833333333333,42.5833333333333,42.5833333333333,42.5833333333333,42.5833333333333,42.5833333333333,42.5833333333333,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.4166666666667,42.25,42.25,42.25,42.25,42.25,42.25,42.25,42.25,42.25,42.0833333333333,42.0833333333333,42.0833333333333,42.0833333333333,42.0833333333333,42.0833333333333,42.0833333333333,42.0833333333333,41.9166666666667,41.9166666666667,41.0833333333333,41.0833333333333,41.0833333333333,41.0833333333333,40.9166666666667,40.9166666666667,40.9166666666667,40.9166666666667,40.9166666666667,40.75,40.75,40.75,40.75,40.75,40.5833333333333,40.5833333333333,40.5833333333333,40.5833333333333,40.5833333333333,40.5833333333333,40.5833333333333,40.5833333333333,40.5833333333333,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.4166666666667,40.25,40.25,40.25,40.25,40.25,40.25,40.25,40.25,40.25,40.25,40.25,40.25],[-8.08333333333333,-7.91666666666667,-7.75,-7.58333333333333,-7.41666666666667,-8.25,-8.08333333333333,-7.91666666666667,-7.75,-7.58333333333333,-7.41666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-6.25,-5.41666666666667,-5.25,-8.41666666666667,-8.25,-8.08333333333333,-7.91666666666667,-7.75,-7.58333333333333,-7.41666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-6.25,-6.08333333333333,-5.91666666666667,-5.41666666666667,-5.25,-5.08333333333333,-4.91666666666667,-4.75,-8.41666666666667,-8.25,-8.08333333333333,-7.91666666666667,-7.75,-7.41666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-6.25,-6.08333333333333,-5.91666666666667,-5.75,-5.58333333333333,-5.41666666666667,-5.25,-5.08333333333333,-4.91666666666667,-4.75,-4.58333333333333,-8.25,-8.08333333333333,-7.91666666666667,-7.75,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-6.25,-6.08333333333333,-5.91666666666667,-5.75,-5.58333333333333,-5.41666666666667,-5.25,-5.08333333333333,-4.91666666666667,-4.75,-4.58333333333333,-9.25,-9.08333333333333,-8.91666666666667,-8.25,-8.08333333333333,-7.91666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-6.25,-6.08333333333333,-5.91666666666667,-5.75,-5.58333333333333,-5.41666666666667,-5.25,-5.08333333333333,-4.91666666666667,-4.75,-4.58333333333333,-4.41666666666667,-9.08333333333333,-8.91666666666667,-7.41666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-6.25,-6.08333333333333,-5.58333333333333,-5.41666666666667,-0.0833333333333339,0.0833333333333321,0.25,-7.41666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-2.58333333333333,-2.41666666666667,-0.25,-0.0833333333333339,0.0833333333333321,0.25,-7.41666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-6.25,-2.58333333333333,-2.41666666666667,-2.25,-0.0833333333333339,-7.58333333333333,-7.41666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-6.25,-7.58333333333333,-7.41666666666667,-7.25,-7.08333333333333,-6.91666666666667,-6.75,-6.58333333333333,-6.41666666666667,-7.08333333333333,-6.91666666666667,-3.91666666666667,-3.75,-3.58333333333333,-3.41666666666667,-4.25,-4.08333333333333,-3.91666666666667,-3.75,-3.58333333333333,-4.25,-4.08333333333333,-3.91666666666667,-3.75,-3.58333333333333,-6.25,-6.08333333333333,-5.91666666666667,-4.91666666666667,-4.75,-4.25,-4.08333333333333,-3.91666666666667,-3.75,-7.91666666666667,-7.75,-7.58333333333333,-7.41666666666667,-6.08333333333333,-5.91666666666667,-5.75,-5.58333333333333,-5.25,-5.08333333333333,-4.91666666666667,-4.75,-4.58333333333333,-4.41666666666667,-7.91666666666667,-7.75,-7.58333333333333,-5.91666666666667,-5.75,-5.58333333333333,-5.25,-5.08333333333333,-4.91666666666667,-4.75,-4.58333333333333,-4.41666666666667],8,null,"Background points",{"interactive":true,"className":"","stroke":false,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"red","fillOpacity":0.5},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[43.06957,43.025312,40.388521,40.309587,43.789352,42.910697,43.495722,40.393618,43.353098,40.341695,40.365428,40.363166,42.57,43.299999,42.93,41.939999,42.669998,43.290001,42.830002,43.110001,43.09,42.200001,42.299999,42.84,43.48,43.279999,43.470001,43.560001,43.75,42.84,43.66,43.02,43.02,43.200001,43.209999,42.119999,43.110001,43.099998,42.919998,42.299999,42.290001,42.209999,43,42.57,42.75,43.48,43,43.48,43.310001,43.380001,43.200001,43.09,43.099998,43.189999,43.09,42.200001,42.75,42.75,43.380001,43.389999,43.189999,43.18,42.84,43.66,43.299999,43.200001,43.470001,43.290001,43.009998,42.919998,43.200001,43.099998,43,42.389999,43.48,43.029999,43.290001,43.380001,43.110001,42.93,43.110001,43.009998,43.110001,43.02,43.57,43.75,43.470001,43.389999,43.099998,42.66,43.75,43.290001,43.470001,43.009998,42.290001,43.02,43.18,43.200001,43.290001,43.200001,42.220001,43.110001,43.66,42.860001,42.66,42.220001,43.470001,43.66,43.389999,43.310001,43.110001,42.919998,43.02,42.119999,43.48,43.290001,43.02,43.48,43.57,43.02,42.830002,43.200001,43,43.380001,43.029999,43.75,43.66,43.48,43.290001,43.400002,42.84,42.209999,43.200001,43.470001,43.139999,43.150002,42.880001,42.849998,43.18,42.110001,42.259998,42.82,42.77,42.889999,42.27,42.59,42.83,41.94,43.56,43.11,42.2,42.64,43.2,41.05,43.19,43.2,43.01,40.6,41.05,40.87,40.69,42.92,43.39,43.38,43.11,43.31,42.21,43.31,42.29,42.83,40.78,43.3,43.39,40.3,40.78,43.29,43.39,43.01,43.21,42.2,43.1,42.92,43.57,43.13,42.92,40.31,40.41,43.29,43.3,43.11,42.3,43.2,40.96,43.1,43.09,42.92,43.03,43.2,42.22,43.46,43.1,43.11,40.69,43.38,42.22,42.3,43.2,42.12,43.47,43.47,43.01,43.4,43.2,40.78,40.32,43.29,43.29,42.39,40.5,43.01,43.56,43.47,43.38,43.21,40.32,43.29,43.38,43.1,42.21,40.32,43.19,43.47,43.19,42.99,43.11,42.92,40.57,42.29,43.11,40.78,42.86,43.29,42.459999,43.03,40.87,42.12,43.38,43.27,42.67,43.2,43.38,43.11,43.2,40.96,43.47,43.28,42.94,43.29,42.99,43.47],[-5.171215,-6.036635,-7.679727,-7.790437,-7.47334,-6.575039,-5.132756,-7.787378,-4.941888,-7.621731,-7.645674,-7.642539,-6.99,-7.95,-7.1,-7.01,-7.35,-4.79,-5.51,-5.03,-6.24,-6.52,-6.76,-6.98,-7.45,-6.6,-6.71,-6.96,-8.07,-6.74,-8.07,-7.1,-4.78,-7.09,-8.08,-6.76,-4.78,-5.52,-6.61,-7.24,-6.39,-7.24,-6.24,-7.11,-6.74,-7.33,-5.76,-8.07,-8.32,-6.84,-4.78,-6.11,-6.36,-5.52,-5.89,-6.64,-6.86,-6.98,-7.21,-8.07,-5.4,-6.11,-6.86,-7.82,-8.08,-6.72,-6.84,-6.97,-5.39,-5.39,-5.28,-6.48,-5.88,-6.87,-7.95,-8.08,-7.21,-6.96,-4.91,-6.86,-7.09,-6.61,-6.73,-6.98,-7.82,-7.94,-6.96,-7.33,-5.4,-6.99,-7.7,-4.91,-5.29,-5.52,-6.63,-5.15,-5.89,-6.97,-7.09,-4.91,-7.49,-5.15,-7.95,-9.06,-7.11,-7.36,-5.16,-7.7,-7.95,-8.2,-5.27,-5.63,-6.85,-6.88,-7.7,-5.03,-6.73,-7.83,-7.95,-5.27,-6.61,-5.03,-6.12,-7.09,-7.96,-7.82,-7.57,-7.58,-6.72,-8.2,-7.1,-6.76,-6.85,-7.08,-5.82,-4.91,-6.43,-6.19,-6.55,-6.66,-6.44,-6.73,-6.62,-5.53,-6.58,-0.02,-5.51,-7.01,-7.2,-6.73,-6.52,0.13,-5.03,-3.77,-5.4,-6.72,-6.61,-4.12,-3.65,-4.01,-3.89,-6.61,-7.33,-7.09,-5.15,-8.2,-6.76,-8.32,-6.63,-6.61,-3.89,-7.95,-8.07,-5.77,-3.77,-5.03,-7.95,-6.36,-8.08,-6.64,-6.48,-6.37,-7.58,-6.01,-5.51,-5.65,-5.18,-6.72,-8.08,-7.09,-7.24,-4.78,-3.77,-5.52,-5.97,-5.39,-7.96,-7.09,-7.49,-6.34,-6.36,-5.27,-4.01,-5.28,-7.36,-6.76,-6.97,-6.76,-6.71,-7.08,-5.39,-8.2,-4.91,-4.01,-4.82,-4.91,-4.79,-6.87,-4.83,-5.52,-6.96,-6.84,-5.16,-4.66,-4.59,-6.97,-6.96,-5.4,-7.24,-4.71,-5.52,-6.96,-6.35,-5.97,-5.03,-5.63,-6.11,-6.39,-4.78,-4.13,-9.06,-7.09,-2.45,-8.08,-3.89,-6.88,-6.84,-5.99,-7.35,-6.85,-7.21,-4.91,-5.28,-3.89,-5.29,-6.6,-4.65,-7.21,-6.02,-5.16],8,null,"Occurrence points",{"interactive":true,"className":"","stroke":false,"color":"green","weight":5,"opacity":0.5,"fill":true,"fillColor":"green","fillOpacity":0.5},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLayersControl","args":[[],["Base map","Range","Background points","Occurrence points"],{"collapsed":false,"autoZIndex":true,"position":"bottomleft","position.1":"topright"}]},{"method":"addLegend","args":[{"colors":["black","red","green"],"labels":["Range raster","Background points","Occurrence points"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"bottomright","type":"unknown","title":null,"extra":null,"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[39,48],"lng":[-9.99999999999999,17]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

However, ENMTools also contains some sample data. It contains an
enmtools.clade object called “iberolacerta.clade”, which holds several
enmtools.species objects including an already-built version of
Iberolacerta monticola. It also has some low-resolution Worldclim data
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
    ## <environment: 0x7fd4aeb66740>
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
    ## 8     -7.787378   40.39362    137    1143    247          1
    ## 9     -4.941888   43.35310    128     843    194          1
    ## 11    -7.645674   40.36543    101    1514    229          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6227  -0.7984  -0.4913   0.8336   2.2163  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  9.188e+00  2.248e+00   4.087 4.38e-05 ***
    ## bio1        -3.510e-02  6.549e-03  -5.359 8.35e-08 ***
    ## bio12       -9.941e-05  7.366e-04  -0.135    0.893    
    ## bio7        -2.218e-02  5.069e-03  -4.375 1.21e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.70  on 719  degrees of freedom
    ## Residual deviance: 510.24  on 716  degrees of freedom
    ## AIC: 250.48
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7298678 
    ## cor            : 0.3143133 
    ## max TPR+TNR at : 0.0741665 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.4215101 
    ## cor            : -0.01637437 
    ## max TPR+TNR at : 0.4981737 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.7282903 
    ## cor            : 0.1813117 
    ## max TPR+TNR at : 0.2438604 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.4145173 
    ## cor            : -0.01005591 
    ## max TPR+TNR at : 0.3935572 
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
    ## values     : 0.02466352, 0.996299  (min, max)
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
    ## <environment: 0x7fd4afcc37b8>
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
    ## 7     -5.132756   43.49572    133     822    190          1
    ## 10    -7.621731   40.34170    101    1514    229          1
    ## 14    -7.950000   43.30000    120    1200    194          1
    ## 15    -7.100000   42.93000    115     935    234          1
    ## 16    -7.010000   41.94000    110     931    265          1
    ## 17    -7.350000   42.67000    121     995    231          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6706  -0.7929  -0.4816   0.8211   2.2545  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) 10.6723716  2.3735632   4.496 6.91e-06 ***
    ## bio1        -0.0393469  0.0068540  -5.741 9.43e-09 ***
    ## bio12       -0.0005188  0.0007709  -0.673    0.501    
    ## bio7        -0.0249437  0.0053198  -4.689 2.75e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.70  on 719  degrees of freedom
    ## Residual deviance: 505.73  on 716  degrees of freedom
    ## AIC: 246.13
    ## 
    ## Number of Fisher Scoring iterations: 4
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7371544 
    ## cor            : 0.3219626 
    ## max TPR+TNR at : 0.06642673 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.4213096 
    ## cor            : -0.01428931 
    ## max TPR+TNR at : 0.4000277 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.7013597 
    ## cor            : 0.1611015 
    ## max TPR+TNR at : 0.09236149 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.3996615 
    ## cor            : -0.01321132 
    ## max TPR+TNR at : 0.4092138 
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
    ## values     : 0.01859726, 0.9977367  (min, max)
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
    ## <environment: 0x7fd4af8ef798>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##       Longitude   Latitude   bio1   bio12   bio7   presence
    ## ---  ----------  ---------  -----  ------  -----  ---------
    ## 2     -6.036635   43.02531     76    1012    246          1
    ## 3     -7.679727   40.38852    137    1143    247          1
    ## 4     -7.790437   40.30959    129    1231    242          1
    ## 6     -6.575039   42.91070     84    1012    247          1
    ## 7     -5.132756   43.49572    133     822    190          1
    ## 8     -7.787378   40.39362    137    1143    247          1
    ## 9     -4.941888   43.35310    128     843    194          1
    ## 10    -7.621731   40.34170    101    1514    229          1
    ## 12    -7.642539   40.36317    101    1514    229          1
    ## 14    -7.950000   43.30000    120    1200    194          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3110  -0.7090  -0.4031   0.6543   2.7021  
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -1.030      0.424  -2.429  0.01513 *  
    ## poly(bio1, 2)1                  -29.649      5.623  -5.273 1.34e-07 ***
    ## poly(bio1, 2)2                  -30.012      6.210  -4.832 1.35e-06 ***
    ## poly(bio7, 2)1                  -12.916     12.959  -0.997  0.31889    
    ## poly(bio7, 2)2                    6.181     10.482   0.590  0.55540    
    ## poly(bio12, 2)1                  35.430     12.772   2.774  0.00553 ** 
    ## poly(bio12, 2)2                 -12.824      9.844  -1.303  0.19268    
    ## poly(bio7, 2)1:poly(bio12, 2)1 -339.663    450.870  -0.753  0.45124    
    ## poly(bio7, 2)2:poly(bio12, 2)1 -149.519    282.522  -0.529  0.59665    
    ## poly(bio7, 2)1:poly(bio12, 2)2  546.379    275.136   1.986  0.04705 *  
    ## poly(bio7, 2)2:poly(bio12, 2)2  122.960    163.468   0.752  0.45193    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 576.70  on 719  degrees of freedom
    ## Residual deviance: 457.74  on 709  degrees of freedom
    ## AIC: 245.11
    ## 
    ## Number of Fisher Scoring iterations: 5
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 512 
    ## AUC            : 0.7874944 
    ## cor            : 0.4123488 
    ## max TPR+TNR at : 0.1346912 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 208 
    ## n absences     : 10000 
    ## AUC            : 0.7483029 
    ## cor            : 0.12484 
    ## max TPR+TNR at : 0.3106917 
    ## 
    ## 
    ## Proportion of data wittheld for model fitting:  0.2
    ## 
    ## Model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 512 
    ## AUC            : 0.7932317 
    ## cor            : 0.2667824 
    ## max TPR+TNR at : 0.13692 
    ## 
    ## 
    ## Environment space model fit (test data):  class          : ModelEvaluation 
    ## n presences    : 52 
    ## n absences     : 10000 
    ## AUC            : 0.7511808 
    ## cor            : 0.0632771 
    ## max TPR+TNR at : 0.3110502 
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
    ## values     : 2.220446e-16, 0.9704931  (min, max)
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
    ## [1] 0.9485595
    ## 
    ## $B2
    ## [1] 0.5582789

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
    ## [1] 0.6830795
    ## 
    ## $I
    ## [1] 0.8935148
    ## 
    ## $rank.cor
    ## [1] 0.2862231

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
    ## [1] 0.9479397
    ## 
    ## $env.I
    ## [1] 0.9961393
    ## 
    ## $env.cor
    ## [1] 0.994656
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
    ## empirical    0.3244487   0.5929443   0.1049357   0.3702728   0.6173426   0.1382323
    ## rep 1        0.9651263   0.9990052   0.9995625   0.9799195   0.9993926   0.9997259
    ## rep 2        0.9555505   0.9983196   0.9787684   0.9565279   0.9974753   0.9869872
    ## rep 3        0.9408846   0.9963909   0.9657639   0.9385034   0.9932377   0.9725036
    ## rep 4        0.9736969   0.9992617   0.9969009   0.9828253   0.9994548   0.9982245

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
    ## empirical    0.0242365   0.1455829   -0.0450249   0.0139447   0.1112518   0.1605736
    ## rep 1        0.6618239   0.8978238    0.7496649   0.5790826   0.8256691   0.7346644
    ## rep 2        0.6285125   0.8700776    0.6657550   0.5191014   0.7703038   0.6697477
    ## rep 3        0.6646040   0.9016134    0.7690369   0.5561483   0.8094668   0.7669896
    ## rep 4        0.6647238   0.8981147    0.7514702   0.5717497   0.8171230   0.7029688

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
    ## empirical    0.1974048   0.4272913   -0.1020590   0.0775623   0.2645385   0.2206893
    ## rep 1        0.9509940   0.9970105    0.6677082   0.8482221   0.9547292   0.9274305
    ## rep 2        0.9533348   0.9981036    0.5457460   0.8972314   0.9849384   0.9565055
    ## rep 3        0.9589487   0.9976435    0.8141599   0.8326076   0.9537578   0.9599762
    ## rep 4        0.9464994   0.9946195    0.7866542   0.8174494   0.9342328   0.9531094

![](Readme_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Ecospat tests

Using enmtools.species objects also provides a simplified interface to
the niche equivalency and similarity tests (or identity and background
tests, respectively) that were developed by Broennimann et al. (2012).
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
    ## 0.48 0.36

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
    ## 0.48 0.40

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
    ##      0.4      0.4      0.6      0.4      0.4      0.8 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I     rank.cor       env.D       env.I      env.cor
    ## ----------  ----------  ----------  -----------  ----------  ----------  -----------
    ## empirical    0.3244487   0.5929443    0.1049357   0.3680269   0.6156600    0.1351890
    ## rep 1        0.2992174   0.5582095    0.0556068   0.3453155   0.5834033    0.0320804
    ## rep 2        0.4210823   0.6995871    0.2260137   0.4470461   0.6997146    0.3805585
    ## rep 3        0.4908584   0.7820489   -0.0602491   0.5747473   0.7962220    0.0986697
    ## rep 4        0.6451580   0.8801286    0.1971547   0.5496357   0.7546442   -0.3888845

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
    ##      0.6      0.6      0.4      0.4      0.4      0.4 
    ## 
    ## 
    ## Replicates:
    ## 
    ## 
    ##                      D           I     rank.cor       env.D       env.I     env.cor
    ## ----------  ----------  ----------  -----------  ----------  ----------  ----------
    ## empirical    0.0242365   0.1455829   -0.0450249   0.0139659   0.1111037   0.1592336
    ## rep 1        0.0043070   0.0229435   -0.2181012   0.0016077   0.0124834   0.0016175
    ## rep 2        0.7346826   0.8883696    0.8354964   0.5217177   0.7175192   0.5227507
    ## rep 3        0.6877305   0.9032299    0.8283227   0.5599550   0.7976309   0.7247965
    ## rep 4        0.0175362   0.1020455   -0.0212972   0.0362747   0.1607758   0.2115328

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
    ## <environment: 0x7fd4a05c51f8>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##  Longitude   Latitude   bio1   bio12   bio7   presence
    ## ----------  ---------  -----  ------  -----  ---------
    ##   -8.07000   43.75000    134    1045    166          1
    ##   -7.94000   43.75000    131    1061    171          1
    ##   -7.82000   43.75000    135    1006    174          1
    ##   -7.70000   43.75000    135    1006    174          1
    ##   -8.07000   43.66000    135    1048    168          1
    ##   -7.47334   43.78935    140     931    179          1
    ##   -7.95000   43.66000    129    1080    176          1
    ##   -7.82000   43.66000    122    1113    184          1
    ##   -7.70000   43.66000    122    1113    184          1
    ##   -7.57000   43.66000    124    1067    187          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9270  -0.7589  -0.3689   0.7003   1.8964  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) 11.9319736  2.2737260   5.248 1.54e-07 ***
    ## bio1        -0.0472009  0.0068304  -6.910 4.83e-12 ***
    ## bio12        0.0002876  0.0007058   0.407    0.684    
    ## bio7        -0.0292366  0.0051457  -5.682 1.33e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 720.87  on 901  degrees of freedom
    ## Residual deviance: 571.11  on 898  degrees of freedom
    ## AIC: 263.26
    ## 
    ## Number of Fisher Scoring iterations: 5
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 260 
    ## n absences     : 642 
    ## AUC            : 0.7888659 
    ## cor            : 0.4067992 
    ## max TPR+TNR at : 0.09289407 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 260 
    ## n absences     : 10000 
    ## AUC            : 0.4287431 
    ## cor            : -0.001385296 
    ## max TPR+TNR at : 0.5231318 
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
    ## values     : 0.00680771, 0.9996784  (min, max)
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
    ## <environment: 0x7fd4bc693a50>
    ## 
    ## 
    ## Data table (top ten lines): 
    ## 
    ##  Longitude   Latitude   bio1   bio12   bio7   presence
    ## ----------  ---------  -----  ------  -----  ---------
    ##  -4.130000   40.78000     91     555    284          1
    ##  -4.130000   40.78000     91     555    284          1
    ##  -4.130000   40.78000     91     555    284          1
    ##  -3.939233   40.80118     94     568    284          1
    ##  -5.770000   40.39000    110     574    293          1
    ##  -5.770000   40.39000    110     574    293          1
    ##  -4.010000   40.78000     91     555    284          1
    ##  -4.010000   40.78000     91     555    284          1
    ##  -4.010000   40.78000     91     555    284          1
    ##  -5.300000   40.49000     85     625    294          1
    ## 
    ## 
    ## Model:  
    ## Call:
    ## glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
    ##     2)], weights = weights)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.84834  -0.29914  -0.11057  -0.00099   1.53572  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -36.873784  11.770105  -3.133  0.00173 ** 
    ## bio1         -0.112942   0.023773  -4.751 2.03e-06 ***
    ## bio12        -0.001980   0.003807  -0.520  0.60298    
    ## bio7          0.173901   0.040517   4.292 1.77e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 210.72  on 847  degrees of freedom
    ## Residual deviance: 114.23  on 844  degrees of freedom
    ## AIC: 56.593
    ## 
    ## Number of Fisher Scoring iterations: 8
    ## 
    ## 
    ## 
    ## Model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 76 
    ## n absences     : 772 
    ## AUC            : 0.8985206 
    ## cor            : 0.2616533 
    ## max TPR+TNR at : -0.1224394 
    ## 
    ## 
    ## Environment space model fit (training data):  class          : ModelEvaluation 
    ## n presences    : 76 
    ## n absences     : 10000 
    ## AUC            : 0.8395066 
    ## cor            : 0.13004 
    ## max TPR+TNR at : 0.3074197 
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
    ## values     : 2.220446e-16, 0.9998234  (min, max)
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
    ##  Longitude   Latitude
    ## ----------  ---------
    ##  -2.710110   41.68400
    ##  -1.409324   41.75997
    ##  -8.857594   40.97279
    ##  -4.467018   41.56909
    ##  -1.147251   41.22926
    ##  -2.895049   41.91698
    ##  -3.898922   41.12935
    ##  -6.300657   41.73490
    ##  -1.470455   41.06071
    ##  -8.135529   40.90007
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
    ##      0.4      0.4      0.4      0.4      0.4      1.2 
    ## 
    ## Species 1 vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.8      0.8      0.8      0.8      0.8      0.4 
    ## 
    ## Species 2 vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      0.4      0.4      0.4      0.4      0.4      0.4 
    ## 
    ## Outside vs. Ribbon:
    ##        D        I rank.cor    env.D    env.I  env.cor 
    ##      1.2      0.8      0.8      0.8      0.8      0.4 
    ## 
    ## 
    ## Replicates:
    ## 
    ## Species 1 vs. Species 2:
    ##                    D         I    rank.cor      env.D     env.I     env.cor
    ## empirical 0.09563344 0.2773912 -0.13001274 0.07929948 0.1890483  0.04634598
    ## rep 1     0.32571200 0.5975720  0.03199639 0.27229601 0.4292887 -0.04718642
    ## rep 2     0.47175811 0.7359257  0.38463907 0.23842761 0.4301462  0.19806445
    ## rep 3     0.12636491 0.3628144  0.07040730 0.29150743 0.4442903  0.18757337
    ## rep 4     0.36135461 0.6079395 -0.11990183 0.24144320 0.3598227 -0.17244692
    ## 
    ## Species 1 vs. Ribbon:
    ##                    D         I   rank.cor      env.D      env.I     env.cor
    ## empirical 0.41400321 0.6576868  0.1039598 0.15672356 0.27339280 -0.29068847
    ## rep 1     0.48299987 0.7523778  0.4260796 0.30753418 0.54899804  0.28595229
    ## rep 2     0.66396384 0.8688856  0.6258878 0.34499925 0.60591954  0.75017742
    ## rep 3     0.04467014 0.1903442 -0.2432176 0.01834095 0.07495722 -0.09175445
    ## rep 4     0.48880908 0.7574501  0.3656087 0.25863530 0.44563200  0.17655053
    ## 
    ## Species 2 vs. Ribbon:
    ##                   D         I  rank.cor      env.D      env.I    env.cor
    ## empirical 0.2047253 0.3711309 0.1603354 0.03873533 0.07968535 -0.3581003
    ## rep 1     0.7626802 0.9260112 0.8711796 0.63042052 0.83713235  0.8917772
    ## rep 2     0.7125481 0.9034020 0.8560082 0.49378810 0.70586385  0.6052986
    ## rep 3     0.3991656 0.6547037 0.7159358 0.15671369 0.39310553  0.4395030
    ## rep 4     0.7537451 0.8952297 0.7879107 0.64284352 0.84734577  0.8915596
    ## 
    ## Outside vs. Ribbon:
    ##                    D         I   rank.cor      env.D     env.I    env.cor
    ## empirical 0.48649639 0.7278116  0.3056086 0.21041592 0.3481907 -0.2512931
    ## rep 1     0.48570612 0.7544960  0.4168713 0.33584465 0.5910064  0.3358261
    ## rep 2     0.68837081 0.8757448  0.6746798 0.31757621 0.5787816  0.4428331
    ## rep 3     0.06768905 0.2440620 -0.2337182 0.02805506 0.1086315 -0.1088008
    ## rep 4     0.54668863 0.8053557  0.5418124 0.36451456 0.5736560  0.4155024

![](Readme_files/figure-markdown_github/rangebreak_ribbon-1.png)![](Readme_files/figure-markdown_github/rangebreak_ribbon-2.png)![](Readme_files/figure-markdown_github/rangebreak_ribbon-3.png)![](Readme_files/figure-markdown_github/rangebreak_ribbon-4.png)

Note that the output table here has slope, intercept, and intercept
offset.

``` r
rbr.glm$lines.df
```

    ##        slope intercept    offset
    ## 1 -0.2418273  41.70449 0.2572062
    ## 2 -2.8091179  23.13636 0.7454505
    ## 3  0.2438461  41.81856 0.2573253
    ## 4 -1.0289391  37.05143 0.3587056

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

tree.path <- paste(system.file(package="ENMTools"), "/StarBEAST_MCC.species.txt", sep='')

hisp.anoles <- read.nexus(file = tree.path)

keepers <- c("brevirostris", "marron", "caudalis", "websteri", "distichus")

hisp.anoles <- drop.tip(phy = hisp.anoles, tip = hisp.anoles$tip.label[!hisp.anoles$tip.label %in% keepers])
plot(hisp.anoles)
```

![](Readme_files/figure-markdown_github/read_tree-1.png)

So there’s our tree. Now we’re going to grab some environmental data.

``` r
hisp.env <- raster::getData('worldclim', var='bio', res=10)
hisp.env <- raster::crop(hisp.env, extent(-75, -65, 16, 21))

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
    ## brevirostris   brevirostris    TRUE      200        0            present 
    ## caudalis       caudalis        TRUE      26         0            present 
    ## distichus      distichus       TRUE      834        0            present 
    ## marron         marron          TRUE      14         0            present 
    ## websteri       websteri        TRUE      21         0            present

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
    ##       0.07843137       0.07843137

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
    ##        0.5490196        0.7058824

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
    ##        0.3137255        0.3921569

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
