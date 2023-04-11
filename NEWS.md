ENMTools 1.x
============


ENMTools 1.1.1
--------------

### Bug Fixes

- Updated Readme file
- Fixes to new maxent interface


ENMTools 1.1.0
--------------

### New features

- Updated the entire package to use terra instead of raster
- Numerous other bug fixes and behind-the-scenes improvements

ENMTools 1.0.7
--------------

### Bug Fixes

-   Fixed issues with background buffer when mask and points didn't overlap
-   Added citation for breadth measurements

ENMTools 1.0.5
--------------

### Bug Fixes

-   Permutations for background tests were selecting presences from both species' ranges, leading to incorrect values for hypothesis tests.  


ENMTools 1.0.4
--------------

### Bug Fixes

-   Raes and ter Steege-style tests were returning incorrect evaluate objects, although plots and p values were correct
-   test.prop = "block" wasn't working correctly

### Enhancements

-   Suppressing maxent startup messages by default
-   Added support for bias layers to modeling functions
-   Added check.env function to homogenize raster stacks so NAs propagate across layers


ENMTools 1.0.3
--------------

### Enhancements

-   Brought up to date for new **spatstat** changes
-   Removed **ppmlasso** until it is brought back on CRAN


ENMTools 1.0.2
--------------

### Enhancements

-   Added a new general-purpose function for making background layers from point data.  It can do both circular buffers and buffered convex hulls, and can return points, a polygon, or a raster.  Converted the existing background buffer functions to just call this one, and will eventually deprecate the single-application functions.
-   Added ability to select which corner you want for "block" validation.
-   Added a function called *multi.variogram* which takes a raster stack, builds a variogram for each layer, and then plots the gamma for each variable as a function of distance, scaled by the maximum gama for that variable.  This allows users to get some idea of the level of spatial autocorrelation in each predictor variable.

ENMTools 1.0.1
--------------

### Bug fixes

-   Minor update to fix an rgdal import that was making CRAN angry

ENMTools 1.0.1
--------------

### Bug fixes

-   Minor update to fix an rgdal import that was making CRAN angry

ENMTools 1.0.1
--------------

### Enhancements

-   Added variable importance tests via interface with the *vip* package
-   Added clamping for the *predict* functions, including plots of where
    clamping is happening
-   Added clamping for model construction functions, with a TRUE/FALSE
    switch defaulting to TRUE
-   Changed naming conventions for *predict* functions so that the
    suitability raster is in the $suitability slot, just as with
    modeling functions
-   Added progress bars for a lot of tests
-   Added “verbose” option for a lot of functions, defaulting to FALSE

### Bug fixes

-   Fixed interactive.plot generic and moved the function to its own
    file to make it easier to extend
-   Temporarily suppressing some warnings coming out of *leaflet* that
    are being produced by the recent *rgdal* changes
-   Fixed background sampling code to resample when necessary
-   Changed *enmtools.ranger* demo code to actually use *ranger* instead
    of *rf*
-   Fixed code for calculating p values for some of the hypothesis
    tests, the old code was getting wrong answers when there were
    repeated values

ENMTools 1.0.0
--------------

-   Initial CRAN release
