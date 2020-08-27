ENMTools 1.x
============

ENMTools 1.0.2
--------------

### Enhancements

-   Added a new general-purpose function for making background layers
    from point data. It can do both circular buffers and buffered convex
    hulls, and can return points, a polygon, or a raster. Converted the
    existing background buffer functions to just call this one, and will
    eventually deprecate the single-application functions.
-   Added ability to select which corner you want for “block”
    validation.

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

-   Added some code to fix a bug with recalibration. Basically
    CalibratR’s call to parallel was tanking on Mac OS, and there’s just
    a little code snippet that needed to be added to the call to make
    that work.
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
