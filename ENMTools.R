require("adehabitat")
require("raster")
source("raster.sum.R")
source("raster.overlap.R")
source("raster.breadth.R")
source("raster.standardize.R")
source("raster.sample.R")
source("reps.identity.R")
source("reps.background.asc.R")
source("reps.background.csv.R")
source("reps.rangebreak.line.R")
source("reps.rangebreak.blob.R")
source("reps.rangebreak.ribbon.R")

na.value <<- -9999  #Sets na value for ASCII rasters
verbose <<- TRUE    #Turns various warning and diagnostic messages on and off
maxlines <<- 1000   #Sets the maximum number of lines to read from an ASCII file, bigger is faster but more memory