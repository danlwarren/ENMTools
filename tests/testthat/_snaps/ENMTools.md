# rf model objects work

    Code
      print(cyreni.rf, plot = FALSE)
    Output
      
      
      Formula:  presence ~ bio1 + bio9
      
      Data table (top ten lines): 
      
      |         x|        y| bio1| bio2| bio3| bio4| bio5| bio6| bio7| bio8| bio9| bio10| bio11| bio12| bio13| bio14| bio15| bio16| bio17| bio18| bio19| presence|
      |---------:|--------:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|--------:|
      | -5.321760| 40.35238|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|        1|
      | -3.821750| 40.87999|   94|  105|   37| 6255|  262|  -21|  283|   76|  179|   179|    22|   558|    65|    20|    28|   169|    84|    92|   144|        1|
      | -4.014862| 40.73640|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|        1|
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -3.939233| 40.80118|   94|  105|   36| 6244|  263|  -21|  284|   75|  179|   179|    22|   568|    67|    21|    28|   173|    84|    91|   147|        1|
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|        1|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|        1|
      | -4.820000| 40.32000|  136|  112|   36| 6503|  313|    9|  304|  122|  223|   223|    57|   381|    47|     9|    36|   123|    47|    47|   100|        1|
      
      
      Model:                  Length Class  Mode     
      call              3    -none- call     
      type              1    -none- character
      predicted        71    -none- numeric  
      mse             500    -none- numeric  
      rsq             500    -none- numeric  
      oob.times        71    -none- numeric  
      importance        2    -none- numeric  
      importanceSD      0    -none- NULL     
      localImportance   0    -none- NULL     
      proximity         0    -none- NULL     
      ntree             1    -none- numeric  
      mtry              1    -none- numeric  
      forest           11    -none- list     
      coefs             0    -none- NULL     
      y                71    -none- numeric  
      test              0    -none- NULL     
      inbag             0    -none- NULL     
      terms             3    terms  call     
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 11 
      AUC            : 0.9893939 
      cor            : 0.8532519 
      max TPR+TNR at : 0.8052286 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.9260983 
      cor            : 0.1295509 
      max TPR+TNR at : 0.9028071 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 11 
      AUC            : 1 
      cor            : 0.9007339 
      max TPR+TNR at : 0.8052286 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.9661906 
      cor            : 0.07290685 
      max TPR+TNR at : 0.9028071 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :       lyr1 
      min value   : 0.03486667 
      max value   : 1.00000000 
      
      
      Notes:  
      NULL

