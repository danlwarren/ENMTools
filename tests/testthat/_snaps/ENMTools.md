# enmtools.model objects work for core methods

    Code
      print(cyreni.dm, plot = FALSE)
    Output
      
      
      Data table (top ten lines): 
      
      |         x|        y| bio1| bio2| bio3| bio4| bio5| bio6| bio7| bio8| bio9| bio10| bio11| bio12| bio13| bio14| bio15| bio16| bio17| bio18| bio19|
      |---------:|--------:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
      | -5.321760| 40.35238|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|
      | -3.821750| 40.87999|   94|  105|   37| 6255|  262|  -21|  283|   76|  179|   179|    22|   558|    65|    20|    28|   169|    84|    92|   144|
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -3.939233| 40.80118|   94|  105|   36| 6244|  263|  -21|  284|   75|  179|   179|    22|   568|    67|    21|    28|   173|    84|    91|   147|
      | -4.120000| 40.60000|  119|  104|   35| 6458|  288|   -3|  291|   78|  204|   206|    41|   452|    53|    14|    32|   144|    63|    66|   117|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|
      | -4.820000| 40.32000|  136|  112|   36| 6503|  313|    9|  304|  122|  223|   223|    57|   381|    47|     9|    36|   123|    47|    47|   100|
      | -5.300000| 40.49000|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|
      
      
      Model:  class    : Domain 
      
      variables: bio1 bio2 bio3 bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13 bio14 bio15 bio16 bio17 bio18 bio19 
      
      
      presence points: 60 
         bio1 bio2 bio3 bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13 bio14
      1    85  111   37 6196  262  -32  294   47  169   170    14   625    74    19
      2    94  105   37 6255  262  -21  283   76  179   179    22   558    65    20
      3    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      4   101  106   36 6259  271  -19  290  118  185   186    27   487    63    18
      5    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      6    94  105   36 6244  263  -21  284   75  179   179    22   568    67    21
      7   119  104   35 6458  288   -3  291   78  204   206    41   452    53    14
      8   110  111   37 6278  285   -8  293   42  193   195    35   574    69    13
      9   136  112   36 6503  313    9  304  122  223   223    57   381    47     9
      10   85  111   37 6196  262  -32  294   47  169   170    14   625    74    19
         bio15 bio16 bio17 bio18 bio19
      1     33   197    80    85   176
      2     28   169    84    92   144
      3     28   173    84    92   139
      4     31   155    74    78   117
      5     28   173    84    92   139
      6     28   173    84    91   147
      7     32   144    63    66   117
      8     38   196    62    64   179
      9     36   123    47    47   100
      10    33   197    80    85   176
        (... ...  ...)
      
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 1000 
      AUC            : 0.7460583 
      cor            : 0.1704147 
      max TPR+TNR at : 0.5351941 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.8853717 
      cor            : 0.1475601 
      max TPR+TNR at : 0.4919228 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 1000 
      AUC            : 0.6989375 
      cor            : 0.06950052 
      max TPR+TNR at : 0.504802 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.84355 
      cor            : 0.0640956 
      max TPR+TNR at : 0.4998238 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :      lyr1 
      min value   : 0.1333333 
      max value   : 0.6849673 
      
      
      Notes:  
      NULL
      NULL

---

    Code
      print(cyreni.bc, plot = FALSE)
    Output
      
      
      Data table (top ten lines): 
      
      |         x|        y| bio1| bio2| bio3| bio4| bio5| bio6| bio7| bio8| bio9| bio10| bio11| bio12| bio13| bio14| bio15| bio16| bio17| bio18| bio19|
      |---------:|--------:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
      | -5.321760| 40.35238|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|
      | -4.014862| 40.73640|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -3.939233| 40.80118|   94|  105|   36| 6244|  263|  -21|  284|   75|  179|   179|    22|   568|    67|    21|    28|   173|    84|    91|   147|
      | -4.120000| 40.60000|  119|  104|   35| 6458|  288|   -3|  291|   78|  204|   206|    41|   452|    53|    14|    32|   144|    63|    66|   117|
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|
      | -4.820000| 40.32000|  136|  112|   36| 6503|  313|    9|  304|  122|  223|   223|    57|   381|    47|     9|    36|   123|    47|    47|   100|
      | -4.130000| 40.78000|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      
      
      Model:  class    : Bioclim 
      
      variables: bio1 bio2 bio3 bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13 bio14 bio15 bio16 bio17 bio18 bio19 
      
      
      presence points: 60 
         bio1 bio2 bio3 bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13 bio14
      1    85  111   37 6196  262  -32  294   47  169   170    14   625    74    19
      2    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      3    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      4   101  106   36 6259  271  -19  290  118  185   186    27   487    63    18
      5    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      6    94  105   36 6244  263  -21  284   75  179   179    22   568    67    21
      7   119  104   35 6458  288   -3  291   78  204   206    41   452    53    14
      8    89  111   37 6278  267  -29  296   22  174   175    16   646    74    19
      9   136  112   36 6503  313    9  304  122  223   223    57   381    47     9
      10   91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
         bio15 bio16 bio17 bio18 bio19
      1     33   197    80    85   176
      2     28   173    84    92   139
      3     28   173    84    92   139
      4     31   155    74    78   117
      5     28   173    84    92   139
      6     28   173    84    91   147
      7     32   144    63    66   117
      8     34   206    80    84   187
      9     36   123    47    47   100
      10    28   173    84    92   139
        (... ...  ...)
      
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 1000 
      AUC            : 0.8039083 
      cor            : 0.3713543 
      max TPR+TNR at : 0.06656667 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.6166633 
      cor            : 0.1473155 
      max TPR+TNR at : 0.1499 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 1000 
      AUC            : 0.7925313 
      cor            : 0.2059825 
      max TPR+TNR at : 0.06656667 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.5747688 
      cor            : 0.07037979 
      max TPR+TNR at : 0.1999 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :       lyr1 
      min value   : 0.01666667 
      max value   : 0.33333333 
      
      
      Notes:  
      NULL
      NULL

---

    Code
      print(cyreni.glm, plot = FALSE)
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
      | -4.120000| 40.60000|  119|  104|   35| 6458|  288|   -3|  291|   78|  204|   206|    41|   452|    53|    14|    32|   144|    63|    66|   117|        1|
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|        1|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|        1|
      
      
      Model:  
      Call:
      glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
          2)], weights = weights)
      
      Deviance Residuals: 
           Min        1Q    Median        3Q       Max  
      -0.54673  -0.26126  -0.18995  -0.09819   1.84341  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept) -44.0085    13.4862  -3.263  0.00110 ** 
      bio1         -0.7568     0.1884  -4.016 5.92e-05 ***
      bio9          0.6522     0.1747   3.732  0.00019 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 166.36  on 1059  degrees of freedom
      Residual deviance: 113.52  on 1057  degrees of freedom
      AIC: 62.145
      
      Number of Fisher Scoring iterations: 5
      
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 1000 
      AUC            : 0.8426583 
      cor            : 0.2889692 
      max TPR+TNR at : 1.044451 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.538495 
      cor            : 0.03413958 
      max TPR+TNR at : 0.1827521 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 1000 
      AUC            : 0.8865312 
      cor            : 0.1722163 
      max TPR+TNR at : 0.07189053 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.5435562 
      cor            : 0.02127246 
      max TPR+TNR at : 0.2172198 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :         lyr1 
      min value   : 2.220446e-16 
      max value   : 9.955854e-01 
      
      
      Notes:  
      NULL

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
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|        1|
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -3.939233| 40.80118|   94|  105|   36| 6244|  263|  -21|  284|   75|  179|   179|    22|   568|    67|    21|    28|   173|    84|    91|   147|        1|
      | -4.120000| 40.60000|  119|  104|   35| 6458|  288|   -3|  291|   78|  204|   206|    41|   452|    53|    14|    32|   144|    63|    66|   117|        1|
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|        1|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|        1|
      | -4.820000| 40.32000|  136|  112|   36| 6503|  313|    9|  304|  122|  223|   223|    57|   381|    47|     9|    36|   123|    47|    47|   100|        1|
      
      
      Model:                  Length Class  Mode     
      call               3   -none- call     
      type               1   -none- character
      predicted       1060   -none- numeric  
      mse              500   -none- numeric  
      rsq              500   -none- numeric  
      oob.times       1060   -none- numeric  
      importance         2   -none- numeric  
      importanceSD       0   -none- NULL     
      localImportance    0   -none- NULL     
      proximity          0   -none- NULL     
      ntree              1   -none- numeric  
      mtry               1   -none- numeric  
      forest            11   -none- list     
      coefs              0   -none- NULL     
      y               1060   -none- numeric  
      test               0   -none- NULL     
      inbag              0   -none- NULL     
      terms              3   terms  call     
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 1000 
      AUC            : 0.943625 
      cor            : 0.5654408 
      max TPR+TNR at : 0.06091695 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.8385067 
      cor            : 0.1454036 
      max TPR+TNR at : 0.145345 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 1000 
      AUC            : 0.8743125 
      cor            : 0.3014653 
      max TPR+TNR at : 0.0446395 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.7491094 
      cor            : 0.05205068 
      max TPR+TNR at : 0.1302884 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :          lyr1 
      min value   : -9.983681e-17 
      max value   :  9.290816e-01 
      
      
      Notes:  
      NULL

# ranger model objects work

    Code
      print(cyreni.rf.ranger, plot = FALSE)
    Output
      
      
      Formula:  presence ~ bio1 + bio9
      
      Data table (top ten lines): 
      
      |         x|        y| bio1| bio2| bio3| bio4| bio5| bio6| bio7| bio8| bio9| bio10| bio11| bio12| bio13| bio14| bio15| bio16| bio17| bio18| bio19|presence |
      |---------:|--------:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|:--------|
      | -5.321760| 40.35238|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|1        |
      | -3.821750| 40.87999|   94|  105|   37| 6255|  262|  -21|  283|   76|  179|   179|    22|   558|    65|    20|    28|   169|    84|    92|   144|1        |
      | -4.014862| 40.73640|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|1        |
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|1        |
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|1        |
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|1        |
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|1        |
      | -4.820000| 40.32000|  136|  112|   36| 6503|  313|    9|  304|  122|  223|   223|    57|   381|    47|     9|    36|   123|    47|    47|   100|1        |
      | -5.290000| 40.22000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|1        |
      | -5.300000| 40.49000|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|1        |
      
      
      Model:                            Length Class         Mode     
      predictions               2120   -none-        numeric  
      num.trees                    1   -none-        numeric  
      num.independent.variables    1   -none-        numeric  
      mtry                         1   -none-        numeric  
      min.node.size                1   -none-        numeric  
      prediction.error             1   -none-        numeric  
      forest                      10   ranger.forest list     
      splitrule                    1   -none-        character
      treetype                     1   -none-        character
      call                         5   -none-        call     
      importance.mode              1   -none-        character
      num.samples                  1   -none-        numeric  
      replace                      1   -none-        logical  
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 1000 
      AUC            : 0.9664917 
      cor            : 0.6655739 
      max TPR+TNR at : 0.07822126 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.817155 
      cor            : 0.1488637 
      max TPR+TNR at : 0.2381819 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 1000 
      AUC            : 0.9546562 
      cor            : 0.4493217 
      max TPR+TNR at : 0.04287352 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.7898063 
      cor            : 0.0618739 
      max TPR+TNR at : 0.2959997 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :   lyr1 
      min value   : 0.0000 
      max value   : 0.9998 
      
      
      Notes:  
      NULL

# gam model objects work

    Code
      print(cyreni.gam, plot = FALSE)
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
      | -3.939233| 40.80118|   94|  105|   36| 6244|  263|  -21|  284|   75|  179|   179|    22|   568|    67|    21|    28|   173|    84|    91|   147|        1|
      | -4.120000| 40.60000|  119|  104|   35| 6458|  288|   -3|  291|   78|  204|   206|    41|   452|    53|    14|    32|   144|    63|    66|   117|        1|
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|        1|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|        1|
      | -5.300000| 40.49000|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|        1|
      
      
      Model:  
      Family: binomial 
      Link function: logit 
      
      Formula:
      presence ~ bio1 + bio9
      
      Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept) -32.8043    12.8421  -2.554 0.010636 *  
      bio1         -0.6420     0.1818  -3.531 0.000413 ***
      bio9          0.5297     0.1676   3.161 0.001571 ** 
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      
      R-sq.(adj) =  0.339   Deviance explained =   37%
      -REML = 57.099  Scale est. = 1         n = 1060
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 1000 
      AUC            : 0.874075 
      cor            : 0.3172385 
      max TPR+TNR at : -0.04994773 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.5460583 
      cor            : 0.03906898 
      max TPR+TNR at : 0.1201071 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 1000 
      AUC            : 0.830125 
      cor            : 0.1568637 
      max TPR+TNR at : 1.216947 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.5518063 
      cor            : 0.01642439 
      max TPR+TNR at : 0.1201071 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :         lyr1 
      min value   : 2.220446e-16 
      max value   : 9.801562e-01 
      
      
      Notes:  
      NULL
      NULL

