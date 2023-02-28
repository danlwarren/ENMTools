# enmtools.model objects work for core methods

    Code
      print(cyreni.dm, plot = FALSE)
    Output
      
      
      Data table (top ten lines): 
      
      |         x|        y| bio1| bio2| bio3| bio4| bio5| bio6| bio7| bio8| bio9| bio10| bio11| bio12| bio13| bio14| bio15| bio16| bio17| bio18| bio19|
      |---------:|--------:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -3.939233| 40.80118|   94|  105|   36| 6244|  263|  -21|  284|   75|  179|   179|    22|   568|    67|    21|    28|   173|    84|    91|   147|
      | -4.120000| 40.60000|  119|  104|   35| 6458|  288|   -3|  291|   78|  204|   206|    41|   452|    53|    14|    32|   144|    63|    66|   117|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|
      | -4.820000| 40.32000|  136|  112|   36| 6503|  313|    9|  304|  122|  223|   223|    57|   381|    47|     9|    36|   123|    47|    47|   100|
      | -5.290000| 40.22000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|
      | -5.300000| 40.49000|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|
      | -4.130000| 40.78000|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      
      
      Model:  class    : Domain 
      
      variables: bio1 bio2 bio3 bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13 bio14 bio15 bio16 bio17 bio18 bio19 
      
      
      presence points: 60 
         bio1 bio2 bio3 bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13 bio14
      1    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      2   101  106   36 6259  271  -19  290  118  185   186    27   487    63    18
      3    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      4    94  105   36 6244  263  -21  284   75  179   179    22   568    67    21
      5   119  104   35 6458  288   -3  291   78  204   206    41   452    53    14
      6   110  111   37 6278  285   -8  293   42  193   195    35   574    69    13
      7   136  112   36 6503  313    9  304  122  223   223    57   381    47     9
      8    89  111   37 6278  267  -29  296   22  174   175    16   646    74    19
      9    85  111   37 6196  262  -32  294   47  169   170    14   625    74    19
      10   91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
         bio15 bio16 bio17 bio18 bio19
      1     28   173    84    92   139
      2     31   155    74    78   117
      3     28   173    84    92   139
      4     28   173    84    91   147
      5     32   144    63    66   117
      6     38   196    62    64   179
      7     36   123    47    47   100
      8     34   206    80    84   187
      9     33   197    80    85   176
      10    28   173    84    92   139
        (... ...  ...)
      
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 622 
      AUC            : 0.9153001 
      cor            : 0.4909736 
      max TPR+TNR at : 0.4790667 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.86497 
      cor            : 0.1294102 
      max TPR+TNR at : 0.4791209 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 622 
      AUC            : 0.9508139 
      cor            : 0.3198432 
      max TPR+TNR at : 0.512863 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.9113813 
      cor            : 0.07611181 
      max TPR+TNR at : 0.512863 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :      lyr1 
      min value   : 0.1583333 
      max value   : 0.6860000 
      
      
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
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      | -3.939233| 40.80118|   94|  105|   36| 6244|  263|  -21|  284|   75|  179|   179|    22|   568|    67|    21|    28|   173|    84|    91|   147|
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|
      | -4.820000| 40.32000|  136|  112|   36| 6503|  313|    9|  304|  122|  223|   223|    57|   381|    47|     9|    36|   123|    47|    47|   100|
      | -5.290000| 40.22000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|
      | -5.300000| 40.49000|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|
      | -4.130000| 40.78000|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|
      
      
      Model:  class    : Bioclim 
      
      variables: bio1 bio2 bio3 bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13 bio14 bio15 bio16 bio17 bio18 bio19 
      
      
      presence points: 60 
         bio1 bio2 bio3 bio4 bio5 bio6 bio7 bio8 bio9 bio10 bio11 bio12 bio13 bio14
      1    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      2   101  106   36 6259  271  -19  290  118  185   186    27   487    63    18
      3    91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
      4    94  105   36 6244  263  -21  284   75  179   179    22   568    67    21
      5    89  111   37 6278  267  -29  296   22  174   175    16   646    74    19
      6   110  111   37 6278  285   -8  293   42  193   195    35   574    69    13
      7   136  112   36 6503  313    9  304  122  223   223    57   381    47     9
      8    89  111   37 6278  267  -29  296   22  174   175    16   646    74    19
      9    85  111   37 6196  262  -32  294   47  169   170    14   625    74    19
      10   91  106   37 6182  260  -24  284  106  175   175    20   555    68    21
         bio15 bio16 bio17 bio18 bio19
      1     28   173    84    92   139
      2     31   155    74    78   117
      3     28   173    84    92   139
      4     28   173    84    91   147
      5     34   206    80    84   187
      6     38   196    62    64   179
      7     36   123    47    47   100
      8     34   206    80    84   187
      9     33   197    80    85   176
      10    28   173    84    92   139
        (... ...  ...)
      
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 610 
      AUC            : 0.9563798 
      cor            : 0.723688 
      max TPR+TNR at : 0.0499 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.6727183 
      cor            : 0.1790666 
      max TPR+TNR at : 0.1001667 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 610 
      AUC            : 0.9245902 
      cor            : 0.7406456 
      max TPR+TNR at : 0.0499 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.7252063 
      cor            : 0.1230439 
      max TPR+TNR at : 0.1499 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :       lyr1 
      min value   : 0.01666667 
      max value   : 0.31666667 
      
      
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
      | -3.821750| 40.87999|   94|  105|   37| 6255|  262|  -21|  283|   76|  179|   179|    22|   558|    65|    20|    28|   169|    84|    92|   144|        1|
      | -4.014862| 40.73640|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|        1|
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -4.120000| 40.60000|  119|  104|   35| 6458|  288|   -3|  291|   78|  204|   206|    41|   452|    53|    14|    32|   144|    63|    66|   117|        1|
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|        1|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|        1|
      | -5.290000| 40.22000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|        1|
      | -5.300000| 40.49000|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|        1|
      
      
      Model:  
      Call:
      glm(formula = f, family = "binomial", data = analysis.df[, -c(1, 
          2)], weights = weights)
      
      Deviance Residuals: 
          Min       1Q   Median       3Q      Max  
      -3.2664   0.4293   0.6502   0.7746   2.0216  
      
      Coefficients:
                  Estimate Std. Error z value Pr(>|z|)  
      (Intercept) -26.2711    15.9484  -1.647   0.0995 .
      bio1         -0.5340     0.2180  -2.450   0.0143 *
      bio9          0.4349     0.2040   2.132   0.0330 *
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      (Dispersion parameter for binomial family taken to be 1)
      
          Null deviance: 166.36  on 78  degrees of freedom
      Residual deviance: 115.55  on 76  degrees of freedom
      AIC: 118.57
      
      Number of Fisher Scoring iterations: 4
      
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 19 
      AUC            : 0.8307018 
      cor            : 0.5753867 
      max TPR+TNR at : -0.1603299 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.549155 
      cor            : 0.03453487 
      max TPR+TNR at : 0.1294723 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 19 
      AUC            : 0.8174342 
      cor            : 0.5465021 
      max TPR+TNR at : 0.9511199 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.5471375 
      cor            : 0.01641175 
      max TPR+TNR at : 0.1294723 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :         lyr1 
      min value   : 2.220446e-16 
      max value   : 9.572602e-01 
      
      
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

# ranger model objects work

    Code
      print(cyreni.rf.ranger, plot = FALSE)
    Output
      
      
      Formula:  presence ~ bio1 + bio9
      
      Data table (top ten lines): 
      
      |         x|        y| bio1| bio2| bio3| bio4| bio5| bio6| bio7| bio8| bio9| bio10| bio11| bio12| bio13| bio14| bio15| bio16| bio17| bio18| bio19|presence |
      |---------:|--------:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|:--------|
      | -3.821750| 40.87999|   94|  105|   37| 6255|  262|  -21|  283|   76|  179|   179|    22|   558|    65|    20|    28|   169|    84|    92|   144|1        |
      | -4.014862| 40.73640|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|1        |
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|1        |
      | -4.179445| 40.64695|  101|  106|   36| 6259|  271|  -19|  290|  118|  185|   186|    27|   487|    63|    18|    31|   155|    74|    78|   117|1        |
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|1        |
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|1        |
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|1        |
      | -5.300000| 40.49000|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|1        |
      | -4.130000| 40.78000|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|1        |
      | -5.070000| 40.50000|   85|  111|   37| 6218|  262|  -33|  295|   46|  169|   170|    13|   605|    74|    19|    32|   189|    81|    86|   164|1        |
      
      
      Model:                            Length Class         Mode     
      predictions               150    -none-        numeric  
      num.trees                   1    -none-        numeric  
      num.independent.variables   1    -none-        numeric  
      mtry                        1    -none-        numeric  
      min.node.size               1    -none-        numeric  
      prediction.error            1    -none-        numeric  
      forest                     10    ranger.forest list     
      splitrule                   1    -none-        character
      treetype                    1    -none-        character
      call                        5    -none-        call     
      importance.mode             1    -none-        character
      num.samples                 1    -none-        numeric  
      replace                     1    -none-        logical  
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 15 
      AUC            : 0.9688889 
      cor            : 0.7920615 
      max TPR+TNR at : 0.6746373 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.9006317 
      cor            : 0.1647657 
      max TPR+TNR at : 0.7489675 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 15 
      AUC            : 0.85625 
      cor            : 0.6497667 
      max TPR+TNR at : 0.6746373 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.6869469 
      cor            : 0.05444408 
      max TPR+TNR at : 0.8131444 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :      lyr1 
      min value   : 0.2226611 
      max value   : 1.0000000 
      
      
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
      | -3.821750| 40.87999|   94|  105|   37| 6255|  262|  -21|  283|   76|  179|   179|    22|   558|    65|    20|    28|   169|    84|    92|   144|        1|
      | -4.014862| 40.73640|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -4.107080| 40.71737|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -4.127770| 40.67088|   91|  106|   37| 6182|  260|  -24|  284|  106|  175|   175|    20|   555|    68|    21|    28|   173|    84|    92|   139|        1|
      | -3.939233| 40.80118|   94|  105|   36| 6244|  263|  -21|  284|   75|  179|   179|    22|   568|    67|    21|    28|   173|    84|    91|   147|        1|
      | -5.180000| 40.32000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|        1|
      | -5.770000| 40.39000|  110|  111|   37| 6278|  285|   -8|  293|   42|  193|   195|    35|   574|    69|    13|    38|   196|    62|    64|   179|        1|
      | -4.820000| 40.32000|  136|  112|   36| 6503|  313|    9|  304|  122|  223|   223|    57|   381|    47|     9|    36|   123|    47|    47|   100|        1|
      | -5.290000| 40.22000|   89|  111|   37| 6278|  267|  -29|  296|   22|  174|   175|    16|   646|    74|    19|    34|   206|    80|    84|   187|        1|
      | -5.300000| 40.49000|   85|  111|   37| 6196|  262|  -32|  294|   47|  169|   170|    14|   625|    74|    19|    33|   197|    80|    85|   176|        1|
      
      
      Model:  
      Family: binomial 
      Link function: logit 
      
      Formula:
      presence ~ bio1 + bio9
      
      Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)   
      (Intercept) -29.4876    14.1476  -2.084  0.03713 * 
      bio1         -0.5998     0.1979  -3.030  0.00244 **
      bio9          0.4888     0.1832   2.668  0.00763 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      
      R-sq.(adj) =  0.364   Deviance explained = 34.6%
      -REML = 59.045  Scale est. = 1         n = 76
      
      
      Model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 16 
      AUC            : 0.8760417 
      cor            : 0.574011 
      max TPR+TNR at : 1.147646 
      
      
      Environment space model fit (training data):  class          : ModelEvaluation 
      n presences    : 60 
      n absences     : 10000 
      AUC            : 0.5325017 
      cor            : 0.03481036 
      max TPR+TNR at : 0.1144978 
      
      
      Proportion of data wittheld for model fitting:  0.2
      
      Model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 16 
      AUC            : 0.8847656 
      cor            : 0.6499723 
      max TPR+TNR at : -1.136655 
      
      
      Environment space model fit (test data):  class          : ModelEvaluation 
      n presences    : 16 
      n absences     : 10000 
      AUC            : 0.53425 
      cor            : 0.01805144 
      max TPR+TNR at : 0.2428535 
      
      
      Suitability:  
      class       : SpatRaster 
      dimensions  : 54, 162, 1  (nrow, ncol, nlyr)
      resolution  : 0.1666667, 0.1666667  (x, y)
      extent      : -10, 17, 39, 48  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
      source(s)   : memory
      name        :         lyr1 
      min value   : 2.220446e-16 
      max value   : 9.725472e-01 
      
      
      Notes:  
      NULL
      NULL

