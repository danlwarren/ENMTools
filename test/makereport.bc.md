---
title: "makereport.bc"
output: html_document
---

# Summary of ENMTools Bioclim object for ahli

## Spatial prediction
![plot of chunk plot-suitability](figure/plot-suitability-1.png)
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Model
![plot of chunk response-plots](figure/response-plots-1.png)

```
##  Length   Class    Mode 
##       1 Bioclim      S4
```
<br>
<br>
<br>
<br>
<br>
<br>

## Evaluation
### Geographic space
<img src="figure/eval-geo-train-1.png" title="plot of chunk eval-geo-train" alt="plot of chunk eval-geo-train" style="display: block; margin: auto;" />

```
## class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 1000 
## AUC            : 0.66225 
## cor            : 0.03620842 
## max TPR+TNR at : 0.08323333
```
<br>
<br>

```
## 
## 
## Proportion of data wittheld for model testing:
```

```
## [1] 0.2
```

<img src="figure/eval-geo-test-1.png" title="plot of chunk eval-geo-test" alt="plot of chunk eval-geo-test" style="display: block; margin: auto;" />

```
## class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 1000 
## AUC            : 0.54775 
## cor            : 0.02751034 
## max TPR+TNR at : 0.6665667
```
<br>
<br>
<br>
<br>

### Environment space
<img src="figure/eval-env-train-1.png" title="plot of chunk eval-env-train" alt="plot of chunk eval-env-train" style="display: block; margin: auto;" />

```
## class          : ModelEvaluation 
## n presences    : 12 
## n absences     : 10000 
## AUC            : 0.7898083 
## cor            : 0.0348072 
## max TPR+TNR at : 0.08323333
```
<br>

```
## 
## 
## Proportion of data wittheld for model testing:
```

```
## [1] 0.2
```

<img src="figure/eval-env-test-1.png" title="plot of chunk eval-env-test" alt="plot of chunk eval-env-test" style="display: block; margin: auto;" />

```
## class          : ModelEvaluation 
## n presences    : 4 
## n absences     : 10000 
## AUC            : 0.6356 
## cor            : 0.02231031 
## max TPR+TNR at : 0.6665667
```
<br>
<br>
<br>
<br>


# Notes

```
## [1] "None"
```
<br>
<br>
<br>
<br>

# Citations
Warren, D.L. (2016) Package ‘enmtools’. Available online at: https://github.com/danlwarren/ENMTools

Hijmans, R.J, Phillips, S., Leathwick, J. and Elith, J. (2011), Package ‘dismo’. Available online at: http://cran.r-project.org/web/packages/dismo/index.html.
