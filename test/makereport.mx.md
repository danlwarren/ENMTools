---
title: "makereport.mx"
output: html_document
---

# Summary of ENMTools Maxent object for ahli

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
## Length  Class   Mode 
##      1 MaxEnt     S4
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
## AUC            : 0.7355833 
## cor            : 0.1266313 
## max TPR+TNR at : 0.3782192
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
## AUC            : 0.8945 
## cor            : 0.0921168 
## max TPR+TNR at : 0.5092622
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
## AUC            : 0.5783667 
## cor            : 0.01133437 
## max TPR+TNR at : 0.7337559
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
## AUC            : 0.68165 
## cor            : 0.01131321 
## max TPR+TNR at : 0.5092622
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
