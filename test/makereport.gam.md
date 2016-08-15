---
title: "makereport.gam"
output: html_document
---

# Summary of ENMTools gam object for ahli

## Spatial prediction
![plot of chunk plot-suitability](figure/plot-suitability-1.png)
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Model: presence ~ s(layer.1, k = 10) + s(layer.2, k = 10) + s(layer.3, ,     k = 10) + s(layer.4, k = 10)
![plot of chunk response-plots](figure/response-plots-1.png)

```
## 
## Family: binomial 
## Link function: logit 
## 
## Formula:
## presence ~ s(layer.1, k = 10) + s(layer.2, k = 10) + s(layer.3, 
##     k = 10) + s(layer.4, k = 10)
## 
## Parametric coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -5.9522     0.7399  -8.044 8.68e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##            edf Ref.df Chi.sq p-value   
## s(layer.1)   1  1.000 10.070 0.00151 **
## s(layer.2)   1  1.000  8.637 0.00329 **
## s(layer.3)   1  1.000  0.749 0.38690   
## s(layer.4)   1  1.001  0.200 0.65513   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.00837   Deviance explained = 19.4%
## UBRE = -0.88638  Scale est. = 1         n = 1012
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
## AUC            : 0.8875 
## cor            : 0.1401324 
## max TPR+TNR at : -4.32762
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
## AUC            : 0.90125 
## cor            : 0.08698397 
## max TPR+TNR at : -4.657075
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
## AUC            : 0.462475 
## cor            : -0.02817414 
## max TPR+TNR at : 0.01290443
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
## AUC            : 0.47635 
## cor            : -0.01535844 
## max TPR+TNR at : 0.00939893
```
<br>
<br>
<br>
<br>

## Model fit using gam.check
![plot of chunk model-fit](figure/model-fit-1.png)

```
## 
## Method: UBRE   Optimizer: outer newton
## full convergence after 7 iterations.
## Gradient range [-6.378997e-07,-1.037187e-08]
## (score -0.8863846 & scale 1).
## Hessian positive definite, eigenvalue range [1.037176e-08,6.375097e-07].
## Model rank =  37 / 37 
## 
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##               k'   edf k-index p-value
## s(layer.1) 9.000 1.000   0.956    0.76
## s(layer.2) 9.000 1.000   0.955    0.70
## s(layer.3) 9.000 1.000   0.978    0.98
## s(layer.4) 9.000 1.000   0.963    0.82
```
<br>
<br>
<br>
<br>

# Notes

```
## [1] "No formula was provided, so a GAM formula was built automatically"
```
<br>
<br>
<br>
<br>

# Citations
Warren, D.L. (2016) Package ‘enmtools’. Available online at: https://github.com/danlwarren/ENMTools

Hijmans, R.J, Phillips, S., Leathwick, J. and Elith, J. (2011), Package ‘dismo’. Available online at: http://cran.r-project.org/web/packages/dismo/index.html.
