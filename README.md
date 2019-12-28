deeplyr
================

[![](https://img.shields.io/github/languages/code-size/systats/deeplyr.svg)](https://github.com/systats/deeplyr)
[![](https://img.shields.io/github/last-commit/systats/deeplyr.svg)](https://github.com/systats/deeplyr/commits/master)

`deeplyr` provides a standrad model interface for some machine learning
libraries. Internally the package is managed by R6 classes, allowing for
cross-validation or genetic hyper parameter tuning. Every model run can
be saved including meta, params, metrics, model, and preds either as
.rds or .json.

Have already full support (fit, predict, save)

  - `keras`
  - `h2o`
  - `xgboost`

For example

``` r
f1 <- fit_learner(params, data, task = "binary", backend = "xgboost")
f1$save(".. your path ...")
```

What about cross validation?

``` r
f1 <- fit_cv(params, data, task = "binary", backend = "xgboost")
f1$save(".. your path ...")
```

Other libraries that are yet not fully supported are

  - `ranger`
  - `randomForest`
  - `sklearn`

Most common preiction tasks are

  - **linear**
  - **binary**
  - **multi**
  - *poisson*
