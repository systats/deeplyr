deeplyr
================

[![](https://img.shields.io/github/languages/code-size/systats/deeplyr.svg)](https://github.com/systats/deeplyr)
[![](https://img.shields.io/github/last-commit/systats/deeplyr.svg)](https://github.com/systats/deeplyr/commits/master)

`deeplyr` provides a standard model interface for some machine learning
libraries. Internally the package is managed by R6 classes and hardhat
allowing for cross-validation or genetic hyper parameter tuning. Every
model run can be saved including meta, params, metrics, process, model,
and preds either as .rds or .json.

Have already full support (fit, predict, save, load)

  - `keras`
  - `h2o`
  - `xgboost`
