deeplyr
================

[![](https://img.shields.io/github/languages/code-size/systats/deeplyr.svg)](https://github.com/systats/deeplyr)
[![](https://img.shields.io/github/last-commit/systats/deeplyr.svg)](https://github.com/systats/deeplyr/commits/master)

This is a light-weight model engine mainly for text classification tasks
written as R6class. While Keras, XGBoost or Ranger (RF) provide high
quality learners, `deeplyr` comes with unifying train/test/report
functions for all backends mentioned. This streamlined data/ model
worklfow does
automatically

<!-- * log (thanks to [`trackR`](https://github.com/daroczig/logger)),  -->

<!-- * benchmark (thanks to [`tictoc`]() and [`Metrics`]()) -->

<!-- * optimize over hyperparameter spaces.  -->

<!-- # Other stuff -->

<!-- * a roadmap for improved NLP decisions focussing on different outcomes (fortcomming). -->

<!-- * shinyapp for monitoring model performances. -->

# Options

``` r
# keras::install_keras()
# tensorflow::install_tensorflow()
# py_config()
# use_python("/anaconda3/lib/python3.7")
# py_install("tensorflow")
# Sys.getenv("RETICULATE_PYTHON")
```

``` bash
echo "Sys.setenv(RETICULATE_PYTHON = '~/usr/local/bin/python3')" > ~/.Rprofile
```

``` bash
nano ~/.Rprofile
```
