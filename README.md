deeplyr
================

[![](https://img.shields.io/github/languages/code-size/systats/deeplyr.svg)](https://github.com/systats/deeplyr)
[![](https://img.shields.io/github/last-commit/systats/deeplyr.svg)](https://github.com/systats/deeplyr/commits/master)

`deeplyr` provides a standard model interface for some machine learning
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

Other libraries that are yet not fully supporteds

  - `ranger`
  - `randomForest`
  - `sklearn`

Most common prediction tasks
    are

  - **linear**
  - **binary**
  - **multi**
  - *poisson*

<!-- end list -->

``` r
tinytest::test_all()
```

    ## Running test_h2o_glm.R................    0 tests    Running test_h2o_glm.R................    0 tests    Running test_h2o_glm.R................    0 tests    Running test_h2o_glm.R................    0 tests    Running test_h2o_glm.R................    0 tests     Connection successful!
    ## 
    ## R is connected to the H2O cluster: 
    ##     H2O cluster uptime:         3 minutes 36 seconds 
    ##     H2O cluster timezone:       Europe/Berlin 
    ##     H2O data parsing timezone:  UTC 
    ##     H2O cluster version:        3.26.0.2 
    ##     H2O cluster version age:    5 months and 1 day !!! 
    ##     H2O cluster name:           H2O_started_from_R_simonroth_qwr054 
    ##     H2O cluster total nodes:    1 
    ##     H2O cluster total memory:   3.28 GB 
    ##     H2O cluster total cores:    8 
    ##     H2O cluster allowed cores:  8 
    ##     H2O cluster healthy:        TRUE 
    ##     H2O Connection ip:          localhost 
    ##     H2O Connection port:        54321 
    ##     H2O Connection proxy:       NA 
    ##     H2O Internal Security:      FALSE 
    ##     H2O API Extensions:         Amazon S3, XGBoost, Algos, AutoML, Core V3, Core V4 
    ##     R Version:                  R version 3.6.0 (2019-04-26) 
    ## 
    ## Running test_h2o_glm.R................    0 tests    Running test_h2o_glm.R................    0 tests      |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ## 0.742 sec elapsed
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ## Running test_h2o_glm.R................    0 tests      |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ## 0.822 sec elapsed
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ## Running test_h2o_glm.R................    0 tests      |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ## 0.76 sec elapsed
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
    ## Running test_h2o_glm.R................    0 tests

    ## [1] "All ok, 0 results"

``` r
devtools::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 3.6.0 (2019-04-26)
    ##  os       macOS  10.15.1              
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       Europe/Berlin               
    ##  date     2019-12-28                  
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package      * version     date       lib source                          
    ##  assertthat     0.2.1       2019-03-21 [1] CRAN (R 3.6.0)                  
    ##  backports      1.1.5       2019-10-02 [1] CRAN (R 3.6.0)                  
    ##  badger       * 0.0.4       2019-01-08 [1] CRAN (R 3.6.0)                  
    ##  bitops         1.0-6       2013-08-17 [1] CRAN (R 3.6.0)                  
    ##  broom          0.5.3       2019-12-14 [1] CRAN (R 3.6.0)                  
    ##  callr          3.4.0       2019-12-09 [1] CRAN (R 3.6.0)                  
    ##  cellranger     1.1.0       2016-07-27 [1] CRAN (R 3.6.0)                  
    ##  cli            2.0.0       2019-12-09 [1] CRAN (R 3.6.0)                  
    ##  colorspace     1.4-1       2019-03-18 [1] CRAN (R 3.6.0)                  
    ##  crayon         1.3.4       2017-09-16 [1] CRAN (R 3.6.0)                  
    ##  DBI            1.1.0       2019-12-15 [1] CRAN (R 3.6.0)                  
    ##  dbplyr         1.4.2       2019-06-17 [1] CRAN (R 3.6.0)                  
    ##  deeplyr      * 0.0.0.9000  2019-12-28 [1] local                           
    ##  desc           1.2.0       2018-05-01 [1] CRAN (R 3.6.0)                  
    ##  devtools       2.0.2       2019-04-08 [1] CRAN (R 3.6.0)                  
    ##  digest         0.6.23      2019-11-23 [1] CRAN (R 3.6.0)                  
    ##  dlstats        0.1.0       2017-08-07 [1] CRAN (R 3.6.0)                  
    ##  dplyr        * 0.8.3       2019-07-04 [1] CRAN (R 3.6.0)                  
    ##  evaluate       0.14        2019-05-28 [1] CRAN (R 3.6.0)                  
    ##  fansi          0.4.0       2018-10-05 [1] CRAN (R 3.6.0)                  
    ##  forcats      * 0.4.0       2019-02-17 [1] CRAN (R 3.6.0)                  
    ##  fs             1.3.1       2019-05-06 [1] CRAN (R 3.6.0)                  
    ##  generics       0.0.2       2018-11-29 [1] CRAN (R 3.6.0)                  
    ##  ggplot2      * 3.2.1       2019-08-10 [1] CRAN (R 3.6.0)                  
    ##  glue           1.3.1       2019-03-12 [1] CRAN (R 3.6.0)                  
    ##  gtable         0.3.0       2019-03-25 [1] CRAN (R 3.6.0)                  
    ##  h2o          * 3.26.0.2    2019-08-01 [1] CRAN (R 3.6.0)                  
    ##  haven          2.2.0       2019-11-08 [1] CRAN (R 3.6.0)                  
    ##  hms            0.5.2       2019-10-30 [1] CRAN (R 3.6.0)                  
    ##  htmltools      0.4.0       2019-10-04 [1] CRAN (R 3.6.0)                  
    ##  httr           1.4.1       2019-08-05 [1] CRAN (R 3.6.0)                  
    ##  jsonlite       1.6         2018-12-07 [1] CRAN (R 3.6.0)                  
    ##  knitr          1.26        2019-11-12 [1] CRAN (R 3.6.0)                  
    ##  lattice        0.20-38     2018-11-04 [1] CRAN (R 3.6.0)                  
    ##  lazyeval       0.2.2       2019-03-15 [1] CRAN (R 3.6.0)                  
    ##  lifecycle      0.1.0       2019-08-01 [1] CRAN (R 3.6.0)                  
    ##  lubridate      1.7.4       2018-04-11 [1] CRAN (R 3.6.0)                  
    ##  magrittr       1.5         2014-11-22 [1] CRAN (R 3.6.0)                  
    ##  memoise        1.1.0       2017-04-21 [1] CRAN (R 3.6.0)                  
    ##  Metrics        0.1.4       2018-07-09 [1] CRAN (R 3.6.0)                  
    ##  modelr         0.1.5       2019-08-08 [1] CRAN (R 3.6.0)                  
    ##  munsell        0.5.0       2018-06-12 [1] CRAN (R 3.6.0)                  
    ##  nlme           3.1-139     2019-04-09 [1] CRAN (R 3.6.0)                  
    ##  pacman         0.5.1       2019-03-11 [1] CRAN (R 3.6.0)                  
    ##  pillar         1.4.3       2019-12-20 [1] CRAN (R 3.6.0)                  
    ##  pkgbuild       1.0.3       2019-03-20 [1] CRAN (R 3.6.0)                  
    ##  pkgconfig      2.0.3       2019-09-22 [1] CRAN (R 3.6.0)                  
    ##  pkgload        1.0.2       2018-10-29 [1] CRAN (R 3.6.0)                  
    ##  prettyunits    1.0.2       2015-07-13 [1] CRAN (R 3.6.0)                  
    ##  processx       3.4.1       2019-07-18 [1] CRAN (R 3.6.0)                  
    ##  ps             1.3.0       2018-12-21 [1] CRAN (R 3.6.0)                  
    ##  purrr        * 0.3.3       2019-10-18 [1] CRAN (R 3.6.0)                  
    ##  R6             2.4.1       2019-11-12 [1] CRAN (R 3.6.0)                  
    ##  RColorBrewer   1.1-2       2014-12-07 [1] CRAN (R 3.6.0)                  
    ##  Rcpp           1.0.3       2019-11-08 [1] CRAN (R 3.6.0)                  
    ##  RCurl          1.95-4.12   2019-03-04 [1] CRAN (R 3.6.0)                  
    ##  readr        * 1.3.1       2018-12-21 [1] CRAN (R 3.6.0)                  
    ##  readxl         1.3.1       2019-03-13 [1] CRAN (R 3.6.0)                  
    ##  remotes        2.1.0       2019-06-24 [1] CRAN (R 3.6.0)                  
    ##  reprex         0.3.0       2019-05-16 [1] CRAN (R 3.6.0)                  
    ##  rlang          0.4.2       2019-11-23 [1] CRAN (R 3.6.0)                  
    ##  rmarkdown      2.0         2019-12-12 [1] CRAN (R 3.6.0)                  
    ##  rprojroot      1.3-2       2018-01-03 [1] CRAN (R 3.6.0)                  
    ##  rstudioapi     0.10        2019-03-19 [1] CRAN (R 3.6.0)                  
    ##  rvcheck        0.1.3       2018-12-06 [1] CRAN (R 3.6.0)                  
    ##  rvest          0.3.5       2019-11-08 [1] CRAN (R 3.6.0)                  
    ##  scales         1.1.0       2019-11-18 [1] CRAN (R 3.6.0)                  
    ##  sessioninfo    1.1.1       2018-11-05 [1] CRAN (R 3.6.0)                  
    ##  stringi        1.4.3       2019-03-12 [1] CRAN (R 3.6.0)                  
    ##  stringr      * 1.4.0       2019-02-10 [1] CRAN (R 3.6.0)                  
    ##  testthat       2.2.1       2019-07-25 [1] CRAN (R 3.6.0)                  
    ##  tibble       * 2.1.3       2019-06-06 [1] CRAN (R 3.6.0)                  
    ##  tictoc         1.0         2014-06-17 [1] CRAN (R 3.6.0)                  
    ##  tidyr        * 1.0.0.9000  2019-12-16 [1] Github (tidyverse/tidyr@885d1af)
    ##  tidyselect     0.2.5       2018-10-11 [1] CRAN (R 3.6.0)                  
    ##  tidyverse    * 1.3.0       2019-11-21 [1] CRAN (R 3.6.0)                  
    ##  tinytest       1.1.0       2019-09-26 [1] CRAN (R 3.6.0)                  
    ##  usethis        1.5.0       2019-04-07 [1] CRAN (R 3.6.0)                  
    ##  vctrs          0.2.99.9000 2019-12-27 [1] Github (r-lib/vctrs@fa48942)    
    ##  withr          2.1.2       2018-03-15 [1] CRAN (R 3.6.0)                  
    ##  xfun           0.11        2019-11-12 [1] CRAN (R 3.6.0)                  
    ##  xml2           1.2.2       2019-08-09 [1] CRAN (R 3.6.0)                  
    ##  yaml           2.2.0       2018-07-25 [1] CRAN (R 3.6.0)                  
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
