deeplyr
================

[![](https://img.shields.io/github/languages/code-size/systats/deeplyr.svg)](https://github.com/systats/deeplyr)
[![](https://img.shields.io/github/last-commit/systats/deeplyr.svg)](https://github.com/systats/deeplyr/commits/master)

This is a light-weight model engine mainly for text classification tasks
written as R6class. While Keras, XGBoost or Ranger (RF) provide high
quality learners, `deeplyr` comes with unifying train/test/report
functions for all backends mentioned. This streamlined data/ model
worklfow does automatically

  - log (thanks to [`trackR`](https://github.com/daroczig/logger)),
  - benchmark (thanks to [`tictoc`]() and [`Metrics`]())
  - optimize over hyperparameter spaces.

# Other stuff

  - a roadmap for improved NLP decisions focussing on different outcomes
    (fortcomming).
  - shinyapp for monitoring model performances.

## Workflow

<img src ="package_logic/package_logic.png" width = "70%">

# Packages

``` r
pacman::p_load(devtools, R6, tidyverse, purrr, furrr, RSQLite, 
      # GA, dials,
      jsonlite, text2vec, keras, reticulate)

options(scipen = 999)
devtools::load_all()
# devtools::document()
```

# Data

## DTM

``` r
dparams <- list(
  data_path = "sample_dat.Rdata",
  text = "lemma",
  input_dim = 2000,
  seq_len = 150,
  term_count_min = 5,
  doc_proportion_max  = .1
)

# plan(multiprocess, workers = 4)
# options(future.globals.maxSize = 1024^4)
dc <- create_dtm_container(dparams)
```

    ## [1] 10000

``` r
dc$report()
#dc$get_param()
```

## Seqences

``` r
dparams <- list(
  data_path = "sample_dat.Rdata",
  text = "lemma",
  input_dim = 5000,
  seq_len = 150,
  term_count_min = 5,
  doc_proportion_max  = .1 
)

# plan(multiprocess, workers = 4)
# options(future.globals.maxSize = 1024^4)
dc_seq <- create_seq_container(dparams)
```

    ## [1] 10000

``` r
# dc$report()
#dc_seq
```

# Backends

## Ranger (RF)

``` r
#devtools::document()

mparams <- list(
  output_dim = 1,
  target = "binary",
  model_name = "rf"
)

jo <- deeplyr::learner$new("ranger:binary")
jo$set_param(mparams)
jo$set_data(dc)
jo$split(oos = F, val = F)

jo$train()
```

    ## Growing trees.. Progress: 73%. Estimated remaining time: 11 seconds.
    ## 43.5 sec elapsed

``` r
jo$test(dev = F)
s <- jo$report()

jo$preds; jo$perform; jo$results
```

    ## # A tibble: 1,486 x 9
    ##     prob  pred  linear binary category lemma          ids target model_id  
    ##    <dbl> <dbl>   <dbl>  <dbl> <chr>    <chr>        <int>  <dbl> <chr>     
    ##  1 0.352     0 -0.134       0 black    rt @ vrick …  5969      0 5bdeaf85e…
    ##  2 0.348     0  1.74        0 white    rt @ expect…   871      0 5bdeaf85e…
    ##  3 0.261     0  0.578       0 hispanic opinion it …  9016      0 5bdeaf85e…
    ##  4 0.234     0  0.590       0 white    rt @usmclib…   755      0 5bdeaf85e…
    ##  5 0.729     1  0.0283      1 white    rt @ diamon…  3232      1 5bdeaf85e…
    ##  6 0.373     0  2.10        0 white    rt @maggien…  8509      0 5bdeaf85e…
    ##  7 0.481     0  0.513       0 white    excuse i wh…  3023      0 5bdeaf85e…
    ##  8 0.655     1  0.784       0 white    "rt drjohnh…  1202      0 5bdeaf85e…
    ##  9 0.446     0 -0.676       0 black    emotion be …  8336      0 5bdeaf85e…
    ## 10 0.249     0 -0.815       0 black    rt @somemid…  8607      0 5bdeaf85e…
    ## # … with 1,476 more rows

    ## # A tibble: 1 x 7
    ##      ll logloss   auc precision recall fbeta_score accuracy
    ##   <dbl>   <dbl> <dbl>     <dbl>  <dbl>       <dbl>    <dbl>
    ## 1 0.435   0.634 0.644     0.625  0.401       0.488    0.664

    ## # A tibble: 1 x 19
    ##   model_name output_dim target data_id data_name text  input_dim seq_len
    ##   <chr>           <dbl> <chr>  <chr>   <chr>     <chr>     <int>   <dbl>
    ## 1 rf                  1 binary 355760… sample_d… lemma      2000     150
    ## # … with 11 more variables: term_count_min <dbl>,
    ## #   doc_proportion_max <dbl>, total_size <int>, data_path <chr>,
    ## #   train_size <int>, test_size <int>, split_mode <S3: glue>,
    ## #   model_id <chr>, duration <dbl>, metrics <list>, timestamp <chr>

``` r
table(jo$preds$pred, jo$preds$target)
```

    ##    
    ##       0   1
    ##   0 749 356
    ##   1 143 238

``` r
# jo$predict_dtm("make america great again! maga")
```

## XGBoost

``` r
#devtools::document()

mparams <- list(
  output_dim = 1,
  target = "binary"
)

jo <- deeplyr::learner$new("xgboost:binary")
jo$set_param(mparams)
jo$set_data(dc)
jo$split(oos = F, val = F)

jo$train()
```

    ## [1]  train-error:0.367659 
    ## Will train until train_error hasn't improved in 5 rounds.
    ## 
    ## [2]  train-error:0.357564 
    ## [3]  train-error:0.349655 
    ## [4]  train-error:0.340737 
    ## [5]  train-error:0.337708 
    ## [6]  train-error:0.335184 
    ## [7]  train-error:0.331987 
    ## [8]  train-error:0.328285 
    ## [9]  train-error:0.326603 
    ## [10] train-error:0.328454 
    ## [11] train-error:0.326098 
    ## [12] train-error:0.324920 
    ## [13] train-error:0.323910 
    ## [14] train-error:0.322733 
    ## [15] train-error:0.320882 
    ## [16] train-error:0.319367 
    ## [17] train-error:0.317685 
    ## [18] train-error:0.317348 
    ## [19] train-error:0.314992 
    ## [20] train-error:0.312637 
    ## [21] train-error:0.312468 
    ## [22] train-error:0.310618 
    ## [23] train-error:0.309608 
    ## [24] train-error:0.307757 
    ## [25] train-error:0.304392 
    ## [26] train-error:0.302373 
    ## [27] train-error:0.301363 
    ## [28] train-error:0.299176 
    ## [29] train-error:0.297829 
    ## [30] train-error:0.297325 
    ## [31] train-error:0.297325 
    ## [32] train-error:0.296315 
    ## [33] train-error:0.294801 
    ## [34] train-error:0.294128 
    ## [35] train-error:0.293286 
    ## [36] train-error:0.291099 
    ## [37] train-error:0.289921 
    ## [38] train-error:0.288238 
    ## [39] train-error:0.287397 
    ## [40] train-error:0.286556 
    ## [41] train-error:0.284536 
    ## [42] train-error:0.283695 
    ## [43] train-error:0.282517 
    ## [44] train-error:0.282349 
    ## [45] train-error:0.281003 
    ## [46] train-error:0.278647 
    ## [47] train-error:0.278311 
    ## [48] train-error:0.276291 
    ## [49] train-error:0.276123 
    ## [50] train-error:0.274272 
    ## [51] train-error:0.273263 
    ## [52] train-error:0.272085 
    ## [53] train-error:0.270907 
    ## [54] train-error:0.270234 
    ## [55] train-error:0.269897 
    ## [56] train-error:0.268888 
    ## [57] train-error:0.268383 
    ## [58] train-error:0.268046 
    ## [59] train-error:0.266700 
    ## [60] train-error:0.266196 
    ## [61] train-error:0.266532 
    ## [62] train-error:0.266196 
    ## [63] train-error:0.265859 
    ## [64] train-error:0.265354 
    ## [65] train-error:0.264176 
    ## [66] train-error:0.263335 
    ## [67] train-error:0.263503 
    ## [68] train-error:0.262830 
    ## [69] train-error:0.262157 
    ## [70] train-error:0.262157 
    ## [71] train-error:0.261821 
    ## [72] train-error:0.261316 
    ## [73] train-error:0.261148 
    ## [74] train-error:0.260811 
    ## [75] train-error:0.260643 
    ## [76] train-error:0.260306 
    ## [77] train-error:0.260306 
    ## [78] train-error:0.259970 
    ## [79] train-error:0.259297 
    ## [80] train-error:0.258624 
    ## [81] train-error:0.257782 
    ## [82] train-error:0.257951 
    ## [83] train-error:0.256773 
    ## [84] train-error:0.256604 
    ## [85] train-error:0.255931 
    ## [86] train-error:0.255090 
    ## [87] train-error:0.253912 
    ## [88] train-error:0.253744 
    ## [89] train-error:0.252398 
    ## [90] train-error:0.252230 
    ## [91] train-error:0.251556 
    ## [92] train-error:0.250715 
    ## [93] train-error:0.250379 
    ## [94] train-error:0.250379 
    ## [95] train-error:0.250042 
    ## [96] train-error:0.248864 
    ## [97] train-error:0.247855 
    ## [98] train-error:0.247350 
    ## [99] train-error:0.246845 
    ## [100]    train-error:0.245331 
    ## 61.152 sec elapsed

``` r
jo$test(dev = F)
jo$test()
s <- jo$report()

jo$preds; jo$perform; jo$results
```

    ## # A tibble: 1,515 x 9
    ##     prob  pred linear binary category lemma           ids target model_id  
    ##    <dbl> <dbl>  <dbl>  <dbl> <chr>    <chr>         <int>  <dbl> <chr>     
    ##  1 0.200     0  1.13       0 black    "rt @ ohnosh…   566      0 cbe6aa60b…
    ##  2 0.379     0  0.513      0 white    @theneedledr…  3561      0 cbe6aa60b…
    ##  3 0.380     0 -0.408      0 hispanic @bolanabos w…  4511      0 cbe6aa60b…
    ##  4 0.432     0  0.686      1 white    rt @ dennisd…  9928      1 cbe6aa60b…
    ##  5 0.351     0  0.353      1 white    first time e…  9842      1 cbe6aa60b…
    ##  6 0.669     1  1.46       1 white    rt @ucf_marc…  3492      1 cbe6aa60b…
    ##  7 0.315     0  0.518      0 hispanic rt @ luvdy :…  1712      0 cbe6aa60b…
    ##  8 0.208     0 -0.812      0 black    rt @ lilblac…  4092      0 cbe6aa60b…
    ##  9 0.294     0 -0.131      1 asian    rt @ cassidy…  5820      1 cbe6aa60b…
    ## 10 0.344     0  2.10       0 white    rt @maggieny…  8509      0 cbe6aa60b…
    ## # … with 1,505 more rows

    ## # A tibble: 1 x 7
    ##      ll logloss   auc precision recall fbeta_score accuracy
    ##   <dbl>   <dbl> <dbl>     <dbl>  <dbl>       <dbl>    <dbl>
    ## 1 0.223   0.631 0.643     0.644  0.291       0.400    0.650

    ## # A tibble: 1 x 20
    ##   model_name objective output_dim target data_id data_name text  input_dim
    ##   <chr>      <chr>          <dbl> <chr>  <chr>   <chr>     <chr>     <int>
    ## 1 xgboost    binary:l…          1 binary 355760… sample_d… lemma      2000
    ## # … with 12 more variables: seq_len <dbl>, term_count_min <dbl>,
    ## #   doc_proportion_max <dbl>, total_size <int>, data_path <chr>,
    ## #   train_size <int>, test_size <int>, split_mode <S3: glue>,
    ## #   model_id <chr>, duration <dbl>, metrics <list>, timestamp <chr>

``` r
table(jo$preds$pred, jo$preds$target)
```

    ##    
    ##       0   1
    ##   0 808 432
    ##   1  98 177

## Keras

``` r
#devtools::document()

model_params <- list(
  epochs = 1,
  embed_dim = 128,
  output_dim = 1, 
  output_fun = "sigmoid", 
  loss = "binary_crossentropy", 
  optimizer = "adam", 
  metrics = "accuracy",
  target = "binary",
  model = deeplyr::keras_multi_cnn,
  model_name = "multi_cnn"
)

jo <- deeplyr::learner$new("keras:binary")
jo$set_param(model_params)
jo$set_data(dc_seq)
jo$split(oos = F, val = T)

jo$train()
```

    ## 9.634 sec elapsed

``` r
jo$test(dev = F)
s <- jo$report()

jo$results; jo$preds; jo$perform
```

    ## # A tibble: 1 x 25
    ##   output_fun loss  metrics optimizer epochs embed_dim output_dim target
    ##   <chr>      <chr> <list>  <chr>      <dbl>     <dbl>      <dbl> <chr> 
    ## 1 sigmoid    bina… <tibbl… adam           1       128          1 binary
    ## # … with 17 more variables: model_name <chr>, data_name <chr>, text <chr>,
    ## #   input_dim <int>, seq_len <dbl>, term_count_min <dbl>,
    ## #   doc_proportion_max <dbl>, total_size <int>, data_path <chr>,
    ## #   train_size <int>, test_size <int>, val_size <int>, split_mode <S3:
    ## #   glue>, model_id <chr>, n_filters <dbl>, duration <dbl>,
    ## #   timestamp <chr>

    ## # A tibble: 722 x 9
    ##     prob  pred linear binary category lemma           ids target model_id  
    ##    <dbl> <dbl>  <dbl>  <dbl> <chr>    <chr>         <int>  <dbl> <chr>     
    ##  1 0.333     0 -0.670      0 hispanic check out wh…  4058      0 6e993a35d…
    ##  2 0.355     0  0.521      0 black    the nigga th…  9805      0 6e993a35d…
    ##  3 0.377     0  0.812      0 white    "rt @ natash…  2578      0 6e993a35d…
    ##  4 0.383     0  1.41       1 white    @feingold32 …   245      1 6e993a35d…
    ##  5 0.417     0  1.77       1 white    rt @ jeffkot…  7394      1 6e993a35d…
    ##  6 0.394     0  0.284      0 white    @maybeacrook…  3935      0 6e993a35d…
    ##  7 0.387     0  1.31       1 white    @hatamotorok…  4503      1 6e993a35d…
    ##  8 0.379     0  2.10       0 white    rt @ thedemc…  3953      0 6e993a35d…
    ##  9 0.415     0  0.793      1 white    sear fox nat…  2967      1 6e993a35d…
    ## 10 0.355     0  0.105      0 hispanic i be shook t…  6230      0 6e993a35d…
    ## # … with 712 more rows

    ## # A tibble: 1 x 7
    ##      ll logloss   auc precision recall fbeta_score accuracy
    ##   <dbl>   <dbl> <dbl>     <dbl>  <dbl>       <dbl>    <dbl>
    ## 1 0.405   0.673 0.622       NaN      0         NaN    0.583

``` r
table(jo$preds$pred, jo$preds$target)
```

    ##    
    ##       0   1
    ##   0 421 301

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

# Data

``` r
# load("projects/dev-predict-participation/data/final_tweets.Rdata")
# sample_dat <- final_tweets %>% 
#   mutate(party_binary = case_when(party == "rep" ~ 1, party == "dem" ~ 0)) %>%
#   select(linear = theta, binary = party_binary, category = race, lemma) %>% 
#   sample_n(10000) %>% 
#   glimpse
# 
# save(sample_dat, file = "sample_dat.Rdata")
```
