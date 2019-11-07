deeplyr
================

[![](https://img.shields.io/github/languages/code-size/systats/deeplyr.svg)](https://github.com/systats/deeplyr)
[![](https://img.shields.io/github/last-commit/systats/deeplyr.svg)](https://github.com/systats/deeplyr/commits/master)

This package provides a simple interface for several third party machine
learning libraries. Beside relational data and boosted trees a special
focus is given to Natural Language Processing by gathering certain types
of modern neural network architectures.

``` r
pacman::p_load(tidyverse, purrrr, tidyr)
```

Thanks to rapid open source development of popular machine learning
libraries we able to fall back to on a wide variety of frameworks and
model architectures. Of course your data project most likely requires
some modifications to any given model, but browsing the main code blcoks
allows to compose a suitable architecture. A short list can be seen
here:

``` r
deeplyr::list_keras_models()
```

    ## # A tibble: 13 x 4
    ##    name         model  param      backend
    ##    <chr>        <list> <list>     <chr>  
    ##  1 simple_mlp   <fn>   <pairlist> keras  
    ##  2 deep_mlp     <fn>   <pairlist> keras  
    ##  3 lstm         <fn>   <pairlist> keras  
    ##  4 bi_lstm      <fn>   <pairlist> keras  
    ##  5 deep_lstm    <fn>   <pairlist> keras  
    ##  6 deep_bi_lstm <fn>   <pairlist> keras  
    ##  7 gru          <fn>   <pairlist> keras  
    ##  8 bi_gru       <fn>   <pairlist> keras  
    ##  9 cnn_gru      <fn>   <pairlist> keras  
    ## 10 cnn_lstm     <fn>   <pairlist> keras  
    ## 11 gru_cnn      <fn>   <pairlist> keras  
    ## 12 multi_cnn    <fn>   <pairlist> keras  
    ## 13 sep_cnn      <fn>   <pairlist> keras

How to view a specific model arch?

``` r
deeplyr::list_keras_models("bi_lstm")$model
```

    ## [[1]]
    ## function (input_dim, embed_dim = 128, seq_len = 50, lstm_dim = 64, 
    ##     lstm_drop = 0.2, dropout = 0.2, output_dim = 1, output_fun = "softmax") 
    ## {
    ##     model <- keras::keras_model_sequential() %>% keras::layer_embedding(input_dim = input_dim, 
    ##         output_dim = embed_dim, input_length = seq_len) %>% keras::bidirectional(keras::layer_lstm(units = lstm_dim, 
    ##         dropout = dropout, recurrent_dropout = lstm_drop)) %>% 
    ##         keras::layer_dense(units = output_dim, activation = output_fun)
    ##     return(model)
    ## }
    ## <bytecode: 0x7fa64d61d190>
    ## <environment: namespace:deeplyr>

<!-- This is a light-weight model engine mainly for text classification tasks written as R6class. While Keras, XGBoost or Ranger (RF) provide high quality learners, `deeplyr` comes with unifying train/test/report functions for all backends mentioned. This streamlined data/ model worklfow does automatically  -->

``` r
# devtools::document()
# devtools::install()
```
