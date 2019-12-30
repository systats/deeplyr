### FUNS
prep_splits <- function(){
  N <- 1000
  df <- tibble(
    split = sample(1:3, size = N, prob = c(.8, .1, .1), replace = T),
    split_cv = sample(1:5, size = N, prob = rep(.2, 5), replace = T),
    x1 = rnorm(N),
    x2 = rnorm(N),
    z = x1 + x2 + rnorm(N),
    y = ifelse(z > mean(z), 1, 0), #2*x1 + 2*x2 + rnorm(1000)
    y_multi0 = case_when(z > (mean(z)+.2*sd(z)) ~ 0, z < (mean(z)-.2*sd(z)) ~ 2, T ~ 1),
    y_multi1 = y_multi0 + 1
  )
  
  ### Data
  sp_linear <<- split_sample(x = df[,c("x1", "x2")], id = df$split, y = df$z, meta = df)
  sp_binary <<- split_sample(x = df[,c("x1", "x2")], id = df$split, y = df$y, meta = df)
  sp_multi <<- split_sample(x = df[,c("x1", "x2")], id = df$split, y = df$y_multi1, meta = df)
  
}

test_linear <- function(learner){
  tinytest::expect_equal(colnames(learner$preds[,1]), c("pred"), info = "linear preds -> correct columns")
  tinytest::expect_equal(colnames(learner$metrics), c("mse", "rmse", "mae", "mape"), info = "linear metrics -> correct columns")
}

test_binary<- function(learner){
  tinytest::expect_equal(colnames(learner$preds[,1:3]), c("pred", "prob0", "prob1"), info = "binary preds -> correct columns")
  tinytest::expect_equal(names(learner$metrics[1:3]), c("accuracy", "precision", "recall"), info = "binary metrics -> correct columns")
}

test_multi<- function(learner){
  tinytest::expect_equal(colnames(learner$preds[,1:4]), c("pred", "prob1", "prob2", "prob3"), info = "multi preds -> correct columns")
  tinytest::expect_equal(names(learner$metrics[1:3]), c("accuracy", "precision", "recall"), info = "multi metrics -> correct columns")
}

### packages
pacman::p_load(tidyverse, h2o, deeplyr)
h2o::h2o.init(nthreads = -1)

### create dummy data
prep_splits()


### GLM
fit_learner(list(), sp_linear, task = "linear", backend = "h2o_glm") %>% test_linear()
fit_learner(list(), sp_binary, task = "binary", backend = "h2o_glm") %>% test_binary()
fit_learner(list(), sp_multi, task = "multi", backend = "h2o_glm") %>% test_multi()

### CV
# sp_linear_cv <- split_sample(x = df[,c("x1", "x2")], id = df$split_cv, y = df$z, meta = df)
# sp_binary_cv <- split_sample(x = df[,c("x1", "x2")], id = df$split_cv, y = df$y, meta = df)
# sp_multi_cv <- split_sample(x = df[,c("x1", "x2")], id = df$split_cv, y = df$y_multi1, meta = df)

# fit_cv(list(), sp_linear, task = "linear", backend = "h2o_glm") %>% test_linear()
# fit_cv(list(), sp_binary, task = "binary", backend = "h2o_glm") %>% test_binary()
# fit_cv(list(), sp_multi, task = "multi", backend = "h2o_glm") %>% test_multi()

