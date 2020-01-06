
get_sk_params <- function(f){
  f %>%
    as.character() %>% 
    stringr::str_remove_all("^.*?\\(|\\)") %>%
    stringr::str_split(", |\\n") %>%
    .[[1]] %>%
    stringr::str_squish() %>%
    str_split("=") %>%
    map_chr(1)
}


fit_sk_glm <- function(self){
  
  sk <- reticulate::import("sklearn.linear_model")
  
  ### set loss function
  if(self$task == "linear") {
    
    model <- sk$LinearRegression()
    sk_params <- sk$LinearRegression() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$LinearRegression, m_params)
    
  } else if(self$task == "multi") {
    
    # sklearn.linear_model.LogisticRegression(setting multi_class=”multinomial”)
    sk_params <- sk$LogisticRegression() %>% get_sk_params
    m_params <- c(self$params[sk_params], list(multi_class = "multinomial", solver = "newton-cg")) %>% compact
    model <- do.call(sk$LogisticRegression, m_params)
    
  } else if(self$task == "binary") {
    
    model <- sk$LogisticRegression()
    sk_params <- sk$LogisticRegression() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$LogisticRegression, m_params)
  }
  
  
  model$fit(X = as.matrix(self$data$train$x), y = self$data$train$y)

  return(model)
}


predict_sk <- function(self, x_test = NULL){
  
  if(is.null(x_test)) x_test <- self$data$test$x
  
  if(self$task == "linear"){
    
    pred <- round(self$model$predict(as.matrix(x_test)), 3)
    tibble(pred)
    
  } else if(self$task == "binary"){
    
    pred <- round(self$model$predict_proba(as.matrix(x_test)), 3)
    probs <- as_tibble(pred) %>% set_names(paste0("prob", 0:(ncol(pred)-1)))
    pred <- pred %>% split(1:nrow(.)) %>% map_int(which.max) - 1
    tibble(pred, prob = probs$prob1)
    
  } else if(self$task %in% "multi"){
    
    pred <- round(self$model$predict_proba(as.matrix(x_test)), 3)
    probs <- as_tibble(pred) %>% set_names(paste0("prob", 1:ncol(pred)))
    pred <- pred %>% split(1:nrow(.)) %>% map_int(which.max)
    tibble(pred) %>% bind_cols(probs)
    
  }
}



fit_sk_tree <- function(self){
  
  sk <- reticulate::import("sklearn.tree")
  
  ### set loss function
  if(self$task == "linear") {
    
    model <- sk$DecisionTreeRegressor()
    sk_params <- sk$DecisionTreeRegressor() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$DecisionTreeRegressor, m_params)
    
  } else if(self$task == "multi") {
    
    sk_params <- sk$DecisionTreeClassifier() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$DecisionTreeClassifier, m_params)
    
  } else if(self$task == "binary") {
    
    model <- sk$DecisionTreeClassifier()
    sk_params <- sk$DecisionTreeClassifier() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$DecisionTreeClassifier, m_params)
  }
  
  
  model$fit(X = as.matrix(self$data$train$x), y = self$data$train$y)
  
  return(model)
}