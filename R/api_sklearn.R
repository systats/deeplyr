#' get_sk_params
#' @export
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

#' fit_sk_glm
#' @export
fit_sk_glm <- function(self){
  
  sk <- reticulate::import("sklearn.linear_model")
  
  ### set loss function
  if(self$meta$task == "linear") {
    
    model <- sk$LinearRegression()
    sk_params <- sk$LinearRegression() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$LinearRegression, m_params)
    
  } else if(self$meta$task == "multi") {
    
    # sklearn.linear_model.LogisticRegression(setting multi_class=”multinomial”)
    sk_params <- sk$LogisticRegression() %>% get_sk_params
    m_params <- c(self$params[sk_params], list(multi_class = "multinomial", solver = "newton-cg")) %>% compact
    model <- do.call(sk$LogisticRegression, m_params)
    
  } else if(self$meta$task == "binary") {
    
    model <- sk$LogisticRegression()
    sk_params <- sk$LogisticRegression() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$LogisticRegression, m_params)
  }

  model$fit(X = self$process$juice_x_matrix(), y = self$process$juice_y())

  return(model)
}

#' predict_sk
#' @export
predict_sk <- function(self, new_data){

  if(self$meta$task == "linear"){
    
    pred <- self$model$predict(self$process$stream(new_data)) %>%
      round(3)
    dplyr::tibble(pred)
    
  } else if(self$meta$task == "binary"){
    
    pred <- self$model$predict_proba(self$process$stream(new_data)) %>% round(3)
    
    probs <- pred %>%
      dplyr::as_tibble() %>% 
      purrr::set_names(paste0("prob", 0:(ncol(pred)-1)))
    
    pred <- pred %>% 
      split(1:nrow(.)) %>% 
      purrr::map_int(which.max) - 1
    
    dplyr::tibble(pred, prob = probs$prob1)
    
  } else if(self$meta$task %in% "multi"){
    
    pred <- self$model$predict_proba(self$process$stream(new_data)) %>% round(3)

    probs <- pred %>%
      dplyr::as_tibble() %>% 
      purrr::set_names(paste0("prob", 1:length(.)))
    
    pred <- probs %>% 
      split(1:nrow(.)) %>% 
      purrr::map_int(which.max) %>% 
      as.factor()
    
    dplyr::tibble(pred) %>% dplyr::bind_cols(probs)
    
  }
}

#' fit_sk_tree
#' @export
fit_sk_tree <- function(self){
  
  sk <- reticulate::import("sklearn.tree")
  
  ### set loss function
  if(self$meta$task == "linear") {
    
    model <- sk$DecisionTreeRegressor()
    sk_params <- sk$DecisionTreeRegressor() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$DecisionTreeRegressor, m_params)
    
  } else if(self$meta$task == "multi") {
    
    sk_params <- sk$DecisionTreeClassifier() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$DecisionTreeClassifier, m_params)
    
  } else if(self$meta$task == "binary") {
    
    model <- sk$DecisionTreeClassifier()
    sk_params <- sk$DecisionTreeClassifier() %>% get_sk_params
    m_params <- c(self$params[sk_params], list()) %>% compact
    model <- do.call(sk$DecisionTreeClassifier, m_params)
  }
  
  
  model$fit(X = self$process$juice_x_matrix(), y = self$process$juice_y())
  
  return(model)
}