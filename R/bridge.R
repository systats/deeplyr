#' learner
#' @export
bridge <- R6::R6Class(
  "bridge",
  #portable = F,
  #inherit = backend,
  active = list(
    formula = function() {
      as.formula(glue::glue("{self$ask_y()} ~ {paste0(self$ask_x(), collapse = ' + ')}"))
    },
    factors = function(){
      sort(unique(self$data$outcomes[[1]]))
    }
  ),
  public = list(
    ### public data slots
    data = NULL,
    
    initialize = function(process = NULL){
      if(!is.null(process)) self$data <- process
    },
    
    ### helper
    ask_y = function() {
      self$data$outcomes %>% colnames()
    },
    ask_x = function() {
      self$data$predictors %>% colnames()
    },
    
    ### bake in data + preprocessing for recipes
    bake_xy = function(x, y, ...) {
      self$data <- hardhat::mold(x, y, ...)
    },
    bake_recipe = function(recipe, data, ...) {
      self$data <- hardhat::mold(recipe, data, ...)
    },
    bake_formula = function(formula, data, ...) {
      self$data <- hardhat::mold(formula, data, ...)
    },
    bake = function(x, y, ...) {
      self$data <- hardhat::mold(x, y, ...)
    },
    
    ### juice out training data
    juice_x = function() {
      self$data$predictors
    },
    juice_x_matrix = function() {
      data.matrix(self$data$predictors)
    },
    juice_y = function() {
      as.vector(self$data$outcomes[[1]])
    },
    juice_y_matrix = function() {
      data.matrix(self$data$outcomes)
    },
    juice_y_tibble = function() {
      self$data$outcomes
    },
    juice_extra = function(){
      dplyr::bind_cols(self$data$extras$roles)
    },
    
    juice = function() {
      
      extra <- dplyr::bind_cols(self$data$extras$roles)

      list(self$juice_extra(), self$juice_x(), self$juice_y_tibble()) %>% 
        purrr::compact() %>% 
        purrr::reduce(dplyr::bind_cols)
    },
    
    ### stream new data through
    stream = function(new_data) {
      hardhat::forge(dplyr::as_tibble(new_data), self$data$blueprint, outcomes = F)$predictors
    },

    stream_matrix = function(new_data){
      data.matrix(self$stream(new_data))
    },
    
    stream_id_x = function(new_data){
      d <- hardhat::forge(dplyr::as_tibble(new_data), self$data$blueprint, outcomes = T)
      extras <- dplyr::bind_cols(d$extras$roles)
      d$outcomes <- NULL
      list(extras, d$predictors) %>%
        purrr::compact() %>% 
        purrr::reduce(dplyr::bind_cols)
    },
    
    stream_all = function(new_data) {
      forge_pos <- purrr::possibly(hardhat::forge, NULL)
      d <- forge_pos(dplyr::as_tibble(new_data), self$data$blueprint, outcomes = T)
      if(is.null(d)) d <- forge_pos(dplyr::as_tibble(new_data), self$data$blueprint, outcomes = F)
      extra <- dplyr::bind_cols(d$extras$roles)
      list(extra, d$outcomes, d$predictors) %>%
        purrr::compact() %>% 
        purrr::reduce(dplyr::bind_cols)
    }
  )
)