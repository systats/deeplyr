#' meta
#' @export
meta <- R6::R6Class(
  private = list(
    
  ), 
  public = list(
    
    params = NULL,
    recs = NULL,
    recipe = NULL,
    tok = NULL,
    
    x = NULL, 
    y = NULL, 
    
    n_features = NULL, 
    n_train = NULL, 
    n_test = NULL, 
    
    timestamp = NULL, 
    runtime = NULL, 
    
    model_id = NULL, 
    task = NULL,
    backend = NULL, 
    
    initialize = function(path = NULL){
      if(!is.null(path)){
        if(file.exists(glue::glue("{path}/meta.json"))){
          
          m <- jsonlite::read_json(glue::glue("{path}/meta.json"))[[1]] %>% glimpse
          
          self$model_id <- m$model_id
          self$timestamp <- m$timestamp
          self$runtime <- m$runtime
          self$task <- m$task
          self$backend <- m$backend
          self$n_train <- m$n_train
          self$n_test <- m$n_test
          self$n_features <- m$n_features
          
          if(file.exists(glue::glue("{path}/recs.rds"))){
            self$recs <- readr::read_rds(glue::glue("{path}/recs.rds"))
          }
          if(file.exists(glue::glue("{path}/recipe.rds"))){
            self$recipe <- readr::read_rds(glue::glue("{path}/recipe.rds"))
          }
          if(file.exists(glue::glue("{path}/tok"))){
            self$tok <- keras::load_text_tokenizer(glue::glue("{path}/tok"))
          }
          
          self$params <- jsonlite::read_json(glue::glue("{path}/params.json"))[[1]]
          
        } else {
          stop("Model does not exist")
        }
      }
    },
    
    feed = function(x, y, params){

      self$x <- x
      self$y <- y 
      self$params <- params
      
      if(!is.null(params[["tok"]])){
        self$tok <- params[["tok"]]
        self$x <- self$stream(x)
      }
      if(!is.null(params[["recipe"]])){
        self$recipe <- params[["recipe"]]
        self$x <- self$stream(x)
        # self$x <- recipes::bake(self$recipe, self$x)
      }
      
      self$n_features <- ncol(self$x)
      nr <- nrow(self$x)
      if(is.null(nr)) nr <- length(self$x)
      self$n_train <- nr
      
      self$timestamp <- Sys.time()
      self$model_id <- stringr::str_sub(digest::digest(self$timestamp), 1, 8)
    },
    
    stream = function(x, params){
      if(!is.null(self$recs)){
        return(self$recs$extract(x))
      } else if(!is.null(self$recipe)){
        return(as.matrix(recipes::bake(self$recipe, x)))
      } else if(!is.null(self$tok)){
        return(as.matrix(deeplyr::tokenize_text(x, self$tok, seq_len = self$params$seq_len)))
      } else {
        return(as.matrix(x))
      }
    }, 
    
    set = function(key, value){
      self[[key]] <- value
    },
    
    get = function(){
      list(
        model_id = self$model_id, 
        timestamp = as.character(self$timestamp),
        runtime = as.character(self$runtime), 
        task = self$task,
        backend = self$backend,
        n_train = self$n_train, 
        n_test = self$n_test, 
        n_features = self$n_features
      )
    }
  )
)

# m <- meta$new()
# m$feed(mtcars, mtcars$mpg)
# m$x_name
# m$y_name
# m$get()

# bridge <- R6::R6Class(
#   "bridge",
#   #portable = F,
#   #inherit = backend,
#   active = list(
#     formula = function() {
#       as.formula(glue::glue("{self$ask_y()} ~ {paste0(self$ask_x(), collapse = ' + ')}"))
#     },
#     factors = function(){
#       sort(unique(self$data$outcomes[[1]]))
#     }
#   ),
#   public = list(
#     ### public data slots
#     data = NULL,
#     
#     initialize = function(process = NULL){
#       if(!is.null(process)) self$data <- process
#     },
#     
#     ### helper
#     ask_y = function() {
#       self$data$outcomes %>% colnames()
#     },
#     ask_x = function() {
#       self$data$predictors %>% colnames()
#     },
#     
#     ### bake in data + preprocessing for recipes
#     bake_xy = function(x, y, ...) {
#       self$data <- hardhat::mold(x, y, ...)
#     },
#     bake_recipe = function(recipe, data, ...) {
#       self$data <- hardhat::mold(recipe, data, ...)
#     },
#     bake_formula = function(formula, data, ...) {
#       self$data <- hardhat::mold(formula, data, ...)
#     },
#     bake = function(x, y, ...) {
#       self$data <- hardhat::mold(x, y, ...)
#     },
#     
#     ### juice out training data
#     juice_x = function() {
#       self$data$predictors
#     },
#     juice_x_matrix = function() {
#       data.matrix(self$data$predictors)
#     },
#     juice_y = function() {
#       as.vector(self$data$outcomes[[1]])
#     },
#     juice_y_matrix = function() {
#       data.matrix(self$data$outcomes)
#     },
#     juice_y_tibble = function() {
#       self$data$outcomes
#     },
#     juice_extra = function(){
#       dplyr::bind_cols(self$data$extras$roles)
#     },
#     
#     juice = function() {
#       
#       extra <- dplyr::bind_cols(self$data$extras$roles)
#       
#       list(self$juice_extra(), self$juice_x(), self$juice_y_tibble()) %>% 
#         purrr::compact() %>% 
#         purrr::reduce(dplyr::bind_cols)
#     },
#     
#     ### stream new data through
#     stream = function(new_data) {
#       hardhat::forge(dplyr::as_tibble(new_data), self$data$blueprint, outcomes = F)$predictors
#     },
#     
#     stream_matrix = function(new_data){
#       data.matrix(self$stream(new_data))
#     },
#     
#     stream_id_x = function(new_data){
#       d <- hardhat::forge(dplyr::as_tibble(new_data), self$data$blueprint, outcomes = T)
#       extras <- dplyr::bind_cols(d$extras$roles)
#       d$outcomes <- NULL
#       list(extras, d$predictors) %>%
#         purrr::compact() %>% 
#         purrr::reduce(dplyr::bind_cols)
#     },
#     
#     stream_all = function(new_data) {
#       forge_pos <- purrr::possibly(hardhat::forge, NULL)
#       d <- forge_pos(dplyr::as_tibble(new_data), self$data$blueprint, outcomes = T)
#       if(is.null(d)) d <- forge_pos(dplyr::as_tibble(new_data), self$data$blueprint, outcomes = F)
#       extra <- dplyr::bind_cols(d$extras$roles)
#       list(extra, d$outcomes, d$predictors) %>%
#         purrr::compact() %>% 
#         purrr::reduce(dplyr::bind_cols)
#     }
#   )
# )
