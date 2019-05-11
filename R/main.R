#' @export
learner <- R6::R6Class("learner",
  inherit = backends, 
  private = list(
    x = NULL,
    data = NULL,
    tokenizer = NULL,
    param = NULL,
    class_weights = NULL,
    metrics = NULL,
    new_path = NULL,
    ### some private functions
    create_id = function(){
      private$param$model_id <- private$param %>%
        keep(~is.atomic(.x) & length(.x) == 1) %>% 
        c(Sys.time()) %>%
        paste(collapse = "") %>% 
        openssl::md5(key = "model_id") %>% 
        as.character
      
      private$new_path <- glue::glue("models/{private$param$model_id}")
      
      if(!dir.exists(private$new_path)){
        dir.create(private$new_path)
      }
    },
    overwrite_param = function(param){
      param %>% iwalk(purrr::possibly(function(.x, .y){private$param[[.y]] <- .x}, NULL))
    },
    save_model_container = function(){
      
      self$results %>%
        as.list() %>%
        jsonlite::toJSON(force = T) %>%
        jsonlite::fromJSON() %>%
        jsonlite::toJSON(pretty = T, auto_unbox = T) %>%
        writeLines(con =  glue::glue("{private$new_path}/model_param.json"))
      
      # save(results, file = glue::glue("{private$new_path}/model_results.Rdata"))
      
      ### write predictions
      preds <- self$preds
      save(preds, file = glue::glue("{private$new_path}/model_preds.Rdata"))
      
      ### self
      private$data <- NULL
      private$x <- NULL
      private$split_id = NULL
      private$train = NULL
      private$test = NULL
      private$val = NULL
      private$x_train = NULL
      private$x_val = NULL
      private$x_test = NULL
      private$y_train = NULL
      private$y_val = NULL
      private$y_test = NULL
      
      s <- self
      save(s, file = glue::glue("{private$new_path}/model_container.Rdata"))
    }
  ),
  public = list(
    ### Initalize variables
    fit = NULL,
    predict = NULL, 
    eval = NULL,
    preds = NULL,
    perform = NULL,
    results = NULL,
    ### Main Function
    initialize = function(backend) {
      
      self$set_backend(backend)
      
      if(!dir.exists("models")){
        dir.create("models")
      }
      # if(!dir.exists("models/results")){
      #   dir.create("models/results")
      # }
    },
    mutate_data = function(...){
      private$data <- private$data %>% 
        mutate(...)
    },
    glimpse_data = function(){
      return(list(private$data, private$x_test, private$y_test) %>% map(glimpse))
    },
    set_data = function(container = NULL, path = NULL){

      if(!is.null(path)) container <- get(load(path))
      private$data <- container$data
      private$x <- container$x
      private$tokenizer <- container$tokenizer
      private$param <- c(private$param, container$get_param())
      #private$overwrite_param(container$get_param())
      
    },
    set_model = function(model_name, model){
      private$param$model <- model
      private$param$model_name <- model_name
    },
    set_param = function(list){
      private$overwrite_param(list)
    },
    get_param = function(){
      return(private$param)
    },
    save_state = function(){
      
      if(is.null(private$param$model_id)){
        private$create_id()
      }
      private$new_path <- glue::glue("models/{private$param$model_id}")
      if(!dir.exists(private$new_path)){
        dir.create(private$new_path)
      }
      
      save(self, file = paste0(private$new_path, "/model_container.Rdata"))
    },
    transform_seq = function(text){
      private$tokenizer %>% 
        keras::texts_to_sequences(text) %>%
        keras::pad_sequences(maxlen = private$param$seq_len)
    },
    predict_seq = function(text){
      self$predict(private$param$model, self$transform_seq(text))
    },
    transform_dtm = function(text){
      private$tokenizer %>%
        keras::texts_to_matrix(text, mode = "binary")
    },
    predict_dtm = function(text){
      self$predict(private$param$model, self$transform_dtm(text))
    },
    train = function(){
      
      private$create_id()
      
      tictoc::tic()
      
      private$param$model <- self$fit(self, private)
      
      time <- tictoc::toc(log = T)
      
      suppressMessages(
        private$param$duration <- as.numeric(time$toc - time$tic) %>% round(1)
      )
    },
    test = function(dev = F){
      
      # return(self$predict(private$param$model, private$x_test))
      self$preds <- self$predict(private$param$model, private$x_test) 
      
      if(dev) return(self$preds)
      
      self$eval()

      ### finalize results
      self$results <- private$param %>%
        keep(~is.atomic(.x) & length(.x) == 1) %>%
        bind_cols %>%
        mutate(metrics = list(self$perform)) %>%
        mutate(timestamp = as.character(Sys.time()))
    },
    report = function(mkdir = "", return_value = F){
      
      if(mkdir != "") {
        private$new_path <- paste0(mkdir, "/", private$new_path)
      }
      
      private$save_model_container()
      
      ### return value?
      # performance evaluation
      if(return_value){
        out <- self$perform
      } else {
        out <- self
      }
      return(out)
    }
  )
)