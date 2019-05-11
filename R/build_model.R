#' @export
learner <- R6::R6Class("learner",
  inherit = backends, 
  private = list(
    tokenizer = NULL,
    data = NULL,
    x = NULL,
    params = NULL,
    class_weights = NULL,
    ### some private functions
    create_id = function(){
      private$params$model_id <- private$params %>%
        keep(~is.atomic(.x) & length(.x) == 1) %>% 
        c(Sys.time()) %>%
        paste(collapse = "") %>% 
        openssl::md5(key = "model_id") %>% 
        as.character
    }
  ),
  public = list(
    fit = NULL,
    predict = NULL, 
    ### Initalize variables
    perform = NULL,
    preds = NULL,
    ### Main Function
    initialize = function() { #type
      
      #private$set_backend(backend)
      # self$set_eval(type) # linear, binary, categorical
      
      if(!dir.exists("models")){
        dir.create("models")
      }
      # if(!dir.exists("models/results")){
      #   dir.create("models/results")
      # }
    },
    set_data = function(container = NULL, path = NULL){
      if(!is.null(path)) container <- get(load(path))
      private$data <- container$data
      private$x <- container$x
      private$tokenizer <- container$tokenizer
      private$params <- container$get_params()
    },
    mutate_data = function(...){
      private$data <- private$data %>% 
        mutate(...)
    },
    glimpse_data = function(){
      return(list(private$data, private$x_test, private$y_test) %>% map(glimpse))
    },
    # set params
    set_model = function(model_name, model){
      private$params$model <- model
      private$params$model_name <- model_name
    },
    model = function(){
      return(private$params$model)
    },
    set_params = function(list){
      iwalk(list, purrr::possibly(function(.x, .y){private$params[[.y]] <- .x}, NULL))
      #private$params <- c(private$params, list)
    },
    get_params = function(){
      return(private$params)
    },
    save_state = function(){
      
      if(is.null(private$params$model_id)){
        private$create_id()
      }
      private$new_path <- glue::glue("models/{private$params$model_id}")
      if(!dir.exists(private$new_path)){
        dir.create(private$new_path)
      }
      
      save(self, file = paste0(private$new_path, "/model_container.Rdata"))
    },
    transform_seq = function(text){
      private$tokenizer %>% 
        keras::texts_to_sequences(text) %>%
        keras::pad_sequences(maxlen = private$params$seq_len)
    },
    predict_seq = function(text){
      self$predict(private$params$model, self$transform_seq(text))
    },
    transform_dtm = function(text){
      private$tokenizer %>%
        keras::texts_to_matrix(text)
    },
    predict_dtm = function(text){
      self$predict(private$params$model, self$transform_dtm(text))
    },
    train_model = function(){
      
      tictoc::tic()
      
      private$params$model <- self$fit(
        model = private$params$model, 
        private$x_train, 
        private$y_train
      )
      
      time <- tictoc::toc(log = T)
      
      suppressMessages(
        private$params$duration <- as.numeric(time$toc - time$tic) %>% round(1)
      )
    },
    eval_model = function(){
      
      self$preds <- self$predict(private$params$model, private$x_test) %>%
        bind_cols(private$test)
      
      self$perform <- tibble(
        acc = Metrics::accuracy(self$preds$pred, private$y_test),
        auc = Metrics::auc(self$preds$pred, private$y_test),
        " !!!!!!! "
      )
    }
  )
)


# get_model_params = function(model, params){
#   
#   # do_func_pos <- function(what, data){
#   #   acceptable_args <- data[names(data) %in% (formals(what) %>% names)]
#   #   do.call(what, acceptable_args %>% as.list)
#   # }
#   
#   out <- formals(model) %>% 
#     imap(~{
#       if(.y %in% names(params)){
#         .x <-  params[[.y]]
#       }
#       return(.x)
#     })
#   return(out)
# },
# complete_params = function(params1, params2){
#   not_included <- params2[!(names(params2) %in% names(params1))]
#   out <- params1 %>% 
#     imap(~{
#       if(.y %in% names(params2)){
#         .x <-  params2[[.y]]
#       } 
#       return(.x)
#     }) %>% 
#     c(not_included)
#   return(out)
# }