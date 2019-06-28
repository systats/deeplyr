#' @export
learner <- R6::R6Class("learner",
  inherit = backends, 
  private = list(
    #class_weights = NULL,
    metrics = NULL,
    new_path = NULL,
    ### some private functions
    create_id = function(){
      self$param$model_id <- self$param %>%
        keep(~is.atomic(.x) & length(.x) == 1) %>% 
        c(Sys.time()) %>%
        paste(collapse = "") %>% 
        openssl::md5(key = "model_id") %>% 
        as.character
    },
    overwrite_param = function(param){
      param %>% iwalk(purrr::possibly(function(.x, .y){self$param[[.y]] <- .x}, NULL))
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
      self$data <- NULL
      
      s <- self
      save(s, file = glue::glue("{private$new_path}/model_container.Rdata"))
    }
  ),
  public = list(
    ### Initalize variables
    data = NULL,
    splits = NULL,
    tokenizer = NULL,
    param = NULL,
    ### Methods placeholder
    fit = NULL,
    predict = NULL, 
    eval = NULL,
    preds = NULL,
    perform = NULL,
    results = NULL,
    plots = NULL,
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
    get_entry = function(name){
      return(list(private = private[[name]], self = self[[name]]) %>% compact %>% .[[1]])
    },
    glimpse_data = function(){
      return(self$data %>% map(glimpse))
    },
    set_data = function(x, y, id = NULL){ #container = NULL, data_path = NULL

      self$data$x <- x
      self$data$y <- y
      self$data$id <- id
      
      # if(!is.null(data)){
      #   self$data <- data
      #   private$x <- as.matrix(x)
      # } else {
      #   if(!is.null(data_path)) container <- get(load(data_path))
      #   self$data <- container$data
      #   private$x <- as.matrix(container$x)
      #   self$tokenizer <- container$tokenizer
      #   self$param <- c(self$param, container$get_param())
      # }
      # private$overwrite_param(container$get_param())
    },
    set_param = function(list){
      private$overwrite_param(list)
    },
    get_param = function(){
      return(self$param)
    },
    set_split = function(id = NULL, val = F, oos = F, balance = NULL, outcome_mat = F){
      sp <- splitter$new()
      sp$set(self$data$x, self$data$y, self$data$id)
      sp$split(val, oos, balance, outcome_mat)
      self$splits <- sp$splits
    },
    set = function(param, data = NULL, x = NULL, container = NULL, data_path = NULL,
                   id = NULL, val = F, oos = F, balance = NULL, outcome_mat = F){
      self$set_param(param)
      self$set_data(data = data, x = x, container = container, data_path = data_path)
      self$set_split(id, val, oos, balance, outcome_mat)
    },
    # save_state = function(){
    #   
    #   if(is.null(self$param$model_id)){
    #     private$create_id()
    #   }
    #   private$new_path <- glue::glue("models/{self$param$model_id}")
    #   if(!dir.exists(private$new_path)){
    #     dir.create(private$new_path)
    #   }
    #   
    #   save(self, file = paste0(private$new_path, "/model_container.Rdata"))
    # },
    transform_seq = function(text){
      self$tokenizer %>% 
        keras::texts_to_sequences(text) %>%
        keras::pad_sequences(maxlen = self$param$seq_len)
    },
    predict_seq = function(text){
      self$predict(self$param$model, self$transform_seq(text))
    },
    transform_dtm = function(text){
      self$tokenizer %>%
        keras::texts_to_matrix(text, mode = "binary")
    },
    predict_dtm = function(text){
      self$predict(self$param$model, self$transform_dtm(text))
    },
    train = function(cv = F){
      
      private$create_id()
      
      tictoc::tic()
      
      self$param$model <- self$fit(self, private, cv)
      
      time <- tictoc::toc(log = T)
      
      suppressMessages(
        self$param$duration <- as.numeric(time$toc - time$tic) %>% round(1)
      )
    },
    test = function(dev = F){
      

      if(private$objective == "mixture"){
        self$preds <- self$predict(self$param$model, self$splits$test$x, self$param$output_dim, self$param$mix_dim) 
      } else {
        self$preds <- self$predict(self$param$model, self$splits$test$x)
      }
      
      if(dev) return(self$preds)
      
      self$eval$eval(target = self$splits$test$target, self$preds)

      ### finalize results
      self$results <- self$param %>%
        keep(~is.atomic(.x) & length(.x) == 1) %>%
        bind_cols %>%
        mutate(metrics = list(self$eval$perform)) %>%
        mutate(timestamp = as.character(Sys.time()))
    },
    report = function(folder = ".", return_value = F){
      
      if(folder != "") {
        private$new_path <- paste0("models/", folder, "/", self$param$model_id)
      } else {
        private$new_path <- glue::glue("models/{self$param$model_id}")
      }
      
      if(!dir.exists(private$new_path)){
        dir.create(private$new_path)
      }
      
      ### save model container
      #private$save_model_container()
      
      ### save plots
      ggsave_pos <- possibly(ggsave, NULL)
      self$eval$plots %>% 
        iwalk(~ggsave_pos(.x, file = glue::glue("{private$new_path}/{.y}.png")))
    }
  )
)
