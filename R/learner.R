#' @export
learner <- R6::R6Class("learner",
  #inherit = backends, 
  private = list(
    backend = NULL,
    objective = NULL,
    metrics = NULL,
    new_path = NULL,
    set_backend = function(backend){
      input <- backend %>%
        str_split(":") %>% unlist()
      
      private$backend <- input[1]
      private$objective <- input[2]
      
      if(private$backend == "keras") self$trainer <- trainer_keras$new(private$objective)
      if(private$backend == "xgboost") self$trainer <- trainer_xgboost$new(private$objective)
      if(private$backend == "ranger") self$trainer <- trainer_ranger$new(private$objective)
      
      self$eval <- evaluator$new(private$objective)
    },
    create_id = function(){
      self$param$model_id <- self$param %>%
        keep(~is.atomic(.x) & length(.x) == 1) %>% 
        c(Sys.time()) %>%
        paste(collapse = "") %>% 
        openssl::md5(key = "model_id") %>% 
        as.character
    },
    save_model_container = function(){
      
      self$results %>%
        as.list() %>%
        jsonlite::toJSON(force = T) %>%
        jsonlite::fromJSON() %>%
        jsonlite::toJSON(pretty = T, auto_unbox = T) %>%
        writeLines(con =  glue::glue("{private$new_path}/model_param.json"))
      
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
    param = NULL,
    splits = NULL,
    trainer = NULL,
    eval = NULL, 
    results = NULL,
    ### Main Function
    initialize = function(backend, folder = "") {
      
      private$set_backend(backend)
      private$create_id()
      
      if(!dir.exists("models")){
        dir.create("models")
      }

      if(folder != "") {
        private$new_path <- paste0("models/", folder, "/", self$param$model_id)
      } else {
        private$new_path <- glue::glue("models/{self$param$model_id}")
      }
      dir.create(private$new_path)
      
    },
    get_entry = function(name){
      return(list(private = private[[name]], self = self[[name]]) %>% compact %>% .[[1]])
    },
    glimpse_data = function(){
      return(self$data %>% map(glimpse))
    },
    set_data = function(x, y, id = NULL){
      
      self$data$x <- x[!is.na(y),]
      self$data$y <- y[!is.na(y)]
      
      if(is.null(id)) {
        self$data$id <- 1:length(y)
      } else {
        self$data$id <- id[!is.na(y)]
      }
      
    },
    set_param = function(list){
      self$param <- list
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
    train = function(){
    
      tictoc::tic()
      
      self$trainer$set(param = self$param, data = self$splits)
      self$trainer$param
      self$trainer$fit()
      
      time <- tictoc::toc(log = T)
      
      suppressMessages(
        self$param$duration <- as.numeric(time$toc - time$tic) %>% round(1)
      )
      
      self$param <- self$trainer$param
    },
    test = function(dev = F){
      
      preds <- self$trainer$predict()
      
      if(dev) return(preds)
      
      self$eval$eval(target = self$splits$test$target, pred = preds, id = self$splits$test$id)
      
      self$results <- self$param %>%
        keep(~is.atomic(.x) & length(.x) == 1) %>%
        bind_cols %>%
        mutate(metrics = list(self$eval$perform)) %>%
        mutate(timestamp = as.character(Sys.time()))
    },
    report = function(){
      ### save model container
      private$save_model_container()
      
      ### save plots
      ggsave_pos <- possibly(ggsave, NULL)
      self$eval$plots %>% 
        iwalk(~ggsave_pos(.x, file = glue::glue("{private$new_path}/{.y}.png")))
    }
  )
)


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
# transform_seq = function(text){
#   self$tokenizer %>% 
#     keras::texts_to_sequences(text) %>%
#     keras::pad_sequences(maxlen = self$param$seq_len)
# },
# predict_seq = function(text){
#   self$predict_new(self$transform_seq(text))
# },
# transform_dtm = function(text){
#   self$tokenizer %>%
#     keras::texts_to_matrix(text, mode = "binary")
# },
# predict_dtm = function(text){
#   self$predict_new(self$transform_dtm(text))
# },
# predict_new = function(newdata){
#   self$predict(self$param$model, newdata)
# },

# if(private$objective == "mixture"){
#   self$preds <- self$predict(self$param$model, self$splits$test$x, self$param$output_dim, self$param$mix_dim) 
# } else {
#   self$preds <- self$predict(self$param$model, self$splits$test$x)
# } else {
#   self$data$x <- x
#   self$data$id <- id
#   self$data$y <- y
# }

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
# overwrite_param = function(param){
#   param %>% iwalk(purrr::possibly(function(.x, .y){self$param[[.y]] <- .x}, NULL))
# },
# }