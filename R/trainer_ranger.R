#' trainer_ranger
#'  @export
trainer_ranger <- R6::R6Class("trainer_ranger",
  private = list(
    objective = NULL
  ),
  public = list(
   model = NULL,
   param = NULL,
   data = NULL,
   initialize = function(objective) {
     private$objective <- objective
   },
   set = function(param, data){
     
     self$data <- data
     self$param <- c(self$param, param)
     
   },
   fit = function(){
      
      if(is.null(self$param$num_trees)) self$param$num_trees <- 500
      #max.depth = NULL 0, 1
      # mtry
     
     if(private$objective == "linear"){
       x_train <- self$data$train$x %>%
         mutate(y = self$data$train$y)
       
       self$model <- ranger::ranger(
          formula = y ~ .,
          data = x_train, 
          importance = "impurity",
          num.trees = self$param$num_trees,
          mtry = self$param$mtry,
          min.node.size = self$param$min_node_size,
          verbose = T,
          seed = 42
      )
       
       #self$model <- ranger::ranger(y ~ ., data = x_train, importance = "impurity_corrected")
       
     } else {
       x_train <- self$data$train$x %>%
         mutate(y = self$data$train$y)
       
       self$model <- ranger::ranger(
          formula = y ~ .,
          data = x_train, 
          importance = "impurity",
          num.trees = self$param$num_trees,
          mtry = self$param$mtry,
          min.node.size = self$param$min_node_size,
          verbose = T,
          seed = 42
       )
       
       #self$model <- ranger::ranger(y ~ ., data = x_train, probability = T, importance = "impurity_corrected")
     }

   },
   predict = function(x_test = NULL){
     
     if(is.null(x_test)) x_test <- self$data$test$x
     if(is.null(x_test)) return(message("No new data found"))
     
     # x_test <- x_test %>%
     #    as_tibble() %>%
     #    #set_names(paste0("V", colnames(.))) %>%
     #    #droplevels
     
     pred <- predict(self$model, x_test)$predictions # take only second column for 1s
     
     # if(private$objective == "linear"){
     #   pred <- pred[,1]
     # } else if(private$objective == "binary"){
     #   pred <- pred[,1]
     # }

     return(pred)
   }
  )
)



#' #' @export
#' predict_ranger_linear <- function(model, x_test){
#'   
#'   x_test <- x_test %>%
#'     as_tibble() %>%
#'     set_names(paste0("V", colnames(.)))
#'   
#'   return(predict(model, x_test)$predictions) # take only second column for 1s
#' }
#' 
#' 
#' #' @export
#' predict_ranger_binary <- function(model, x_test){
#'   
#'   x_test <- x_test %>%
#'     as_tibble() %>%
#'     set_names(paste0("V", colnames(.)))
#'   
#'   return(predict(model, x_test)$predictions[,-1]) # take only second column for 1s
#' }
#' 
#' 
#' #' @export
#' predict_ranger_categorical <- function(model, x_test){
#'   
#'   x_test <- x_test %>%
#'     as_tibble() %>%
#'     set_names(paste0("V", colnames(.)))
#'   
#'   return(predict(model, x_test)$predictions) # take only second column for 1s
#' }

