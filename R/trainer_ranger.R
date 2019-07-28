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
     
     if(private$objective == "linear"){
       x_train <- self$data$train$x %>%
         as_tibble() %>%
         set_names(paste0("V",colnames(.))) %>%
         mutate(y = self$data$train$y) %>%
         droplevels
       
       self$model <- ranger::ranger(y ~ ., data = x_train, importance = "impurity_corrected")
     } else {
       x_train <- self$data$train$x %>%
         as_tibble() %>%
         set_names(paste0("V",colnames(.))) %>%
         mutate(y = as.factor(self$data$train$y)) %>%
         droplevels
       
       self$model <- ranger::ranger(y ~ ., data = x_train, probability = T, importance = "impurity_corrected")
     }

   },
   predict = function(x_test = NULL){
     
     if(is.null(x_test)) x_test <- self$data$test$x
     if(is.null(x_test)) return(message("No new data found"))
     
     x_test <- x_test %>%
        as_tibble() %>%
        set_names(paste0("V", colnames(.))) %>%
        droplevels
     
     pred <- predict(self$model, x_test)$predictions # take only second column for 1s
     
     if(private$objective == "linear"){
       pred <- pred[,1]
     } else if(private$objective == "binary"){
       pred <- pred[,2]
     }
     
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

