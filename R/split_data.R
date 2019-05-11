#' #' splitter
#' #' 
#' #' @export
#' 
#' 
splitter <- R6::R6Class("split_data",
  private = list(
    split_id = NULL,
    train = NULL,
    test = NULL,
    val = NULL,
    x_train = NULL,
    x_val = NULL,
    x_test = NULL,
    y_train = NULL,
    y_val = NULL,
    y_test = NULL,
    split_all = function(){
      private$split_data()
      private$split_y()
      private$split_x()
      private$split_count()
    },
    split_data = function(){
      private$train <- private$data[private$split_id == 1,]
      private$test <- private$data[private$split_id == 2,]
      private$val <- private$data[private$split_id == 3,]
      if(nrow(private$val) == 0) private$val <- NULL
    },
    split_y = function(){

      if(private$param$output_dim == 1){
        private$y_train <- private$train[[private$param$target]]
        private$y_test <- private$test[[private$param$target]]
        if(!is.null(private$val)) private$y_val <- private$val[[private$param$target]]
      } else {
        private$y_train <-  dummies::dummy(private$train[[private$param$target]])
        private$y_test <-  dummies::dummy(private$test[[private$param$target]])
        if(!is.null(private$val)) private$y_val <-  dummies::dummy(private$val[[private$param$target]])
      }
    },
    split_x = function(){
      private$x_train <- private$x[private$split_id == 1,]
      private$x_test <- private$x[private$split_id == 2,]
      if(!is.null(private$val)) private$x_val <- private$x[private$split_id == 3,]
    },
    split_count = function(){
      private$param$train_size <- nrow(private$train)
      private$param$test_size <- nrow(private$test)
      if(!is.null(private$val)) private$param$val_size <- nrow(private$val)
    },
    split_train_test = function(prob){
      
      if(is.null(prob)) prob <- c(.8, .2)
      
      private$split_id <- sample(c(1:2), size = nrow(private$data), replace = T, prob = prob)
      
      private$split_all()
      
      private$param$split_mode <- glue::glue("train_test_{paste(prob, collapse = '_')}")
    },
    split_train_test_val = function(prob){
      
      if(is.null(prob)) prob <- c(.8, .1, .1)
      
      private$split_id <- sample(c(1:3), size = nrow(private$data), replace = T, prob = prob) #.01, .04
      
      private$split_all()
      
      private$param$split_mode <- glue::glue("train_test_val_{paste(prob, collapse = '_')}")
    },
    # split out of sample (oos)
    split_train_test_oos = function(prob, id){
      if(is.null(prob)) prob <- c(.8, .2)
      
      ids <- private$data %>%
        dplyr::count_(id) %>%
        pull(!!id)
      
      splitted <- sample(c(1:2), size = length(ids), replace = T, prob = prob) #.01, .04
      
      private$split_id <- case_when(
        private$data[[id]] %in% ids[splitted == 1] ~ 1,
        private$data[[id]] %in% ids[splitted == 2] ~ 2
      )
      
      private$split_all()
      
      private$param$split_mode <- glue::glue("train_test_oos_{paste(prob, collapse = '_')}")
    },
    split_train_test_val_oos = function(prob, id){
      
      if(is.null(prob)) prob <- c(.8, .1, .1)
      
      ids <- private$data %>%
        dplyr::count_(id) %>%
        pull(!!id)
      
      splitted <- sample(c(1:3), size = length(ids), replace = T, prob = prob) #.01, .04
      
      private$split_id <- case_when(
        private$data[[id]] %in% ids[splitted == 1] ~ 1,
        private$data[[id]] %in% ids[splitted == 2] ~ 2,
        private$data[[id]] %in% ids[splitted == 3] ~ 3
      )
      
      private$split_all()
      
      private$param$split_mode <- glue::glue("train_test_val_oos{paste(prob, collapse = '_')}")
    }
  ),
  public = list(
    split = function(val = F, oos = F, prob = NULL, id = NULL){
      
      na_target <- is.na(private$data[[private$param$target]])
      
      # print(nrow(private$data))
      # print(any(na_target))
      
      if(any(na_target)){
        private$x <- private$x[!na_target, ]
        private$data <- private$data[!na_target, ]
      }
      
      #print(nrow(private$data))
      
      if(oos){
        if(val){
          private$split_train_test_val_oos(prob, id)
        } else {
          private$split_train_test_oos(prob, id)
        }
      } else {
        if(val){
          private$split_train_test_val(prob)
        } else {
          private$split_train_test(prob)
        }
      }
    }
  )
)







#' splitter <- R6::R6Class("split_data",
#'   private = list(
#'     split_id = NULL,
#'     train = NULL,
#'     test = NULL,
#'     val = NULL,
#'     x_train = NULL,
#'     x_val = NULL,
#'     x_test = NULL,
#'     y_train = NULL,
#'     y_val = NULL,
#'     y_test = NULL,
#'     split_count = function(){
#'       private$param$train_size <- nrow(private$train)
#'       private$param$test_size <- nrow(private$test)
#'     },
#'     split_data = function(){
#'       private$train <- private$data[private$split_id == 1,]
#'       private$test <- private$data[private$split_id == 2,]
#'       private$val <- private$data[private$split_id == 3,]
#'       if(nrow(private$val) == 0) private$val <- NULL
#'     },
#'     split_x = function(){
#'       private$x_train <- private$x[private$split_id == 1,]
#'       private$x_test <- private$x[private$split_id == 2,]
#'       private$x_val <- private$x[private$split_id == 3,]
#'       if(nrow(private$x_val) == 0) private$x_val <- NULL
#'     },
#'     split_y = function(){
#'       if(private$param$output_dim == 1){
#'         private$y_train <- private$train[[private$param$target]]
#'         private$y_test <- private$test[[private$param$target]]
#'         private$y_val <- private$val[[private$param$target]]
#'       } else {
#'         private$y_train <-  dummies::dummy(private$train[[private$param$target]])
#'         private$y_test <-  dummies::dummy(private$test[[private$param$target]])
#'         private$y_val <-  dummies::dummy(private$val[[private$param$target]])
#'         if(length(private$y_val) == 0) private$y_val <- NULL
#'       }
#'     },
#'     split_train_test = function(prob){
#'       
#'       if(is.null(prob)) prob <- c(.8, .2)
#'       
#'       private$split_id <- sample(c(1:2), size = nrow(private$data), replace = T, prob = prob)
#' 
#'       private$split_all()
#'       private$split_y()
#'       private$split_x()
#'       private$split_count()
#'     
#'       private$param$split_mode <- glue::glue("train_test_{paste(prob, collapse = '_')}")
#'     },
#'     split_train_test_val = function(prob){
#'       
#'       if(is.null(prob)) prob <- c(.8, .1, .1)
#'       
#'       private$split_id <- sample(c(1:3), size = nrow(private$data), replace = T, prob = prob) #.01, .04
#'       
#'       private$split_data()
#'       private$split_y()
#'       private$split_x()
#'       private$split_count()
#'       
#'       private$param$split_mode <- glue::glue("train_test_val_{paste(prob, collapse = '_')}")
#'     },
#'     # split out of sample (oos)
#'     split_train_test_oos = function(prob, id){
#'       if(is.null(prob)) prob <- c(.8, .2)
#'       
#'       ids <- private$data %>%
#'         dplyr::count_(id) %>%
#'         pull(!!id)
#'       
#'       splitted <- sample(c(1:2), size = length(ids), replace = T, prob = prob) #.01, .04
#'       
#'       private$split_id <- case_when(
#'         private$data[[id]] %in% ids[splitted == 1] ~ 1,
#'         private$data[[id]] %in% ids[splitted == 2] ~ 2
#'       )
#'       
#'       private$split_all()
#'       private$split_y()
#'       private$split_x()
#'       private$split_count()
#'       
#'       private$param$split_mode <- glue::glue("train_test_oos_{paste(prob, collapse = '_')}")
#'     },
#'     split_train_test_val_oos = function(prob, id){
#'       
#'       if(is.null(prob)) prob <- c(.8, .1, .1)
#'       
#'       ids <- private$data %>%
#'         dplyr::count_(id) %>%
#'         pull(!!id)
#'       
#'       splitted <- sample(c(1:3), size = length(ids), replace = T, prob = prob) #.01, .04
#'       
#'       private$split_id <- case_when(
#'         private$data[[id]] %in% ids[splitted == 1] ~ 1,
#'         private$data[[id]] %in% ids[splitted == 2] ~ 2,
#'         private$data[[id]] %in% ids[splitted == 3] ~ 3
#'       )
#'       
#'       private$split_all()
#'       private$split_y()
#'       private$split_x()
#'       private$split_count()
#'       
#'       private$param$split_mode <- glue::glue("train_test_val_oos{paste(prob, collapse = '_')}")
#'     }
#'   ),
#'   public = list(
#'     split = function(val = F, oos = F, prob = NULL, id = NULL){
#'       
#'       na_target <- is.na(private$data[[private$param$target]])
#'       
#'       print(nrow(private$data))
#'       print(any(na_target))
#'       
#'       if(any(na_target)){
#'         private$x <- private$x[!na_target, ]
#'         private$data <- private$data[!na_target, ]
#'       }
#'       
#'       print(nrow(private$data))
#'       
#'       if(oos){
#'         if(val){
#'           private$split_train_test_val_oos(prob, id)
#'         } else {
#'           private$split_train_test_oos(prob, id)
#'         }
#'       } else {
#'         if(val){
#'           private$split_train_test_val(prob)
#'         } else {
#'           private$split_train_test(prob)
#'         }
#'       }
#'     }
#'   )
#' )
#' 
#' #' #' splitter
#' #' #' 
#' #' #' @export
#' #' splitter <- R6::R6Class("split_data",
#' #'                         private = list(
#' #'                           split_id = NULL,
#' #'                           train = NULL,
#' #'                           test = NULL,
#' #'                           val = NULL,
#' #'                           x_train = NULL,
#' #'                           x_val = NULL,
#' #'                           x_test = NULL,
#' #'                           y_train = NULL,
#' #'                           y_val = NULL,
#' #'                           y_test = NULL,
#' #'                           split_all = function(){
#' #'                             private$train <- private$data[private$split_id == 1,]
#' #'                             private$test <- private$data[private$split_id == 2,]
#' #'                             private$val <- private$data[private$split_id == 3,]
#' #'                             if(nrow(private$val) == 0) private$val <- NULL
#' #'                           },
#' #'                           split_x = function(){
#' #'                             private$x_train <- private$x[private$split_id == 1,]
#' #'                             private$x_test <- private$x[private$split_id == 2,]
#' #'                             private$x_val <- private$x[private$split_id == 3,]
#' #'                           },
#' #'                           split_y = function(){
#' #'                             if(private$params$output_dim == 1){
#' #'                               private$y_train <- private$train[[private$params$target]]
#' #'                               private$y_test <- private$test[[private$params$target]]
#' #'                               private$y_val <- private$val[[private$params$target]]
#' #'                             } else {
#' #'                               private$y_train <-  dummies::dummy(private$train[[private$params$target]])
#' #'                               private$y_test <-  dummies::dummy(private$test[[private$params$target]])
#' #'                               private$y_val <-  dummies::dummy(private$val[[private$params$target]])
#' #'                             }
#' #'                           },
#' #'                           split_train_test = function(prob){
#' #'                             
#' #'                             if(is.null(prob)) prob <- c(.8, .2)
#' #'                             
#' #'                             private$split_id <- sample(c(1, 2), size = nrow(private$data), replace = T, prob = prob) #.01, .04
#' #'                             
#' #'                             private$split_all()
#' #'                             private$split_x()
#' #'                             private$split_y()
#' #'                             
#' #'                             private$params$split_mode <- glue::glue("train_test_{paste(prob, collapse = '_')}")
#' #'                           },
#' #'                           split_train_test_val = function(prob){
#' #'                             
#' #'                             if(is.null(prob)) prob <- c(.8, .1, .1)
#' #'                             
#' #'                             private$split_id <- sample(c(1:3), size = nrow(private$data), replace = T, prob = prob) #.01, .04
#' #'                             
#' #'                             private$split_all()
#' #'                             private$split_x()
#' #'                             private$split_y()
#' #'                             
#' #'                             private$params$split_mode <- glue::glue("train_test_val_{paste(prob, collapse = '_')}")
#' #'                           },
#' #'                           # split out of sample (oos)
#' #'                           split_train_test_oos = function(prob, id){
#' #'                             if(is.null(prob)) prob <- c(.8, .2)
#' #'                             
#' #'                             ids <- private$data %>%
#' #'                               dplyr::count_(id) %>%
#' #'                               pull(!!id)
#' #'                             
#' #'                             splitted <- sample(c(1:2), size = length(ids), replace = T, prob = prob) #.01, .04
#' #'                             
#' #'                             private$split_id <- case_when(
#' #'                               private$data[[id]] %in% ids[splitted == 1] ~ 1,
#' #'                               private$data[[id]] %in% ids[splitted == 2] ~ 2
#' #'                             )
#' #'                             
#' #'                             private$split_all()
#' #'                             private$split_x()
#' #'                             private$split_y()
#' #'                             
#' #'                             private$params$split_mode <- glue::glue("train_test_oos_{paste(prob, collapse = '_')}")
#' #'                           },
#' #'                           split_train_test_val_oos = function(prob, id){
#' #'                             
#' #'                             if(is.null(prob)) prob <- c(.8, .1, .1)
#' #'                             
#' #'                             ids <- private$data %>%
#' #'                               dplyr::count_(id) %>%
#' #'                               pull(!!id)
#' #'                             
#' #'                             splitted <- sample(c(1:3), size = length(ids), replace = T, prob = prob) #.01, .04
#' #'                             
#' #'                             private$split_id <- case_when(
#' #'                               private$data[[id]] %in% ids[splitted == 1] ~ 1,
#' #'                               private$data[[id]] %in% ids[splitted == 2] ~ 2,
#' #'                               private$data[[id]] %in% ids[splitted == 3] ~ 3
#' #'                             )
#' #'                             
#' #'                             private$split_all()
#' #'                             private$split_x()
#' #'                             private$split_y()
#' #'                             
#' #'                             private$params$split_mode <- glue::glue("train_test_val_oos{paste(prob, collapse = '_')}")
#' #'                           }
#' #'                         ),
#' #'                         public = list(
#' #'                           # initialize = function() {
#' #'                           # },
#' #'                           split_data = function(out_of_sample = F, val_split = F, balance_test = F, id = "voter_id", prob = NULL){
#' #'                             
#' #'                             na_target <- is.na(private$data[[private$params$target]])
#' #'                             
#' #'                             if(any(na_target)){
#' #'                               private$x <- private$x[!na_target, ]
#' #'                               private$data <- private$data[!na_target, ]
#' #'                             }
#' #'                             
#' #'                             if(out_of_sample){
#' #'                               if(val_split){
#' #'                                 private$split_train_test_val_oos(prob, id)
#' #'                               } else {
#' #'                                 private$split_train_test_oos(prob, id)
#' #'                               }
#' #'                             } else {
#' #'                               if(val_split){
#' #'                                 private$split_train_test_val(prob)
#' #'                               } else {
#' #'                                 private$split_train_test(prob)
#' #'                               }
#' #'                             }
#' #'                             
#' #'                             private$params$train_size <- nrow(private$train)
#' #'                             private$params$test_size <- nrow(private$test)
#' #'                           }
#' #'                         )
#' #' )
