#' splitter
#' 
#' @export
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
                            private$split_count()
                            private$split_y()
                            private$split_x()
                          },
                          split_data = function(){
                            private$train <- private$data %>% filter(private$split_id == 1)
                            private$test <- private$data %>% filter(private$split_id == 2)
                            private$val <- private$data %>% filter(private$split_id == 3)
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
                            
                            ids <- private$data[[id]] %>% unique
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
                            
                            ids <- private$data[[id]] %>% unique
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
                          set_split = function(val = F, oos = F, prob = NULL, id = NULL, by_index = NULL){
                            
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
                            } else if(!oos & is.null(by_index)) {
                              if(val){
                                private$split_train_test_val(prob)
                              } else {
                                private$split_train_test(prob)
                              }
                            }
                            if(!is.null(by_index)){
                              private$split_id <- private$data[[by_index]]
                              private$split_all()
                              private$param$split_mode <- "train_test_by_index"
                            }
                          }
                        )
)
