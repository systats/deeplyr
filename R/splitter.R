#' splitter
#' @export
splitter <- R6::R6Class("split",
  private = list(
    # variables
    x = NULL,
    y = NULL,
    id = NULL,
    # functions
    create_split = function(n_splits, p){
      
      self$split_id <- sample(1:n_splits, size = nrow(private$x), replace = T, prob = self$param$prior_p)
      
    },
    create_oos_split = function(n_splits, p){
      
      ids <- private$id %>% unique
      
      splitted <- sample(1:n_splits, size = length(ids), replace = T, prob = self$param$prior_p)
      
      self$split_id <- dplyr::case_when(
        private$id %in% ids[splitted == 1] ~ 1,
        private$id %in% ids[splitted == 2] ~ 2,
        private$id %in% ids[splitted == 3] ~ 3
      )
      
    },
    split_data = function(n_splits, balance = NULL, outcome_mat = F){
      self$splits <- 1:n_splits %>% 
        purrr::set_names(c("train", "test", "val")[1:n_splits]) %>%
        purrr::imap(~{
          
          
          x <- private$x[self$split_id == .x, ] %>% as.matrix
          target <- private$y[self$split_id == .x]
          
          if(!is.null(balance)) {
            if(balance == .x){
          
              x <- private$x[self$split_id == .x, ]# %>% as.matrix
              target <- private$y[self$split_id == .x]
                  
              x_sub <- private$sub_sample(x, target)
              target <- x_sub$target
              x <- x_sub$x %>% dplyr::select(-target) %>% as.matrix
            }
          }
          
          if(outcome_mat){
            print("dummies")
            y <- dummies::dummy(target) %>% as.matrix
          } else {
            print("no dummies")
            y <- target
          }
          
          return(list(x = x, y = y, target = target))
        })
    },
    sub_sample = function(x, y){
      y_min <- dplyr::tibble(y = y) %>% 
        dplyr::count(y, sort = T) %>% 
        dplyr::slice(2) %>% 
        dplyr::pull(n)
      
      x_sub <- x %>% 
        as_tibble() %>%
        dplyr::mutate(y = y) %>%
        dplyr::group_by(y) %>% 
        dplyr::sample_n(y_min) %>% 
        dplyr::ungroup() %>% 
        sample_n(n())
      
      return(x_sub)
    },
    callback = function(){
      p <- self$p %>% paste(collapse = '_')
      real_count <- self$splits %>% purrr::map_dbl(~length(.x$y))
      real_p <- round(real_count / sum(real_count), 3)
      real <- paste(self$param$real_p, collapse = '_')
      self$param$real_p <- as.vector(real_p)
      self$param$real_count <- as.vector(real_count)
      
      cat(glue::glue("seed={ self$seed } prior={ p } real={ real }\n\n"))
    }
  ),
  public = list(
    # variables
    param = NULL, 
    split_id = NULL,
    splits = NULL,
    # functions
    initialize = function() {
      
    },
    set = function(x, y, id = NULL){
      
      na_target <- is.na(y)
      private$y <- y[!na_target]
      private$x <- x[!na_target, ]
      private$id <- id[!na_target]
      
    },
    split = function(val = F, oos = F, p = NULL, seed = NULL, balance = NULL, outcome_mat = F){
      
      self$param$val <- val
      self$param$oos <- oos
      
      if(is.null(seed)) seed <- 42
      set.seed(seed)
      self$param$seed <- seed
      
      if(val){
        n_splits <- 3
        if(is.null(p)) p <- c(.8, .1, .1)
      } else {
        n_splits <- 2
        if(is.null(p)) p <- c(.8, .2)
      }
      self$param$prior_p <- p
      
      if(is.null(private$id)){
        private$create_split(n_splits)
      } else {
        self$split_id <- private$id
      }
      
      if(oos & is.null(private$id)) message("please provide an ID variable")
      if(oos & !is.null(private$id)) private$create_oos_split(n_splits)
      
      private$split_data(n_splits, balance, outcome_mat)
      private$callback()
    }
  )
)




#' #' splitter
#' #' 
#' #' @export
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
#'     split_all = function(){
#'       private$split_data()
#'       private$split_count()
#'       private$split_y()
#'       private$split_x()
#'     },
#'     split_data = function(){
#'       private$train <- private$data %>% filter(private$split_id == 1)
#'       private$test <- private$data %>% filter(private$split_id == 2)
#'       private$val <- private$data %>% filter(private$split_id == 3)
#'       if(nrow(private$val) == 0) private$val <- NULL
#'     },
#'     split_y = function(){
#'       
#'       if(private$param$output_dim == 1){
#'         private$y_train <- private$train[[private$param$target]]
#'         private$y_test <- private$test[[private$param$target]]
#'         if(!is.null(private$val)) private$y_val <- private$val[[private$param$target]]
#'       } else {
#'         private$y_train <-  dummies::dummy(private$train[[private$param$target]])
#'         private$y_test <-  dummies::dummy(private$test[[private$param$target]])
#'         if(!is.null(private$val)) private$y_val <-  dummies::dummy(private$val[[private$param$target]])
#'       }
#'     },
#'     split_x = function(){
#'       private$x_train <- private$x[private$split_id == 1,]
#'       private$x_test <- private$x[private$split_id == 2,]
#'       if(!is.null(private$val)) private$x_val <- private$x[private$split_id == 3,]
#'     },
#'     split_count = function(){
#'       private$param$train_size <- nrow(private$train)
#'       private$param$test_size <- nrow(private$test)
#'       if(!is.null(private$val)) private$param$val_size <- nrow(private$val)
#'     },
#'     split_train_test = function(prob){
#'       
#'       if(is.null(prob)) prob <- c(.8, .2)
#'       
#'       private$split_id <- sample(c(1:2), size = nrow(private$data), replace = T, prob = prob)
#'       
#'       private$split_all()
#'       
#'       private$param$split_mode <- glue::glue("train_test_{paste(prob, collapse = '_')}")
#'     },
#'     split_train_test_val = function(prob){
#'       
#'       if(is.null(prob)) prob <- c(.8, .1, .1)
#'       
#'       private$split_id <- sample(c(1:3), size = nrow(private$data), replace = T, prob = prob) #.01, .04
#'       
#'       private$split_all()
#'       
#'       private$param$split_mode <- glue::glue("train_test_val_{paste(prob, collapse = '_')}")
#'     },
#'     # split out of sample (oos)
#'     split_train_test_oos = function(prob, id){
#'       if(is.null(prob)) prob <- c(.8, .2)
#'       
#'       ids <- private$data[[id]] %>% unique
#'       splitted <- sample(c(1:2), size = length(ids), replace = T, prob = prob) #.01, .04
#'       
#'       private$split_id <- case_when(
#'         private$data[[id]] %in% ids[splitted == 1] ~ 1,
#'         private$data[[id]] %in% ids[splitted == 2] ~ 2
#'       )
#'       
#'       private$split_all()
#'       
#'       private$param$split_mode <- glue::glue("train_test_oos_{paste(prob, collapse = '_')}")
#'     },
#'     split_train_test_val_oos = function(prob, id){
#'       
#'       if(is.null(prob)) prob <- c(.8, .1, .1)
#'       
#'       ids <- private$data[[id]] %>% unique
#'       splitted <- sample(c(1:3), size = length(ids), replace = T, prob = prob) #.01, .04
#'       
#'       private$split_id <- case_when(
#'         private$data[[id]] %in% ids[splitted == 1] ~ 1,
#'         private$data[[id]] %in% ids[splitted == 2] ~ 2,
#'         private$data[[id]] %in% ids[splitted == 3] ~ 3
#'       )
#'       
#'       private$split_all()
#'       
#'       private$param$split_mode <- glue::glue("train_test_val_oos{paste(prob, collapse = '_')}")
#'     }
#'   ),
#'   public = list(
#'     set_split = function(val = F, oos = F, prob = NULL, id = NULL, by_index = NULL){
#'       
#'       set.seed(42)
#'       
#'       na_target <- is.na(private$data[[private$param$target]])
#'       
#'       # print(nrow(private$data))
#'       # print(any(na_target))
#'       
#'       if(any(na_target)){
#'         private$x <- private$x[!na_target, ]
#'         private$data <- private$data[!na_target, ]
#'       }
#'       
#'       #print(nrow(private$data))
#'       
#'       if(oos){
#'         if(val){
#'           private$split_train_test_val_oos(prob, id)
#'         } else {
#'           private$split_train_test_oos(prob, id)
#'         }
#'       } else if(!oos & is.null(by_index)) {
#'         if(val){
#'           private$split_train_test_val(prob)
#'         } else {
#'           private$split_train_test(prob)
#'         }
#'       }
#'       if(!is.null(by_index)){
#'         private$split_id <- private$data[[by_index]]
#'         private$split_all()
#'         private$param$split_mode <- "train_test_by_index"
#'       }
#'     }
#'   )
#' )
