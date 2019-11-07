#' splitter
#' @export
splitter <- R6::R6Class("split",
  private = list(
    # variables
    x = NULL,
    y = NULL,
    id = NULL,
    meta = NULL,
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
        purrr::set_names(c("train", "test", "val", "bet")[1:n_splits]) %>%
        purrr::imap(~{
          
          x <- private$x[self$split_id == .x, ]# %>% as.matrix
          target <- private$y[self$split_id == .x]
          id <- private$id[self$split_id == .x]
          meta <- private$meta[self$split_id == .x, ]
          
          return(list(x = x, y = target, target = target, id = id, meta = meta))
        })
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
    set = function(x, y, id = NULL, meta = NULL){
      
      private$x <- x
      private$y <- y
      private$id <- id
      private$meta <- meta
      
    },
    split = function(val = F, bet = F, oos = F, p = NULL, seed = NULL, balance = NULL, outcome_mat = F){
      
      self$param$val <- val
      self$param$bet <- bet
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
      
      if(bet){
        n_splits <- n_splits + 1
      }
      
      self$param$prior_p <- p
      
      if(length(private$id) == length(unique(private$id))){
        private$create_split(n_splits)
      } else {
        self$split_id <- private$id
      }

      #if(oos & is.null(private$id)) message("please provide an ID variable")
      if(oos & !is.null(private$id)) private$create_oos_split(n_splits)
      
      private$split_data(n_splits, balance, outcome_mat)
      private$callback()
    }
  )
)

#' generate_split
#' @export
generate_split <- function(x, y, id, meta){
  sp <- deeplyr::splitter$new()
  sp$set(x, y, id, meta)
  sp$split(val = T, bet = T)
  return(sp$splits)
}