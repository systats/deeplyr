#' splitter
#' @export
splitter <- R6::R6Class("split",
  public = list(
    # variables
    x = NULL,
    y = NULL,
    target = NULL,
    id = NULL,
    meta = NULL,
    param = NULL,
    splits = NULL,
    # functions
    initialize = function() {
      
    },
    set = function(x, y, target = NULL, id = NULL, meta = NULL){
      
      self$x <- x
      self$y <- y
      self$id <- id
      self$meta <- meta
      self$target <- target
      
    },
    split_data = function(){
      self$splits <- 1:self$param$n_splits %>% 
        purrr::set_names(c("train", "test", "val", "bet")[1:self$param$n_splits]) %>%
        purrr::imap(~{
          
          x <- self$x[self$id == .x, ]
          y <- self$y[self$id == .x]
          
          if(is.null(self$target)){
            target <- self$y[self$id == .x]
          } else {
            target <- self$target[self$id == .x]
          }
          
          id <- self$id[self$id == .x]
          meta <- self$meta[self$id == .x, ]
          
          return(list(x = x, y = y, target = target, id = id, meta = meta))
        })
    },
    split = function(){
      
      self$param$n_splits <- length(unique(self$id))
      
      self$split_data()
    }
  )
)

#' split_sample
#' @export
split_sample <- function(x, y, target = NULL, id = NULL, meta = NULL){
  sp <- deeplyr::splitter$new()
  sp$set(x, y, target, id, meta)
  sp$split()
  return(sp$splits)
}