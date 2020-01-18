#' learner
#' @export
learner <- R6::R6Class(
  
  inherit = backend,
  
  public = list(
    ### data slots
    meta = NULL,
    params = NULL,
    process = NULL,
    model = NULL,
    imps = NULL,
    preds = NULL,
    metrics = NULL, 
    
    ### extra
    tokenizer = NULL, 
    
    ### main functions
    initialize = function(params, task = NULL, backend = NULL, meta = NULL){
      
      if(is.null(task)){
        
        self$meta <- load_json(params, "meta")
        self$params <- load_json(params, "params")
        self$process <- bridge$new(load_rds(params, "process"))
        
        private$model_backend()
        
        self$model <- private$model_load(params)
        
        if(file.exists(glue::glue("{params}/tok"))) self$tokenizer <- load_tokenizer(params)
        
      } else {
        
        self$process <- bridge$new()
        self$params <- params
        
        if(!is.null(meta)){
          self$meta <- meta
        } else {
          self$meta$task <- task
          self$meta$backend <- backend
          
          private$model_backend()
        }
      }
    },

    
    fit = function(x, y){
      
      ### freeze training data + pre-processing steps
      self$process$bake(x, y)

      start <- Sys.time()

      self$model <- private$model_fit(self)

      self$meta$runtime <- as.numeric(Sys.time() - start)

      ### if avaible: feature importane
      if(!is.null(private$imp_model)) self$imps <- private$model_imp(self)
    },
    
    predict = function(new_data, dev = F){
      
      self$preds <- private$model_predict(self, new_data) %>% 
        dplyr::bind_cols(self$process$stream_all(new_data))
      
      if(dev) return(self$preds)
      
      if(self$process$ask_y() %in% colnames(as_tibble(new_data))){
        self$metrics <- model_eval(self, self$process$ask_y())
      }
    },
    
    predict_feature = function(new_data, suffix){
      
      yname <- self$process$ask_y() %>% 
        stringr::str_remove("^local_|^visitor_") %>% 
        stringr::str_remove_all("_")
      
      private$model_predict(self, new_data) %>%
        dplyr::select(-dplyr::contains("team_id")) %>%
        ### apply prefix other than team ids
        dplyr::rename_all(~ paste0(yname, "_", .x)) %>%
        ### reoreder local|visitor label
        dplyr::rename_all(suf_to_pref) %>%
        ### apply suffix
        dplyr::rename_at(-1, ~ paste0(.x, "_", self$params$type, "_", suffix)) %>%
        ### combine all data
        cbind(self$process$stream_all(new_data), .)

    },
    
    # test = function(){
    #   # if(self$meta$task == "linear"){
    #   #   self$metrics <- self$preds %>% 
    #   #     yardstick::metrics(truth = .data[[actual]], estimate =  pred)
    #   # } else {
    #   #   self$metrics <- self$preds %>% 
    #   #     yardstick::metrics(truth = .data[[actual]], estimate = pred, contains("prob"))
    #   #} 
    # },
    
    save = function(path = NULL){
      
      if(is.null(path)) path <- "."
      if(!dir.exists(path)) dir.create(path)
      
      ### model
      private$model_save <- purrr::possibly(private$model_save, NULL)
      private$model_save(self$model, "model", path)
      
      ### meta
      if(!is.null(self$meta)) save_json_pos(self$meta, "meta", path)
      
      ### params
      params <- self$params %>% purrr::keep(~is.numeric(.x)|is.character(.x))
      if(!is.null(params)) save_json_pos(params, "params", path)

      ### recipe
      if(!is.null(self$process$data)) save_rds_pos(self$process$data, "process", path)
            
      ### metrics
      if(!is.null(self$metrics)) save_json_pos(self$metrics, "metrics", path)
      
      ### preds
      if(!is.null(self$preds)) save_rds_pos(self$preds, "preds", path)
      
      ### imps
      if(!is.null(self$imps)) save_rds_pos(self$imps, "imps", path)
    }
  )
)


#' fit_learner
#' @export
fit_learner <- function(x, y, params, task, backend){
  g <- learner$new(params, task, backend)
  g$fit(x, y)
  return(g)
}

#' fit_cv
#' @export
fit_cv <- function(rsample, rec, params, task, backend){
  rsample %>%
    dplyr::mutate(models = map(splits, ~{
      g <- deeplyr::fit_learner(
        rec, dplyr::bind_rows(rsample::analysis(.x)), 
        params, task, backend
      )
      g$predict(new_data = dplyr::bind_rows(rsample::assessment(.x)))
      return(g)
    })
    ) %>% #furrr::future_  #, .progress = F
    dplyr::mutate(
      preds = purrr::map(models, ~.x$preds),
      metrics = purrr::map(models, ~.x$metrics)
    )
}



#' future_fit_cv
#' @export
future_fit_cv <- function(rsample, rec, params, task, backend){
  rsample %>%
    dplyr::mutate(models = furrr::future_map(splits, ~{
      g <- deeplyr::fit_learner(
        x = rec, y = dplyr::bind_rows(rsample::analysis(.x)), 
        params, task, backend
      )
      g$predict(new_data = dplyr::bind_rows(rsample::assessment(.x)))
      return(g)
    }, .progress = T)
    ) %>% #furrr::future_  #, .progress = F
    dplyr::mutate(
      preds = purrr::map(models, ~.x$preds),
      metrics = purrr::map(models, ~.x$metrics)
    )
}

#' fit_tcv
#' @export
fit_tcv <- function(rsample, rec, params, task, backend){
  rsample %>%
    dplyr::mutate(models = map(splits, ~{
      g <- deeplyr::fit_learner(
        rec, dplyr::bind_rows(rsample::analysis(.x)$data), 
        params, task, backend
      )
      g$predict(new_data = dplyr::bind_rows(rsample::assessment(.x)$data))
      return(g)
    })
    ) %>% #furrr::future_  #, .progress = F
    dplyr::mutate(
      preds = purrr::map(models, ~.x$preds),
      metrics = purrr::map(models, ~.x$metrics)
    )
}



