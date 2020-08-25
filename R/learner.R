#' learner
#' @export
learner <- R6::R6Class(
  
  inherit = backend,
  active = list(
    set_metrics = function(metrics){
      self$metrics <- metrics
    }
  ),
  public = list(
    
    ### data slots
    meta = NULL,
    params = NULL,
    model = NULL,
    imps = NULL,
    preds = NULL,
    metrics = NULL, 
    
    ### main functions
    initialize = function(params, task = NULL, backend = NULL, path = NULL){
      
      dir.exists_pos <- purrr::possibly(dir.exists, F)
      p <- params[1]
      if(dir.exists_pos(p)){
        self$meta <- meta$new(p)
        self$params <- load_params(p)
        private$model_backend()
        if(file.exists(glue::glue("{p}/model"))) self$model <- private$model_load(p)
      } else {
        self$meta <- meta$new()
        self$params <- params
        
        self$meta$set("task",  task)
        self$meta$set("backend",  backend)
        private$model_backend()
      }
    },
    
    set = function(key, value){
      self[[key]] <- value
    },
  
    fit = function(x, y){
      
      ### freeze training data + pre-processing steps
      #  self$process$bake(x, y)
       
      self$meta$feed(x, y, params = self$params)
      
      self$model <- private$model_fit(self)

      self$meta$set("runtime", round(as.numeric(Sys.time() - self$meta$timestamp), 2))

      if(!is.null(private$model_imp)) self$imps <- private$model_imp(self)
    },
    
    predict = function(new_data, y_test = NULL, dev = F, add_x = F){
      
      nr <- nrow(new_data)
      if(is.null(nr)) nr <- length(new_data)
      self$meta$set("n_test",  nr)
      
      self$preds <- private$model_predict(self, self$meta$stream(new_data))
      
      if(add_x) self$preds <- cbind(self$preds, new_data)
      
      if(dev) return(self$preds)
      if(!is.null(y_test)) self$metrics <- model_eval(self, y_test)
    },

    save = function(path = NULL){
      
      path <- glue::glue("{path}/{self$meta$model_id}")
      if(!dir.exists(path)) dir.create(path)
      
      ### model
      private$model_save <- purrr::possibly(private$model_save, NULL)
      private$model_save(self$model, "model", path)
      
      ### meta & params
      self$meta$get() %>% save_json_pos("meta", path)
      self$params %>% 
        purrr::keep(~is.numeric(.x)|is.character(.x)) %>% 
        save_json_pos("params", path)

      ### recipe & tokenizer
      if(!is.null(self$meta$recs)) self$meta$recipe %>% save_rds_pos("recs", path)
      if(!is.null(self$meta$recipe)) self$meta$recipe %>% save_rds_pos("recipe", path)
      if(!is.null(self$meta$tok)) save_keras_tokenizer(self$meta$tok, path)
            
      ### metrics
      self$metrics %>% save_json_pos("metrics", path)
      
      ### preds
      self$preds %>% save_rds_pos("preds", path)
      
      ### imps
      if(!is.null(self$imps)) self$imps %>% save_rds_pos("imps", path)
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

#' fold_cv_oob
#' @export
fold_cv_oob <- function(data, split){
  ### extract id and split vars
  idx <- tibble(rid = 1:nrow(data), split = split)
  ### find test instances
  indices <- 1:5 %>% purrr::map(~{idx$rid[idx$split == .x]})
  ### analysis/assessment
  indices <- indices %>% purrr::map(rsample:::vfold_complement, n = nrow(idx))
  ### make split obj.
  split_objs <- purrr::map(indices, rsample:::make_splits, data = data, class = "vfold_split")
  tibble::tibble(splits = split_objs, id = paste0("Fold", 1:length(split_objs)))
}

#' fit_cv
#' @export
fit_cv <- function(rsample, rec, params, task, backend, path = NULL){
  
  out <- rsample %>%
    dplyr::mutate(
      models = map(splits, ~{
        g <- deeplyr::fit_learner(
          rec, dplyr::bind_rows(rsample::analysis(.x)), 
          params, task, backend
        )
        g$predict(dplyr::bind_rows(rsample::assessment(.x)))
        return(g)
      })
    ) %>%
    dplyr::mutate(
      preds = purrr::map(models, ~.x$preds),
      metrics = purrr::map(models, ~ as.list(.x$metrics))
    )
  
  if(!is.null(path)){
    out$metrics %>% dplyr::bind_rows() %>% save_json(., name = "cv_metrics", path = path)
    out %>% dplyr::select(id, preds) %>% save_rds(., name = "cv_preds", path = path)
  }
  
  return(out)
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
    ) %>% 
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
    ) %>% 
    dplyr::mutate(
      preds = purrr::map(models, ~.x$preds),
      metrics = purrr::map(models, ~.x$metrics)
    )
}


# fit_pair = function(x, y){
#   
#   ### freeze training data + pre-processing steps
#   self$process$bake(x, y)
#   self$meta$timestamp <- Sys.time()
#   
#   start <- Sys.time()
#   
#   self$model <- private$model_fit_pair(self)
#   
#   self$meta$runtime <- as.numeric(Sys.time() - start)
#   self$meta$n_features <- ncol(x)
#   self$meta$n_train <- nrow(x)
# },
# 
# predict_pair = function(new_data, suffix){
#   
#   if(!is.null(self$model)){
# 
#     self$meta$n_test <- nrow(new_data)
#     
#     yname <- self$process$ask_y() %>% 
#       stringr::str_remove("^local_|^visitor_") %>% 
#       stringr::str_remove_all("_") %>%
#       paste0(., "_")
#     
#     ### for summary stats only
#     if(yname == "_") yname <- ""
#     private$model_predict_pair(self, new_data) %>%
#       ### apply prefix other than team ids
#       dplyr::rename_at(-1:-3, ~ paste0(.x, "_", yname, suffix))
#     
#   } else {
#     
#     self$process$stream_id_x(new_data) %>%
#       dplyr::select(game_id, contains("team_id"))
#     
#   }
# },
