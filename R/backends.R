#' backend
#' @export
backend <- R6::R6Class("backend",
  #inherit = backends, 
  private = list(
    
    model_fit = NULL,
    model_predict = NULL,
    model_imp = NULL,
    model_save = NULL,
    model_load = NULL,
    
    model_backend = function(){
      
      ### keras
      if(self$meta$backend == "keras"){
        private$model_fit <- fit_keras
        private$model_predict <- predict_keras
        #private$model_imp <- feature_imp_keras
        private$model_save <- save_keras
        private$model_load <- load_keras
      }
      
      ### xgboost
      if(self$meta$backend == "xgboost"){
        private$model_fit <- fit_xgboost
        private$model_predict <- predict_xgboost
        private$model_imp <- feature_imp_xgboost
        private$model_save <- save_xgboost
      }
      
      ### lightgbm
      if(self$meta$backend == "lightgbm"){
        private$model_fit <- fit_lightgbm
        private$model_predict <- predict_lightgbm
      }
      
      ### catboost
      if(self$meta$backend == "catboost"){
        private$model_fit <- fit_catboost
        private$model_predict <- predict_catboost
        private$model_imp <- h2o_feature_imp
      }
      # if(self$meta$backend == "rgf"){
      #   private$model_fit <- fit_rgf
      #   private$model_predict <- predict_rgf
      # }
      
      ### h2o
      if(stringr::str_detect(self$meta$backend, "h2o_")){
        
        private$model_predict <- predict_h2o
        private$model_imp <- feature_imp_h2o
        private$model_save <- save_h2o
        private$model_load <- load_h2o
        
        if(self$meta$backend == "h2o_glm") private$model_fit <- fit_h2o_glm
        if(self$meta$backend == "h2o_rf") private$model_fit <- fit_h2o_rf
        if(self$meta$backend == "h2o_nb")  private$model_fit <- fit_h2o_nb
        if(self$meta$backend == "h2o_svm") private$model_fit <- fit_h2o_svm
        if(self$meta$backend == "h2o_gbm") private$model_fit <- fit_h2o_gbm
        if(self$meta$backend == "h2o_xgb") private$model_fit <- fit_h2o_xgb
        if(self$meta$backend == "h2o_dnn") private$model_fit <- fit_h2o_dnn
        
      }
      ### sklearn
      if(stringr::str_detect(self$meta$backend, "sk_")){
        
        private$model_predict <- predict_sk
        
        if(self$meta$backend == "sk_glm") private$model_fit <- fit_sk_glm
        if(self$meta$backend == "sk_tree") private$model_fit <- fit_sk_tree
        
      }
      ### ranger
      if(self$meta$backend == "ranger"){
        
        private$model_fit <- fit_ranger
        private$model_predict <- predict_ranger
        
      }
      ### randomForest
      if(self$meta$backend == "randomForest"){
        
        private$model_fit <- fit_randomForest
        private$model_predict <- predict_randomForest
        
      }
      ### rpart
      if(self$meta$backend == "rpart"){
        
        private$model_fit <- fit_rpart
        private$model_predict <- predict_rpart
        private$model_imp <- feature_imp_rpart
        private$model_save <- save_rpart
        private$model_load <- load_rpart
        
      }
    }
  )
)
