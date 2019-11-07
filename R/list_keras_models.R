#' list_keras_models
#'
#' @export

list_keras_models <- function(name = ""){
  
   m <-  list(
      dplyr::tibble(
        name = "simple_mlp",
        model = list(deeplyr::keras_simple_mlp),
        param = list(formals(deeplyr::keras_simple_mlp))
      ),
      dplyr::tibble(
        name = "deep_mlp",
        model = list(deeplyr::keras_deep_mlp),
        param = list(formals(deeplyr::keras_deep_mlp))
      ),
      dplyr::tibble(
        name = "lstm",
        model = list(deeplyr::keras_lstm),
        param = list(formals(deeplyr::keras_lstm))
      ),
      dplyr::tibble(
        name = "bi_lstm",
        model = list(deeplyr::keras_bi_lstm),
        param = list(formals(deeplyr::keras_bi_lstm))
      ),
      dplyr::tibble(
        name = "deep_lstm",
        model = list(deeplyr::keras_deep_lstm),
        param = list(formals(deeplyr::keras_deep_lstm))
      ),
      dplyr::tibble(
        name = "deep_bi_lstm",
        model = list(deeplyr::keras_deep_bi_lstm),
        param = list(formals(deeplyr::keras_deep_bi_lstm))
      ),
      dplyr::tibble(
        name = "gru",
        model = list(deeplyr::keras_gru),
        param = list(formals(deeplyr::keras_gru))
      ),
      dplyr::tibble(
        name = "bi_gru",
        model = list(deeplyr::keras_bi_gru),
        param = list(formals(deeplyr::keras_bi_gru))
      ),
      dplyr::tibble(
        name = "cnn_gru",
        model = list(deeplyr::keras_cnn_gru),
        param = list(formals(deeplyr::keras_cnn_gru))
      ),
      dplyr::tibble(
        name = "cnn_lstm",
        model = list(deeplyr::keras_cnn_lstm),
        param = list(formals(deeplyr::keras_cnn_lstm))
      ),
      dplyr::tibble(
        name = "gru_cnn",
        model = list(deeplyr::keras_gru_cnn),
        param = list(formals(deeplyr::keras_gru_cnn))
      ),
      dplyr::tibble(
        name = "multi_cnn",
        model = list(deeplyr::keras_multi_cnn),
        param = list(formals(deeplyr::keras_multi_cnn))
      ),
      dplyr::tibble(
        name = "sep_cnn",
        model = list(deeplyr::keras_sep_cnn),
        param = list(formals(deeplyr::keras_sep_cnn))
      )
    ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(backend = "keras")
  
  if(name == "") return(m)
   
   
   mname <- name
   m %>% filter(name == mname)
   
}
