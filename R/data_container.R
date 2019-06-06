#' data_container
#' 
#' @export
data_container <- R6::R6Class("container",
  #inherit = keras_embed,
  private = list(
    param = list(
      data_id = NULL, 
      data_name = NULL,
      text = NULL,
      input_dim = NULL, 
      seq_len = NULL,
      term_count_min = 5,
      doc_proportion_max  = .2
      #sample = NULL
    ),
    create_id = function(){
      private$param$data_id <- private$param %>%
        purrr::keep(~is.atomic(.x) & length(.x) == 1) %>% 
        paste(collapse = "") %>% 
        openssl::md5(key = "id") %>% 
        as.character
    }
  ),
  public = list(
    ### Initalize variables
    data = NULL,
    vocab = NULL, 
    tokenizer = NULL,
    x = NULL,
    ### Main Function
    initialize = function() {
      if(!dir.exists("data")){
        dir.create("data")
      }
      if(!dir.exists("data/container")){
        dir.create("data/container")
      }
    },
    # set_param = function(param, value){
    #   private$param[[param]] <- value
    # },
    set_param = function(list){
      purrr::iwalk(list, purrr::possibly(function(.x, .y){private$param[[.y]] <- .x}, NULL))
    },
    get_param = function() {
      return(private$param)
    },
    set_data = function(data = NULL) {
      
      if(!is.null(private$param$data_path)){
        # data_name <- path %>% 
        #   str_remove("\\..*?$") %>% 
        #   str_extract("\\w+$")
        data <- get(load(path))
      }
      
      # shuffle
      self$data <- data %>%
        dplyr::arrange(sample(1:n(), n()))
      
      # subsample 
      # if(is.null(private$param$sample)) private$param$sample <- nrow(data) + 1
      # if(nrow(data) > private$param$sample) {
      #   
      #   self$data <- data %>%
      #     dplyr::sample_n(n) %>%
      #     dplyr::arrange(sample(1:n(), n()))
      #   
      #   message("subsampled")
      #   
      # } else {
      #   
      #   self$data <- data
      #   message("subsampled not 1")
      # }

      private$param$total_size <- nrow(self$data)
    },
    get_param_recom = function(){
      seq_porps <- self$data %>% 
        dplyr::mutate_("var" = private$param$text) %>%
        dplyr::mutate(
          nchar = stringr::str_length(var),
          nword = stringr::str_count(var, "\\w+")
        ) %>%
        dplyr::select(nword, nchar) %>%
        tidyr::gather(var, value) %>%
        ggplot2::ggplot(ggplot2::aes(value)) +
        ggplot2::geom_histogram(alpha = .7) + 
        ggplot2::geom_vline(xintercept = private$param$seq_len) +
        ggplot2::facet_wrap(~var, scales = "free") +
        ggplot2::ggtitle("Determine seq_len")
      
      token_freq <- self$data %>% 
        dplyr::select_(private$param$text) %>%
        tidytext::unnest_tokens_("word", private$param$text) %>% 
        dplyr::count(word, sort = T) %>% 
        dplyr::filter(n < (sd(n)*.2)) %>%
        ggplot2::ggplot(ggplot2::aes(n)) + 
        ggplot2::geom_density() +
        ggplot2::geom_vline(xintercept = private$param$term_count_min) +
        ggplot2::ggtitle("Limit of term_count_min")
      
      gridExtra::grid.arrange(seq_porps, token_freq, nrow = 2)
    },
    transform_tokenizer = function(){
      self$vocab <- text2vec::itoken(
        self$data[[private$param$text]], 
        #ids = self$data$ids, 
        progressbar = F) %>% 
        text2vec::create_vocabulary(.) %>% 
        text2vec::prune_vocabulary(
          term_count_min = private$param$term_count_min, 
          doc_proportion_max = private$param$doc_proportion_max, 
          vocab_term_max = private$param$input_dim
        )
      
      ### Constraint
      #if(private$param$input_dim != length(self$vocab$term)) return()
      private$param$input_dim <- length(self$vocab$term)
      
      self$tokenizer <- keras::text_tokenizer(num_words = private$param$input_dim, lower = F, split = " ", char_level = F)
      keras::fit_text_tokenizer(self$tokenizer, self$vocab$term)
    },
    transform_drop = function(drop = NULL){
      target_col <- which(!(colnames(self$data) %in% drop))
      self$x <- self$data[, target_col]
    }, 
    transform_dtm = function(mode = "binary"){
      
      self$data <- self$data %>%
        mutate(nwords = self$data[[private$param$text]] %>% str_count("\\w+"))
      
      self$transform_tokenizer()
      
      self$x <- self$tokenizer %>%
        keras::texts_to_matrix(texts = self$data[[private$param$text]], mode)
    },
    transform_seq = function(parallel = T){
      
      # glimpse(self$data)
      # print(private$param$text)
      # print(head(self$data[, private$param$text]))
      self$data <- self$data %>%
        mutate(nwords = self$data[[private$param$text]] %>% str_count("\\w+"))
      
      self$transform_tokenizer()
      
      if(parallel){
        
        self$x <- self$data %>%
          split(ntile(1:nrow(.), 100)) %>%
          #split(1:nrow(.) %/% 10000) %>%
          furrr::future_imap(~{
            self$tokenizer %>%
              keras::texts_to_sequences(.x[[private$param$text]]) %>%
              keras::pad_sequences(maxlen = private$param$seq_len)
          }, .progress = T) %>% 
          purrr::reduce(rbind)
          # system("rm -r temp")
        
      } else {
        
        self$x <- self$tokenizer %>% 
          keras::texts_to_sequences(self$data[[private$param$text]]) %>%
          keras::pad_sequences(maxlen = private$param$seq_len)
        
      }
    },
    report = function(){
      
      private$create_id()

      new_path <- glue::glue("data/container/{private$param$data_id}")
      if(!dir.exists(new_path)){
        dir.create(new_path)
      }

      self$get_param() %>%
        as.list() %>%
        jsonlite::toJSON(force = T) %>%
        jsonlite::fromJSON() %>%
        jsonlite::toJSON(pretty = T, auto_unbox = T) %>%
        writeLines(con =  glue::glue("{new_path}/data_param.json"))
      
      save(self, file = paste0(new_path, "/data_container.Rdata"))
    }
  )
)
