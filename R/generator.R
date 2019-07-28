#' generator
#' @export
generator <- R6::R6Class("container",
  private = list(
    create_id = function(){
      self$param$data_id <- self$param %>%
        purrr::keep(~is.atomic(.x) & length(.x) == 1) %>% 
        paste(collapse = "") %>% 
        openssl::md5(key = "id") %>% 
        as.character
    },
    fit_tokenizer = function(tokens = "word"){
      
      
      if(tokens == "word"){
        
        if(is.null(self$param$input_dim)) self$param$input_dim <- 5000
        if(is.null(self$param$term_count_min)) self$param$term_count_min <- 3
        if(is.null(self$param$doc_proportion_max)) self$param$doc_proportion_max <- .9
        
        self$vocab <- text2vec::itoken(
          self$data[[self$param$text]], 
          #ids = self$data$ids, 
          progressbar = F
        ) %>% 
          text2vec::create_vocabulary(.) %>% 
          text2vec::prune_vocabulary(
            term_count_min = self$param$term_count_min, 
            doc_proportion_max = self$param$doc_proportion_max,
            vocab_term_max = self$param$input_dim
          )
        
        # emperical number of tokens
        self$param$input_dim <- length(self$vocab$term)
        self$tokenizer <- keras::text_tokenizer(
          num_words = self$param$input_dim, 
          lower = F, 
          split = " ", 
          char_level = F
        )
        
        keras::fit_text_tokenizer(self$tokenizer, self$vocab$term)
        
      } else if(tokens == "character"){
        
        if(is.null(self$param$input_dim)) self$param$input_dim <- 100
        
        self$tokenizer <- keras::text_tokenizer(
          num_words = self$param$input_dim, 
          lower = F, 
          filters = "",
          char_level = T
        )
        
        keras::fit_text_tokenizer(self$tokenizer, head(self$data[[self$param$text]], 1000))
        
      }
    }
  ),
  public = list(
    ### Initalize variables
    param = NULL,
    text = NULL,
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
    #   self$param[[param]] <- value
    # },
    set_param = function(list){
      self$param <- list
      #purrr::iwalk(list, purrr::possibly(function(.x, .y){self$param[[.y]] <- .x}, NULL))
    },
    get_param = function() {
      return(self$param)
    },
    set_data = function(data = NULL) {
      self$data <- data
    },
    fit_word_tokenizer = function(){
      private$fit_tokenizer("word")
    },
    fit_char_tokenizer = function(){
      private$fit_tokenizer("character")
    },
    tokenize_dtm = function(x = NULL, mode = "binary"){
      
      internal <- is.null(x)
      if(is.null(internal)) {
        text <- self$data[[self$param$text]]
      } else {
        text <- x
      }
      
      x <- self$tokenizer %>%
        keras::texts_to_matrix(texts = text, mode)
      
      if(internal){
        self$x <- x
      } else {
        return(x)
      }
    },
    tokenize_seq = function(x = NULL, parallel = F, n = 1000){
      
      internal <- is.null(x)
      if(internal) {
        text <- self$data[[self$param$text]]
      } else {
        text <- x
      }
      
      if(parallel){
        
        plan(multicore)
        print("parallel")
        #print(head(text))
        
        x <- text %>%
          split(1:length(.) %/% n) %>%
          #split(1:nrow(.) %/% 10000) %>%
          furrr::future_imap(~{
            self$tokenizer %>%
              keras::texts_to_sequences(.x) %>%
              keras::pad_sequences(maxlen = self$param$seq_len)
          }, .progress = T) %>% 
          purrr::reduce(rbind)
        # system("rm -r temp")
        
      } else {
        
        print("sequential")
        
        x <- self$tokenizer %>% 
          keras::texts_to_sequences(text) %>%
          keras::pad_sequences(maxlen = self$param$seq_len)
        
      }
      
      if(internal){
        self$x <- x
      } else {
        return(x)
      }
    },
    report = function(){
      
      private$create_id()
      
      new_path <- glue::glue("data/container/{self$param$data_id}")
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



