#' fit_tokenizer
#' @export
fit_tokenizer <- function(text, max_words = 10000, max_docs = .5, min_word = 3, char_level = F, path = NULL){
  
  vocab <- text2vec::itoken(text, progressbar = F) %>%
    text2vec::create_vocabulary(.) %>%
    text2vec::prune_vocabulary(
      term_count_min = min_word,
      doc_proportion_max = max_docs,
      vocab_term_max = max_words
    )
  
  tok <- keras::text_tokenizer(
    num_words = max_words,
    lower = T,
    char_level = char_level
  )
  
  keras::fit_text_tokenizer(tok, vocab$term)
  
  if(!is.null(path)) keras::save_text_tokenizer(tok, filename = glue::glue("{path}/tok"))
  
  return(tok)
}

#' tokenize_seq
#' @export
tokenize_seq <- function(text, tok, seq_len){
  tok %>% 
    keras::texts_to_sequences(texts = text) %>% 
    keras::pad_sequences(maxlen = seq_len)
}

#' tokenize_text
#' @export
tokenize_text <- function(text, tok, seq_len = 100, multi = F, n_split = 10000){
  
  if(!multi){
    
    tokenize_seq(text, tok, seq_len)
    
  } else {
    
    ### nasty work around for mac
    tmp <- tempdir()
    keras::save_text_tokenizer(tok, filename = glue::glue("{tmp}/tok"))
    
    out <- text %>%
      split(1:length(.) %/% n_split) %>%
      furrr::future_map(~{
        ### part 2 work around
        tok <- keras::load_text_tokenizer(glue::glue("{tmp}/tok"))
        tokenize_seq(.x, tok, seq_len)
      }, .progress = T) %>% 
      rlist::list.rbind()
    
    rm(tmp)
    return(out)
  }
}



#' load_tokenizer
#' @export
load_tokenizer <- function(path) keras::load_text_tokenizer(glue::glue("{path}/tok"))


#' save_keras_tokenizer
#' @export
save_keras_tokenizer <- function(model, path) keras::save_text_tokenizer(model, glue::glue("{path}/tok"))
