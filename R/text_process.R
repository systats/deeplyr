#' fit_tokenizer
#' @export
fit_tokenizer <- function(text, max_words = 10000, max_docs = .5, min_word = 3, char_level = T, path = NULL){
  
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

#' tokenize_text
#' @export
tokenize_text <- function(text, tok, seq_len = 100, multi = F){
  
  if(multi){
    tok %>% 
      keras::texts_to_sequences(texts = text) %>% 
      keras::pad_sequences(maxlen = seq_len)
  } else {
    
    text %>% 
      split(1:length(.) %/% 1000) %>%
      furrr::future_map(~{
        tok %>% 
          keras::texts_to_sequences(texts = .x) %>% 
          keras::pad_sequences(maxlen = seq_len)
      }, .progress = T) %>% 
      rlist::list.rbind()
  }
  
}