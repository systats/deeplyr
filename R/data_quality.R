#' add_na
#' @export 
add_na <- function(.x){
  out <- .x %>%
    is.na() %>%
    tibble(var = .) %>% 
    count(var, .drop = F) %>% 
    filter(var) %>%
    pull(n)
  if(length(out) == 0) out <- 0
  return(out)
}

#' add_nan
#' @export 
add_nan <- function(.x){
  out <- .x %>%
    is.nan() %>%
    tibble(var = .) %>% 
    count(var, .drop = F) %>% 
    filter(var) %>%
    pull(n)
  if(length(out) == 0) out <- 0
  return(out)
}

#' add_inf
#' @export 
add_inf <- function(.x){
  out <- .x %>%
    is.infinite() %>%
    tibble(var = .) %>% 
    count(var, .drop = F) %>% 
    filter(var) %>%
    pull(n)
  if(length(out) == 0) out <- 0
  return(out)
}

#' check_col
#' @export 
check_col <- function(.x, .y){
  
  var <- var(.x, na.rm = T)
  ran <- as.numeric(range(.x, na.rm = T))
  n <- length(.x)
  tibble(
    features = .y, 
    var, 
    lower = ran[1],
    upper = ran[2],
    freq_na = add_na(.x),
    freq_nan = add_nan(.x),
    freq_inf = add_inf(.x),
    perc_na = freq_na/n,
    perc_nan = freq_nan/n,
    perc_inf = freq_inf/n
  )
}

#' check_data
#' @export 
check_data <- function(.data){
  .data %>%
    imap_dfr(check_col) %>% 
    mutate_at(-1, round, 3)
}


#' replace_nan
#' @export 
replace_nan <- function(.data){
  .data %>%
    mutate_all(~ifelse(is.nan(.x), NA, .x))
}

#' replace_inf
#' @export 
replace_inf <- function(.data){
  .data %>%
    mutate_all(~ifelse(is.infinite(.x), NA, .x))
}