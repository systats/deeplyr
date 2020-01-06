#' create_plan for weekly splits
#' @export
create_plan <- function(date, d_train, d_test){
  
  dplyr::tibble(date = lubridate::as_date(date)) %>%
    dplyr::mutate(
      week = date %>% 
        lubridate::week() %>%
        stringr::str_pad(width = 2, pad = 0),
      mon = lubridate::month(date),
      year = lubridate::year(date),
      version = glue::glue("{year}_{week}"),
      
      period = dplyr::case_when(
        date < d_train ~ "0",
        date >= d_train & date < d_test ~ "1",
        date >= d_test ~ "2"
      ),
      index = ifelse(period == "2", version, NA) %>% 
        as.factor() %>% 
        forcats::fct_reorder2(-year, -mon) %>%
        as.numeric(),
      index = ifelse(period %in% c("1", "2") & is.na(index), 0, index),
      index = ifelse(date > Sys.Date(), NA, index)
    )
}

#' tcv manage weekly splits
#' @export
tcv <- R6::R6Class("stack",
  private = list(
    
    init_versions = function(){
      versions <- glue::glue("{self$path}/versions")
      if(!dir.exists(versions)) dir.create(versions)
    }
    
  ),
  public = list(
    path = NULL,
    index = NULL,
    data = NULL,
    plan = NULL,
    initialize = function(path){
      
      self$path <- path
      
      private$init_versions()
      
    },
    feed = function(data){
      
      self$data <- create_plan(data$date, data$t1, data$t2)
      
      self$plan <- self$data %>%
        dplyr::mutate(status = ifelse(version %in% dir(glue::glue("{self$path}/versions")), 1, 0)) %>%
        dplyr::group_by(period, version, index, status) %>%
        dplyr::summarise(date = date[1], n = n()) %>%
        dplyr::ungroup() %>%
        tidyr::drop_na() 
    },
    plot_plan = function(){
      
      self$plan %>%
        #count(date, period, index, status) %>%
        drop_na(period) %>%
        ggplot(aes(x = date, y = period, colour = index)) +
        geom_point()
      
    },
    plot_progress = function(){
      self$plan %>%
        dplyr::filter(period == "2") %>%
        ggplot(aes(x = date, y = 1, colour = status, label = index)) +
        geom_text(nudge_y = .05, size =2) + 
        geom_point(size = 2) +
        ylim(.95, 1.1)
    },
    go_forward = function(n = 100){
      self$plan %>%
        dplyr::filter(period == "2") %>%
        dplyr::filter(status == 0) %>%
        dplyr::distinct(index, version, status) %>%
        dplyr::arrange(index) %>%
        head(n)
    },
    go_back = function(n = 100){
      self$plan %>%
        dplyr::filter(period == "2") %>%
        dplyr::filter(status == 0) %>%
        dplyr::distinct(index, version, status) %>%
        dplyr::arrange(-index) %>%
        head(n)
    }
  )
)

#' check_tcv
#' @export
check_tcv <- function(data, path){
  plan <- tcv$new(path)
  plan$feed(data)
  plan
}

#' fit_tcv
#' @export
fit_tcv <- function(params, data, task, backend, path, n = 100){
  
  plan <- tcv$new(path)
  plan$feed(data)
  
  #if(!any("params.json" %in% dir(path))) {
  params <- params %>% purrr::keep(~is.numeric(.x)|is.character(.x))
  save_json_pos(params, "params", path)
  # } else {
  #   params <- params %>% purrr::keep(~is.numeric(.x)|is.character(.x))
  #   existing_params <- jsonlite::read_json(glue::glue("{path}/params.json"))
  #   if(!identical(params, existing_params)) stop("params are different")
  # }
  
  plan$go_back(n) %>%
    split(1:nrow(.)) %>%
    walk(~{
      d <- list()
      d$train$x <- data$x[plan$data$index < .x$index & !is.na(plan$data$index), ] 
      d$train$y <- data$y[plan$data$index < .x$index & !is.na(plan$data$index)] 
      d$train$target <- data$target[plan$data$index < .x$index & !is.na(plan$data$index)] 
      
      d$test$x <- data$x[plan$data$index %in% c(.x$index:(.x$index+4)) & !is.na(plan$data$index), ] 
      d$test$y <- data$y[plan$data$index %in% c(.x$index:(.x$index+4)) & !is.na(plan$data$index)]
      d$test$target <-  data$target[plan$data$index %in% c(.x$index:(.x$index+4)) & !is.na(plan$data$index)]
      fit_learner(params, data = d, task, backend, path = glue::glue("{path}/versions/{.x$version}"))
    })
}
