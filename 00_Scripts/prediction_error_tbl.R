prediction_error_tbl <- function(Portfolio_tbl, h2o_leaderboard, n = 1, test_tbl){
  automl_model <- h2o_leaderboard@leaderboard %>% 
    as.tibble() %>%
    slice(n) %>% 
    pull(model_id) %>% 
    h2o.getModel()
  automl_name <- h2o_leaderboard@leaderboard %>% 
    as.tibble() %>%
    mutate(model_type = str_split(model_id, pattern = "_", simplify = TRUE)[,1],
           model_name = glue::glue("{model_type} Model") %>% as.character()) %>%
    slice(n) %>% 
    pull(model_name) 
  prediction_h2o <- h2o.predict(automl_model, newdata = test_tbl)
  
  error_tbl <- Portfolio_tbl %>% 
    filter(lubridate::year(date) == 2019) %>% 
    na.omit() %>% 
    add_column(pred = prediction_h2o %>% as.tibble() %>% pull(predict)) %>% 
    rename(actual = Portfolio) %>% 
    mutate(
      error = actual - pred,
      error_pct = error/actual
    ) 
  return(error_tbl)
}

pull_model_name <- function(h2o_leaderboard, n = 1){
  pull_model_name <- portfolio_automl_models_h2o@leaderboard %>% 
    as_tibble() %>% 
    rownames_to_column() %>% 
    mutate(model_type = str_split(model_id, pattern = "_", simplify = TRUE)[,1],
           model_id = glue("Model_{rowname}_{model_type}")) %>% 
    pull(model_id) %>% 
    .[n]
  
  return(pull_model_name)
}
