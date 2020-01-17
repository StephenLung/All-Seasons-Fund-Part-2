plot_FF_leaderboard <- function(h2o_leaderboard, order_by = c("mean", "rmse", "mae"), n_max = 10,
                                size = 4, include_lbl = TRUE, pretty_breaks = 3){
  
  # order_by <- switch(tolower(ordery_by[[1]],
  #                            "mean" = "mean_residual_deviance",
  #                            "rmse" = "rmse"))
  #                            # "mse" = "mse",
  #                            # "mae" = "mae",
  #                            # "rmsle" = "rmsle"))
  
  order_by <- tolower(order_by[[1]])
  
  leaderboard_tbl <- h2o_leaderboard %>% 
    as.tibble() %>%
    mutate(model_type = str_split(model_id, pattern = "_", simplify = TRUE)[,1]) %>% 
    slice(1:n_max) %>% 
    rownames_to_column(var = "rowname") %>% 
    mutate(model_id = glue("{rowname}. {model_id}") %>% as_factor() %>% fct_rev())
  
  if(order_by == "mean"){
    
    data_transformed_tbl <- leaderboard_tbl %>% 
      mutate(model_id = as_factor(model_id) %>% reorder(mean_residual_deviance) %>% fct_rev(),
             model_type = as.factor(model_type)) %>% 
      pivot_longer(names_to = "key", values_to = "value", 
                   -c(model_id, model_type, rowname))
    
  }
  else if(order_by == "rmse"){
    data_transformed_tbl <- leaderboard_tbl %>% 
      mutate(model_id = as_factor(model_id) %>% reorder(rmse) %>% fct_rev(),
             model_type = as.factor(model_type)) %>% 
      pivot_longer(names_to = "key", values_to = "value", 
                   -c(model_id, model_type, rowname))
  }
  else if(order_by == "mae"){
    data_transformed_tbl <- leaderboard_tbl %>% 
      mutate(model_id = as_factor(model_id) %>% reorder(mae) %>% fct_rev(),
             model_type = as.factor(model_type)) %>% 
      pivot_longer(names_to = "key", values_to = "value", 
                   -c(model_id, model_type, rowname))
  }
  
  else {
    stop(glue("order_by = '{order_by}'is not a permitted option."))
  }
  
  
  g <- data_transformed_tbl %>% 
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(aes(size = size)) + 
    # geom_label(aes(label = round(value, 7), hjust = "inward")) +
    facet_wrap(~key, scales = "free_x") +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Machine Learning Leaderboard Metrics",
         subtitle = glue('Ordered by: {order_by}'),
         y = "Model Position, Model ID", x = "") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = pretty_breaks))
  
  if(include_lbl) g <- g +
    geom_label(aes(label = round(value, 5), hjust = "inward"), size = size)
  
  return(g)
}   
