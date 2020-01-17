if(!require(pacman)) install.packages("pacman")
pacman::p_load("tidyquant",
               "tidyr",
               "furrr", 
               "plotly",
               "tibble",
               "dplyr",
               "highcharter",
               "purrr",
               "tidyverse", 
               "broom",
               "tibbletime",
               "timetk",
               "h2o",
               "recipes")

# IMPORT DATA

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
end     <- "2019-11-30" %>% ymd()
start   <- end - years(5) + days(1)
w <- c(0.3,
       0.4,
       0.15,
       0.075,
       0.075)
wts_tbl <- tibble(symbols, w)

portfolio_data <- function(symbols, end, start, wts_tbl){
  
  # Download Data
  download_data <- symbols %>% 
    tq_get(get = "stock.prices",
           from = start,
           to = end) 
  
  # Determine earliest date with full set of data
  min_date <- download_data %>%   
    group_by(symbol) %>% 
    summarise(min_date = min(date), 
              max_date = max(date)) %>% 
    ungroup() %>% 
    pull(min_date) %>% 
    max()
  
  # Transform to Returns
  returns_reg_date_tbl <- download_data %>% 
    select(symbol, date, adjusted) %>% 
    group_by(symbol) %>% 
    tq_transmute(select     = adjusted,
                 mutate_fun = periodReturn,
                 period     = "monthly") %>% 
    ungroup() %>% 
    #rollback to first day of the month - ETF Issue ----
  mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
    filter(date >= min_date)
  
  # Transform to Portfolio Returns
  returns_port_tbl <- returns_reg_date_tbl %>% 
    tq_portfolio(assets_col = symbol,
                 returns_col = monthly.returns,
                 weights = wts_tbl,
                 rebalance_on = "years") %>% 
    add_column(symbol = "Portfolio", .before = 1) %>% 
    rename(monthly.returns = portfolio.returns)
  
  return(returns_port_tbl)
}

test_1 <- portfolio_data(symbols, end, start, wts_tbl)

test_1


# Function to calculate portfolio based multi period return data (daily or monthly)
portfolio_multi_period_data <- function(symbols, end, start, wts_tbl, period = "monthly"){
  
  # Download Data
  download_data <- symbols %>% 
    tq_get(get = "stock.prices",
           from = start,
           to = end) 
  
  # Determine earliest date with full set of data
  min_date <- download_data %>%   
    group_by(symbol) %>% 
    summarise(min_date = min(date), 
              max_date = max(date)) %>% 
    ungroup() %>% 
    pull(min_date) %>% 
    max()
  
  if(period == "monthly"){
  
  # Transform to Returns
  returns_reg_date_tbl <- download_data %>% 
    select(symbol, date, adjusted) %>% 
    group_by(symbol) %>% 
    tq_transmute(select     = adjusted,
                 mutate_fun = periodReturn,
                 period     = period) %>% 
    ungroup() %>% 
    #rollback to first day of the month - ETF Issue ----
  mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
    filter(date >= min_date)
  
  # Transform to Portfolio Returns
  returns_port_tbl <- returns_reg_date_tbl %>% 
    tq_portfolio(assets_col = symbol,
                 returns_col = monthly.returns,
                 weights = wts_tbl,
                 rebalance_on = "years") %>% 
    add_column(symbol = "Portfolio", .before = 1) %>% 
    rename(monthly.returns = portfolio.returns)
  
  return(returns_port_tbl)
  }
  
  else if(period == "daily"){
    # Transform to Returns
    returns_reg_date_tbl <- download_data %>% 
      select(symbol, date, adjusted) %>% 
      group_by(symbol) %>% 
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = period) %>% 
      ungroup() %>% 
    #   #rollback to first day of the month - ETF Issue ----
    # mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
      filter(date >= min_date)
    
    # Transform to Portfolio Returns
    returns_port_tbl <- returns_reg_date_tbl %>% 
      tq_portfolio(assets_col = symbol,
                   returns_col = daily.returns,
                   weights = wts_tbl,
                   rebalance_on = "years") %>% 
      add_column(symbol = "Portfolio", .before = 1) %>% 
      rename(daily.returns = portfolio.returns)
    
    return(returns_port_tbl)
    
  }
  
}

test_3 <- portfolio_multi_period_data(symbols, end, start, wts_tbl, period = "daily")

test_3

# Function to calculate individual asset returns based multi period return data (daily or monthly)
individual_asset_multi_period_data <- function(symbol, end, start, period = "monthly"){
  
  if(length(symbol) > 1){
  # Download Data
  download_data <- symbol %>% 
    tq_get(get = "stock.prices",
           from = start,
           to = end) 
  
  # Determine earliest date with full set of data
  min_date <- download_data %>%   
    group_by(symbol) %>% 
    summarise(min_date = min(date), 
              max_date = max(date)) %>% 
    ungroup() %>% 
    pull(min_date) %>% 
    max()
  
  if(period == "monthly"){
    
    # Transform to Returns
    returns_reg_date_tbl <- download_data %>% 
      select(symbol, date, adjusted) %>% 
      group_by(symbol) %>% 
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = period) %>% 
      ungroup() %>% 
      #rollback to first day of the month - ETF Issue ----
    mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
      filter(date >= min_date)
    
    return(returns_reg_date_tbl)
  }
  
  else if(period == "daily"){
    # Transform to Returns
    returns_reg_date_tbl <- download_data %>% 
      select(symbol, date, adjusted) %>% 
      group_by(symbol) %>% 
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = period) %>% 
      ungroup() %>% 
      #   #rollback to first day of the month - ETF Issue ----
    # mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
    filter(date >= min_date)
    
    return(returns_reg_date_tbl)
    
  }
  }
  else if(length(symbol) == 1){
    # Download Data
    download_data <- symbol %>% 
      tq_get(get = "stock.prices",
             from = start,
             to = end) 
    
    # Determine earliest date with full set of data
    min_date <- download_data %>%   
      summarise(min_date = min(date), 
                max_date = max(date)) %>% 
      ungroup() %>% 
      pull(min_date) %>% 
      max()
    
    if(period == "monthly"){
      
      # Transform to Returns
      returns_reg_date_tbl <- download_data %>% 
        select(date, adjusted) %>% 
        tq_transmute(select     = adjusted,
                     mutate_fun = periodReturn,
                     period     = period) %>% 
        ungroup() %>% 
        #rollback to first day of the month - ETF Issue ----
      mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
        filter(date >= min_date)
      
      return(returns_reg_date_tbl)
    }
    
    else if(period == "daily"){
      # Transform to Returns
      returns_reg_date_tbl <- download_data %>% 
        select(date, adjusted) %>% 
        tq_transmute(select     = adjusted,
                     mutate_fun = periodReturn,
                     period     = period) %>% 
        ungroup() %>% 
        #   #rollback to first day of the month - ETF Issue ----
      # mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
      filter(date >= min_date)
      
      return(returns_reg_date_tbl)
      
    }  
  }
}

symbol <- c("SPY")
test_4 <- individual_asset_multi_period_data(symbol, end, start, period = "monthly")
test_4
