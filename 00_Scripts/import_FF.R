# 2.0 Setup of 3 Factor function ----
import_3_FF <- function(link, file_name, returns_tbl){
  temp <- tempfile()
  
  download.file(link,
                temp,
                quiet = TRUE)
  
  Global_3_Factors <- read_csv(unz(temp, file_name), skip =6) %>% 
    rename(date = X1) %>% 
    mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>% 
    mutate_at(vars(-date), as.numeric) %>% 
    mutate_if(is.numeric, funs(./100)) %>% 
    rename(MKT = `Mkt-RF`) %>% 
    #rollback to first day of the month - ETF Issue ----
  mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
    na.omit()
  
  #Linear regression stored as a S3 object
  index_monthly_returns %>% 
    left_join(Global_3_Factors, by = "date") %>% 
    do(model = lm(monthly.returns ~ MKT + SMB + HML + RF,
                  data = .))
  
  fama_factor_lm_func <- returns_tbl %>% 
    left_join(Global_3_Factors, by = "date") %>%
    mutate(R_excess = monthly.returns - RF) %>% 
    select(-monthly.returns, RF) %>% 
    do(model = lm(R_excess ~ MKT + SMB + HML,
                  data = .)) %>% 
    tidy(model, conf.int = T, conf.level = .95) %>% 
    rename(beta = estimate,
           factor = term) %>% 
    mutate(beta_text = scales::number(beta, accuracy = 0.01))
  
  return(fama_factor_lm_func)
}

# 3.0 Setup of 5 Factor function ----
import_5_FF <- function(link, file_name, returns_tbl){
  temp <- tempfile()
  
  download.file(link,
                temp,
                quiet = TRUE)
  
  Global_5_Factors <- read_csv(unz(temp, file_name), skip =6) %>% 
    rename(date = X1) %>% 
    mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>% 
    mutate_at(vars(-date), as.numeric) %>% 
    mutate_if(is.numeric, funs(./100)) %>% 
    rename(MKT = `Mkt-RF`) %>% 
    #rollback to first day of the month - ETF Issue ----
  mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
    na.omit()
  
  return(Global_5_Factors)
}

fama_french_lm <- function(fama_french_factors, portfolio_return, returns_tbl){
  #Linear regression stored as a S3 object
  portfolio_return %>% 
    left_join(fama_french_factors, by = "date") %>% 
    do(model = lm(monthly.returns ~ MKT + SMB + HML + RMW + CMA + RF,
                  data = .))
  
  fama_factor_lm_func <- returns_tbl %>% 
    left_join(fama_french_factors, by = "date") %>%
    mutate(R_excess = monthly.returns - RF) %>% 
    select(-monthly.returns, RF) %>% 
    do(model = lm(R_excess ~ MKT + SMB + HML + RMW + CMA,
                  data = .)) %>% 
    tidy(model, conf.int = T, conf.level = .95) %>% 
    rename(beta = estimate,
           factor = term) %>% 
    mutate(beta_text = scales::number(beta, accuracy = 0.01))
  
  return(fama_factor_lm_func)
}