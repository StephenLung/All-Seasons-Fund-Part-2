


setwd("C:/Users/admin/Dropbox/Business University Science/Personal Portfolio")

setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/Part 2 Draft")
remove(list=ls())
if(!require(pacman)) install.packages("pacman")
pacman::p_load("tidyquant",
               "tidyr",
               "furrr", 
               "plotly",
               "tibble",
               "dplyr",
               "highcharter",
               "purrr",
               "correlationfunnel",
               "dplyr",
               "glue",
               "tibble")

# 1.0 IMPORT DATA ----

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
end     <- "2019-11-30" %>% ymd()
start   <- end - years(5) + days(1)
w <- c(0.3,
       0.4,
       0.15,
       0.075,
       0.075)
wts_tbl <- tibble(symbols, w)
source(file = "00_Scripts/portfolio_multi_period_data.R")
source(file = "00_Scripts/import_FF.R")

# All seasons data and portfolio
portfolio_training_data <- portfolio_multi_period_data(symbols, end, start, wts_tbl, period = "monthly")
all_seasons_asset <- individual_asset_multi_period_data(symbols, end, start, period = "monthly")
returns_port_tbl <- portfolio_training_data %>% 
    rbind(all_seasons_asset)
    # mutate(symbol = str_replace_all(symbol, c("VTI" = "Stocks",
    #                                 "TLT" = "Long Term Bonds",
    #                                 "IEF" = "Medium Term Bonds",
    #                                 "GLD" = "Gold",
    #                                 "DBC" = "Commodities")))

returns_port_tbl %>% 
    pivot_wider(names_from = symbol, values_from = monthly.returns)


# 4.0 Correlation funnel -----

factors_input <- "Developed_5_Factors"
factors_address_1 <- paste("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/", 
                         factors_input, 
                         "_CSV.zip",
                         sep = "")
factors_csv_name_1 <- paste(factors_input,
                            ".csv", 
                            sep = "")

#UPDATE Correcting lm
link <- factors_address_1
file_name <- factors_csv_name_1
returns_tbl <- returns_port_tbl

fama_french_factors <- Global_5_Factors
portfolio_return <- portfolio_training_data
returns_tbl <- returns_port_tbl

Wfactors_input_2 <- "Developed_3_Factors"
factors_address_2 <- paste("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/", 
                           factors_input_2, 
                           "_CSV.zip",
                           sep = "")
factors_csv_name_2 <- paste(factors_input_2,
                            ".csv", 
                            sep = "")
Global_3_Factors <- import_5_FF(factors_address_2, factors_csv_name_2, returns_port_tbl) 
Global_3_Factors

link_5_FF = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Developed_5_Factors_CSV.zip"
# link_5_FF = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_5_Factors_CSV.zip"
file_name_5_FF = "Developed_5_Factors.csv"
Global_5_Factors <- import_5_FF(factors_address_1, factors_csv_name_1, returns_port_tbl) 
Global_5_Factors

Port_FF_5_tbl <- returns_port_tbl %>% 
    filter(symbol == "Portfolio") %>% # new addition 01.03.2020
    rename(Portfolio = monthly.returns) %>% 
    select(-symbol) %>% 
    left_join(Global_5_Factors, by="date")

returns_correlation_funnel <- Port_FF_5_tbl %>% 
    select(-date) %>% 
    na.omit() %>% 
    correlate(Portfolio) %>% 
    filter(!(feature == "Portfolio")) %>% 
    mutate(correlation = round(correlation, 2)) 

# Plot
returns_correlation_funnel %>% 
    filter(!(feature == "RF")) %>% 
    plot_correlation_funnel(interactive = FALSE) +
    geom_label(aes(label = correlation %>% scales::percent()), hjust = "outward", size = 8) +
    geom_segment(aes(x = correlation, y = feature, xend = 0, yend = feature)) + 
    labs(title = "Correlation Funnel of Portfolio with Pearson Method",
         subtitle = "Strong correlation with profitability (RMW), market value (MKT) \nand negative correlation with value (HML)",
         caption = "Ray Dalio's All Weather Portfolio",
         x = "Correlation",
         y = "Fama French factors")

## Fama French 5 Factor
# Name the variables
link_5_FF = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Developed_5_Factors_CSV.zip"
# link_5_FF = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_5_Factors_CSV.zip"
file_name_5_FF = "Developed_5_Factors.csv"
# file_name_5_FF = "Global_5_Factors.csv"
returns_tbl = returns_port_tbl
index_monthly_returns = portfolio_training_data

# Plot
import_5_FF(link_5_FF, file_name_5_FF, returns_port_tbl) %>% 
    fama_french_lm(index_monthly_returns, returns_tbl) %>% 
    filter(factor != "(Intercept)") %>% 
    ggplot(aes(factor,
               y = beta,
               color = factor)) + 
    geom_point(aes(size = 2)) +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high)) +
    labs(title = "Fama French 5-Factor Betas with 95% Confidence Interval",
         caption = "Ray Dalio's All Weather Portfolio",
         subtitle = "Positive betas on profitability (RMW), market value (MKT), investment (CMA) and size (SMB) \nNegative relationship with value (HML)",
         x = "Factor",
         y = "Beta") +
    theme_tq() + 
    scale_color_tq() + 
    geom_text(aes(label = beta_text), hjust = -0.3)

## Fama French 3 Factor
# Name the variables
link_3_FF = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Developed_3_Factors_CSV.zip"
file_name_3_FF = "Developed_3_Factors.csv"

# Plot
import_3_FF(link_3_FF, file_name_3_FF, returns_port_tbl) %>% 
    filter(factor != "(Intercept)") %>% 
    ggplot(aes(factor,
               y = beta,
               color = factor)) + 
    geom_point(aes(size = 2)) +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high)) +
    labs(title = "Fama French 3-Factor Betas with Confidence Interval",
         caption = "Ray Dalio's All Weather Portfolio",
         subtitle = "Positive betas on market value (MKT)\nNegative betas on value (HML) and size (SMB)",
         x = "Factor",
         y = "Beta") +
    theme_tq() + 
    scale_color_tq() + 
    geom_text(aes(label = beta_text), hjust = -0.3)


# 5.0 Portfolio Standard Deviation ----
asset_returns_wide <- returns_port_tbl %>% 
    filter(symbol != "Portfolio") %>% 
    pivot_wider(names_from = "symbol", values_from= "monthly.returns") %>% 
    # convert to xts function
    timetk::tk_xts(date_var = date)

# 5.1 Portfolio std deviation is 2.05%. Our potfolio has lower volatility
port_std_dev <- asset_returns_wide %>% 
    StdDev(weights = w) %>% 
    round(.,4) * 100

asset_names <- names(asset_returns_wide)

# 5.2 Asset std deviation is 2-5% 


SP500_tbl <- individual_asset_multi_period_data("SPY", end, start, period = "monthly") %>% 
    add_column(symbol = "S&P500") %>% 
    select(symbol, everything())

asset_std_dev <- returns_port_tbl %>% 
    filter(symbol != "Portfolio") %>%
    rbind(SP500_tbl) %>% 
    pivot_wider(names_from = "symbol", values_from= "monthly.returns") %>% 
    select(-date) %>% 
    map_df(~ StdDev(.)) %>% 
    round(.,4) *100

std_dev_comparison_tbl <- tibble(Name = c("Portfolio", asset_names, "SP500"),
       Std_Dev = c(port_std_dev, asset_std_dev)) %>% 
    unnest() %>% 
    mutate(Std_Dev = Std_Dev/100)

std_dev_comparison_tbl %>% 
    ggplot(aes(Name, Std_Dev))+
    geom_point() + 
    theme_tq() +
    scale_y_continuous(labels = scales::percent) +
    geom_label(aes(label = scales::percent(round(Std_Dev,5))), hjust = "inward")+
    labs(title = "Asset and Portfolio Standard Deviation Comparison",
         caption = "Ray Dalio's All Weather Portfolio",
         subtitle = "Portfolio Standard Deviation is 1.76%",
         x = "",
         y = "Standard Deviation") +
    geom_label(data = std_dev_comparison_tbl, aes(x = "Portfolio", 
             y = 0.02, 
             label = "Portfolio Standard Deviation"),
             color = palette_light()[[2]],
             fontface = "bold") + 
    theme_tq()


# 6.0 Modelling ----

# Plot to identify validation and test sets
Port_FF_5_tbl %>% 
    ggplot(aes(date, Portfolio)) + 
    annotate("text", x = ymd("2016-01-01"), y = -0.034, 
             color = palette_light()[[1]], label = "Train Region") +
    # # Validation Square
    # geom_rect(xmin = as.numeric(ymd("2018-01-01")),
    #           xmax = as.numeric(ymd("2019-01-01")),
    #           ymin = -Inf, ymax = Inf, alpha = 0.02,
    #           fill = palette_light()[[3]]) +
    # annotate("text", x = ymd("2018-06-01"), y = -0.034, 
    #          color = palette_light()[[1]], label = "Validation\nRegion") +
    #Test Square
    geom_rect(xmin = as.numeric(ymd("2019-01-01")),
              xmax = as.numeric(ymd("2019-11-01")),
              ymin = -Inf, ymax = Inf, alpha = 0.02,
              fill = palette_light()[[3]]) +
    annotate("text", x = ymd("2019-06-01"), y = -0.034, 
             color = palette_light()[[1]], label = "Test\nRegion") +
    geom_line(col = palette_light()[[1]]) + 
    geom_point(col = palette_light()[[1]]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) + 
    theme_tq() + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::percent_format())+
    labs(title = "Portfolio Returns: 2015 through 2019",
         subtitle = "Train, and Test Sets Shown",
         caption = "Ray Dalio's All Weather Funds",
         y = "Portfolio Returns", x = "Date") 


# Start up h2o
h2o.init()
h2o.no_progress()

portfolio_return_tbl_clean <- Port_FF_5_tbl %>% 
    na.omit() %>% 
    tk_augment_timeseries_signature() %>% 
    select_if(~ !is.Date(.)) %>% 
    select_if(~ !any(is.na(.))) %>%
    mutate_if(is.ordered, ~as.character(.) %>% as.factor)

train_tbl <- portfolio_return_tbl_clean %>% filter(year< 2019)
train_tbl_h2o <- as.h2o(train_tbl)

test_tbl <- portfolio_return_tbl_clean %>% filter(year == 2019)
test_tbl_h2o <- as.h2o(test_tbl)

y <- "Portfolio"
x <- setdiff(names(train_tbl_h2o), y)

portfolio_automl_models_h2o <- h2o.automl(
    x = x,
    y =y,
    training_frame = train_tbl_h2o,
    max_runtime_secs = 60,
    stopping_metric = "deviance")

automl_leader <- portfolio_automl_models_h2o@leader
automl_leader_name <- portfolio_automl_models_h2o@leaderboard %>% 
    as_tibble() %>%
    mutate(model_type = str_split(model_id, pattern = "_", simplify = TRUE)[,1],
           model_name = glue::glue("{model_type} Model") %>% as.character()) %>%
    slice(1) %>% 
    pull(model_name) 
prediction_h2o_leader <- h2o.predict(automl_leader, newdata = test_tbl_h2o)
prediction_h2o_leader %>% as.tibble()

error_tbl <- Port_FF_5_tbl %>% 
    filter(lubridate::year(date) == 2019) %>% 
    na.omit() %>% 
    add_column(pred = prediction_h2o_leader %>% as.tibble() %>% pull(predict)) %>% 
    rename(actual = Portfolio) %>% 
    mutate(
        error = actual - pred,
        error_pct = error/actual
    )
error_tbl
error_tbl %>% 
    summarize(
        me = mean(error),
        rmse = mean(error^2)^0.5,
        mae = mean(abs(error)),
        mape = mean(abs(error_pct)),
        mpe = mean(error_pct)
    )


source("00_Scripts/prediction_error_tbl.R")
pull_model_name(h2o_leaderboard = portfolio_automl_models_h2o,
                n = 3)


# Reproducibility prediction based on leaderboard models
model_1 <- prediction_error_tbl(Portfolio_tbl = Port_FF_5_tbl, 
                                                h2o_leaderboard = portfolio_automl_models_h2o, 
                                                n = 1, 
                                                test_tbl = test_tbl_h2o)
model_2 <- prediction_error_tbl(Portfolio_tbl = Port_FF_5_tbl, 
                                                 h2o_leaderboard = portfolio_automl_models_h2o, 
                                                 n = 2, 
                                                 test_tbl = test_tbl_h2o)
model_3 <- prediction_error_tbl(Portfolio_tbl = Port_FF_5_tbl, 
                                                         h2o_leaderboard = portfolio_automl_models_h2o, 
                                                         n = 3, 
                                                         test_tbl = test_tbl_h2o)



# Plot 
#Spooky theme

p_load("extrafont")
library(extrafont)
loadfonts(device="win") 
theme_spooky = function(base_size = 10, base_family = "Chiller") {
    
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        
        theme(
            # Specify axis options
            axis.line = element_blank(),  
            axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
            axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
            axis.ticks = element_line(color = "white", size  =  0.2),  
            axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
            axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
            axis.ticks.length = unit(0.3, "lines"),   
            # Specify legend options
            legend.background = element_rect(color = NA, fill = " gray10"),  
            legend.key = element_rect(color = "white",  fill = " gray10"),  
            legend.key.size = unit(1.2, "lines"),  
            legend.key.height = NULL,  
            legend.key.width = NULL,      
            legend.text = element_text(size = base_size*0.8, color = "white"),  
            legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
            legend.position = "none",  
            legend.text.align = NULL,  
            legend.title.align = NULL,  
            legend.direction = "vertical",  
            legend.box = NULL, 
            # Specify panel options
            panel.background = element_rect(fill = " gray10", color  =  NA),  
            #panel.border = element_rect(fill = NA, color = "white"),  
            panel.border = element_blank(),
            panel.grid.major = element_line(color = "grey35"),  
            panel.grid.minor = element_line(color = "grey20"),  
            panel.spacing = unit(0.5, "lines"),   
            # Specify facetting options
            strip.background = element_rect(fill = "grey30", color = "grey10"),  
            strip.text.x = element_text(size = base_size*0.8, color = "white"),  
            strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
            # Specify plot options
            plot.background = element_rect(color = " gray10", fill = " gray10"),  
            plot.title = element_text(size = base_size*1.2, color = "white",hjust=0,lineheight=1.25,
                                      margin=margin(2,2,2,2)),  
            plot.subtitle = element_text(size = base_size*1, color = "white",hjust=0,  margin=margin(2,2,2,2)),  
            plot.caption = element_text(size = base_size*0.8, color = "white",hjust=0),  
            plot.margin = unit(rep(1, 4), "lines")
            
        )
    
}

Port_FF_5_tbl %>% 
    na.omit() %>% 
    # filter(lubridate::year(date) >= 2018) %>% 
    ggplot(aes(date, Portfolio)) +
    geom_point(size = 2, color = "gray", alpha = 0.5, shape = 21, fill = "orange") +
    geom_line(color = "orange", size = 0.5) +
    geom_ma(n = 12, color = "white") +
    # Predictions - Spooky Purple (Model 1)
    geom_point(aes(y = pred), size = 2, color = "gray", alpha = 1, shape = 21, fill = "purple", data = model_1) +
    geom_line(aes(y = pred), color = "purple", size = 0.5, data = model_1) +
    # Predictions - Spooky Purple (Model 2)
    geom_point(aes(y = pred), size = 2, color = "gray", alpha = 1, shape = 21, fill = palette_light()[[2]], data = model_2) +
    geom_line(aes(y = pred), color = palette_light()[[2]], size = 0.5, data = model_2) +
    # Predictions - Spooky Purple (Model 3)
    geom_point(aes(y = pred), size = 2, color = "gray", alpha = 1, shape = 21, fill = palette_light()[[3]], data = model_3) +
    geom_line(aes(y = pred), color = palette_light()[[3]], size = 0.5, data = model_3) +
    # Aesthetics
    theme_spooky(base_size = 20, base_family = "Chiller") + 
    annotate("text", x = ymd("2019-05-01"), y = -0.015,
             color = "purple", label = pull_model_name(h2o_leaderboard = portfolio_automl_models_h2o,
                                                       n = 1)) +
    annotate("text", x = ymd("2019-02-01"), y = 0.04000,
             color = palette_light()[[2]], label = pull_model_name(h2o_leaderboard = portfolio_automl_models_h2o,
                                                                   n = 2)) + 
    annotate("text", x = ymd("2019-09-01"), y = -0.030,
             color = palette_light()[[3]], label = pull_model_name(h2o_leaderboard = portfolio_automl_models_h2o,
                                                       n = 3)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::percent_format())+
    labs(title = "Forecast of Portfolio Returns: 2019",
         subtitle = "MAPE = 89.9%",
         caption = "Ray Dalio's All Weather Portfolio",
         y = "Portfolio Returns", x = "Date") 


source(file = "00_Scripts/plot_FF_leaderboard.R")

portfolio_automl_models_h2o@leaderboard %>% 
    plot_FF_leaderboard(order_by = "mean", n_max = 5, pretty_breaks = 2, size = 3)

portfolio_automl_models_h2o@leaderboard %>% 
    plot_FF_leaderboard(order_by = "rmse", n_max = 5, pretty_breaks = 2, size = 3)

portfolio_automl_models_h2o@leaderboard %>% 
    plot_FF_leaderboard(order_by = "mae", n_max = 5, pretty_breaks = 2, size = 3)


GBM_h2o <- portfolio_automl_models_h2o@leaderboard %>% 
    as.tibble() %>% 
    filter(str_detect(model_id, "GBM")) %>% 
    slice(1) %>% 
    pull(model_id) %>% 
    h2o.getModel()

h2o.performance(GBM_h2o, newdata = as.h2o(test_tbl_h2o))

GBM_h2o@allparameters$learn_rate
GBM_h2o@allparameters$max_runtime_secs

GBM_h2o <- h2o.grid(
    algorithm = "gbm",
    x = x,
    y = y,
    grid_id = "GBM_grid_01",
    training_frame = train_tbl_h2o,
    # validation_frame = F,
    nfolds = 5,
    
    hyper_params = list(
        max_runtime_secs = 60
    )
)

h2o.getGrid(grid_id = "GBM_grid_01", sort_by = "mean_residual_deviance", decreasing = FALSE)
h2o.getModel("deeplearning_grid_01_model_9") %>% h2o.performance(newdata = as.h2o(test_tbl_h2o))




# Grid Search -----
deep_learning_h2o <- portfolio_automl_models_h2o@leaderboard %>% 
    as.tibble() %>% 
    filter(str_detect(model_id, "DeepLearning")) %>% 
    slice(1) %>% 
    pull(model_id) %>% 
    h2o.getModel()

h2o.performance(deep_learning_h2o, newdata = as.h2o(test_tbl_h2o))

deep_learning_h2o@allparameters$hidden
deep_learning_h2o@allparameters$epochs

deeplearning_grid_01 <- h2o.grid(
    algorithm = "deeplearning",
    x = x,
    y = y,
    grid_id = "deeplearning_grid_01",
    training_frame = train_tbl_h2o,
    # validation_frame = F,
    nfolds = 5,
    
    hyper_params = list(
        hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
        epochs = c(5, 10, 20)  #allow to generalize to new data
    )
)
h2o.getGrid(grid_id = "deeplearning_grid_01", sort_by = "mae", decreasing = FALSE)
h2o.getModel("deeplearning_grid_01_model_9") %>% h2o.performance(newdata = as.h2o(test_tbl_h2o))
