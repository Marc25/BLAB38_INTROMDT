# Ex Learning Mod Time
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(lubridate)
# P1 : Read Data ----
pathData <- "/cloud/project/data/"
dtEntB <- readRDS("/cloud/project/data/gtrends_list.rds")
gtrends_interest_over_time <- gtrends_list$interest_over_time
# P2.1 Visualize results ----
gtrends_interest_over_time%>%
  plot_time_series(date, hits)

gtrends_interest_over_time%>%

# P2.2 Preprocesing ----
library(data.table)
dtgtrends <- as.data.table(gtrends_interest_over_time) 
dtgtrends[,hits:=ifelse(hits==0, NA, hits)]

dtgtrends[, hits:=ts_impute_vec(hits, period = 12)]

dtgtrends%>% plot_time_series(date,hits)
setnames(dtgtrends, old = c("hits"), new = c("value"))

dtgtrends%>%
  plot_seasonal_diagnostics(.date_var = date,.value=value,
                          .interactive = F) +
  labs(
    title = "gtrends Seasonal Diagnostics",
    y = "hits",
    caption = "FAD"
  )
# P2.3 Splits Train and Test ----
splits <- time_series_split(dtgtrends[,.(date,value)], 
                            assess = "1 year",cumulative = TRUE)
splits %>% 
  tk_time_series_cv_plan()%>%
  plot_time_series_cv_plan(date,value)  

summary(splits)
library(tidymodels)

# P2 Models Engine ----
model_fil_arima <- arima_reg() %>% 
  set_engine("auto_arima") %>%
  fit(
    value ~ date, data=training(splits)
      )

model_fil_arima$fit$data$.residuals

library(forecast)
checkresiduals(model_fil_arima$fit$data$.residuals)

model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(
    value ~ date, 
    data = training(splits)
  )

# * LINEAR REGRESSION ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * LINEAR REGRESSION - NO TREND ----
model_fit_lm_no_trend <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    value ~ as.numeric(date)+ month(date), 
    data = training(splits)
  )

model_fit_lm_no_trend$fit$model
summary(model_fit_lm_no_trend)
# P3 TBL Models ----
model_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_lm_no_trend
  )

model_tbl

# P4. Calibration ----
calibration_tbl <- model_tbl %>% 
  modeltime_calibrate(testing(splits))
# P4.1 Accuracy ----
calibration_tbl %>%
  modeltime_accuracy()%>%
  table_modeltime_accuracy(resizable=TRUE,
                           bordered=TRUE)
# P4.2 Plot forecast calibration test ----
calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = dtgtrends[,.(date,value)],
    conf_interval = 0.8
  ) %>%
  plot_modeltime_forecast(.legend_show = TRUE,
                          .legend_max_width = 25)
# P5 Refit ----
refit_tbl <- calibration_tbl %>%
  modeltime::modeltime_refit(data = dtgtrends[,.(date,value)])

refit_tbl$

forecast_tbl <- refit_tbl[refit_tbl$.model_id==1,]%>%
  modeltime_forecast(
    h = "1 year",
    actual_data = dtgtrends[,.(date,value)],
    conf_interval = 0.80
  ) 

tail(forecast_tbl,3)
table(forecast_tbl$.key)

# save Artifacts 
saveRDS(refit_tbl, "/cloud/project/data/REFITMODEL.RDS")
MODARTF <- readRDS("/cloud/project/data/REFITMODEL.RDS")
dtPredictions <- MODARTF%>% modeltime_forecast(h=12, 
                              actual_data = dtgtrends[,.(date,value)],
                              conf_interval = 0.90
                              )


tail(dtPredictions,14)
