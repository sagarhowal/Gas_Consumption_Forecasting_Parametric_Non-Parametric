#Time Series Forecasting


#Load Data
library(data.table)
library(feather)
DT_ens <- read_feather("DT_ens.feather")
DT_ens <- data.table(DT_ens)

n_date <- unique(DT_ens[, date])
period <- 48

#Values required for building the models
n_weekdays <- c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday")


## STL + ARIMA
library(forecast)
stlARIMAPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period) # Transform to ts
  dekom <- stl(ts_Y, s.window="periodic", robust = T) # STL decomposition
  arima <- forecast(dekom, h = period, method = "arima") # Predict decomposed time series with ARIMA
  return(as.vector(arima$mean))
}

## STL + EXP
stlEXPPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period) # Transform to ts
  dekom <- stl(ts_Y, s.window = "periodic", robust = T) # STL decomposition
  expo <- forecast(dekom, h = period, method = "ets", etsmodel = "ZZN") # Predict decomposed time series with Exponential Smoothing
  return(as.vector(expo$mean))
}

## Optional other methods - HW and snaive:
## Holt-Winters ES
HWPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period) # Transform to ts
  HW <- HoltWinters(ts_Y, beta = F, seasonal = "additive") # Holt-Winters ES , alpha = 0.15, gamma = 0.95
  HW_pred <- forecast(HW, period) # Predict
  return(as.vector(HW_pred$mean))
}

## Seasonal naive forecast
snaivePred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  naive_pred <- snaive(ts_Y, period)
  return(as.vector(naive_pred$mean))
}

## Function to return forecast of length one week
predictWeek <- function(data, set_of_date, FUN, train_win = 6){
  
  for_mon <- FUN(data[(week == n_weekdays[1] & date %in% set_of_date), value])
  seq_tuethu <- data[(week %in% n_weekdays[2:4] & date %in% set_of_date), value]
  for_tuethu <- as.vector(sapply(2:0, function(j) FUN(seq_tuethu[(length(seq_tuethu)-(period*j)+1-(train_win*period)):(length(seq_tuethu)-(period*j))])))
  for_fri <- FUN(data[(week == n_weekdays[5] & date %in% set_of_date), value])
  for_sat <- FUN(data[(week == n_weekdays[6] & date %in% set_of_date), value])
  for_sun <- FUN(data[(week == n_weekdays[7] & date %in% set_of_date), value])
  
  return(c(for_mon, for_tuethu, for_fri, for_sat, for_sun))
}

## Mean Absolute Percentage Error
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}


#Modelling

for_week_arima <- predictWeek(DT_ens, n_date[56:84], stlARIMAPred)
for_week_exp <- predictWeek(DT_ens, n_date[56:84], stlEXPPred)
for_week_naive <- predictWeek(DT_ens, n_date[56:84], snaivePred)
real_week <- DT_ens[date %in% n_date[85:91], value]


#Error - MAPE
mape(real_week, for_week_arima) #6.130705
mape(real_week, for_week_exp) #6.627346
mape(real_week, for_week_naive) #5.673666










