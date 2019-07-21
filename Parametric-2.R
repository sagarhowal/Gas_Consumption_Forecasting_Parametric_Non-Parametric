library(fpp2)
library(astsa)
library(tseries)
library(forecast)
library(dplyr)
library(prophet)




gas_ts <- ts(DT_final$value, frequency = 48)
plot(gas_ts) # visualizing the time series

train <- gas_ts[1:24480] # training data - all data except last six months
test <- gas_ts[24481:25920] # test data - just the last six months

plot(train) # visualizing train and test data
lines(test, col = 'red') 

################################################################################################################
# ARIMA Model


#Check for stationarity
plot(diff(log(train))) # using a diff and a log to stationarize the data

adf.test(train)
# data:  train
# Dickey-Fuller = -14.144, Lag order = 29, p-value = 0.01
# alternative hypothesis: stationary

adf.test(diff(log(train))) # passes the Dickey Fuller test
# Augmented Dickey-Fuller Test
# data:  diff(log(train))
# Dickey-Fuller = -40.166, Lag order = 29, p-value = 0.01
# alternative hypothesis: stationary



acf2(train) # acf tails off - pacf kinda tails sorta

auto.arima(train) 
# Series: train 
# ARIMA(2,1,4) 
# 
# Coefficients:
#   ar1     ar2     ma1     ma2     ma3     ma4
# -0.2378  0.3475  1.3541  0.9216  0.5485  0.1924
# s.e.   0.0552  0.0371  0.0547  0.0319  0.0230  0.0090
# 
# sigma^2 estimated as 23796:  log likelihood=-158072.9
# AIC=316159.7   AICc=316159.8   BIC=316216.5

# first model --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
fit <- sarima(log(train), p=2, d=1, q=4)
print(cbind(fit$AIC, fit$BIC)) # look at AIC and BIC of model
#           [,1]       [,2]
# [1,] -0.7914674 -0.7888184


fit$ttable 
# Estimate     SE t.value p.value
# ar1        0.9120 0.0505 18.0618  0.0000
# ar2       -0.3025 0.0316 -9.5822  0.0000
# ma1       -0.2168 0.0503 -4.3127  0.0000
# ma2        0.3246 0.0112 29.0448  0.0000
# ma3        0.1448 0.0142 10.2004  0.0000
# ma4        0.1216 0.0123  9.8709  0.0000
# constant  -0.0001 0.0037 -0.0164  0.9869





#------------------------ COntine from here


sarima_gas <- as.ts(sarima.for(train, n.ahead = 50, p=2, d=1, q=4))
lines(test[1:50], col = 'blue') # compare predicted and actual
accuracy(sarima_gas$pred, test[1:50]) # look at accuracy of model
# ME     RMSE      MAE      MPE     MAPE
# Test set 359.9859 547.6289 380.2201 38.12458 60.97544




# second model --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
fit2 <- sarima(log(train), p=2, d=1, q=4)
print(cbind(fit2$AIC, fit2$BIC)) # look at AIC and BIC of model
fit2$ttable 

sarima_gas2 <- as.ts(sarima.for(train, n.ahead = 50, p=2, d=1, q=4))
lines(test, col = 'blue') # compare predicted and actual
accuracy(sarima_oil2$pred, test) # look at accuracy of model

# third model --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
fit3 <- sarima(log(train), p=2, d=1, q=2, P=1, D=0, Q=2, S=12)
print(cbind(fit3$AIC, fit3$BIC)) # look at AIC and BIC of model
fit3$ttable 

sarima_oil3 <- as.ts(sarima.for(train, n.ahead = 6, p=2, d=1, q=2, P=1, D=0, Q=2, S=12))
lines(test, col = 'blue') # compare predicted and actual
accuracy(sarima_oil3$pred, test) # look at accuracy of model

# forth model --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
fit4 <- sarima(log(train), p=2, d=1, q=1, P=1, D=0, Q=1, S=12)
print(cbind(fit4$AIC, fit4$BIC)) # look at AIC and BIC of model
fit4$ttable 

sarima_oil4 <- as.ts(sarima.for(train, n.ahead = 6, p=2, d=1, q=1, P=1, D=0, Q=1, S=12))
lines(test, col = 'blue') # compare predicted and actual
accuracy(sarima_oil4$pred, test) # look at accuracy of model

# ARIMA Summary:
# first model was best - slightly less accurate than third model but less complex
# AR only model was not as good as first model even though suggested by ACF and PACF plots


################################################################################################################
# Exponetial Smoothing Model

# not trying simple exponetial smoothing due to trend in data

# holt model
fit_holt <- holt(train, h = 12)
summary(fit_holt)
checkresiduals(fit_holt) # checking for autocorrelation in residuals
autoplot(fit_holt) + autolayer(fitted(fit_holt)) # plotting model fit
accuracy(fit_holt, test) # checking model accuracy

for_fit <- forecast(fit_holt, h = 6) # forecast test values
plot(for_fit) # plot the predicted data
lines(test, col = 'red')

# holt model damped
fit_holt_d <- holt(train, h = 12, damped = TRUE)
summary(fit_holt_d)
checkresiduals(fit_holt_d)
autoplot(fit_holt_d) + autolayer(fitted(fit_holt_d))
accuracy(fit_holt_d, test)

for_fit2 <- forecast(fit_holt_d, h = 6)
plot(for_fit2)
lines(test, col = 'red')

# holt winters model - dont expect this model to work super well - no clear seasonality in the data
fit_hw <- hw(train, seasonal = "multiplicative")
summary(fit_hw)
checkresiduals(fit_hw)
autoplot(fit_hw) + autolayer(fitted(fit_hw))
accuracy(fit_hw, test)

for_fit3 <- forecast(fit_hw, h = 6)
plot(for_fit3)
lines(test, col = 'red')

# Exponential Smoothing Summary:
# holt model seems to predict the best
# makes sense as there is a trend but no seasonality

################################################################################################################
# Phophet Model
head(oil) 
colnames(oil)[1] <- 'ds' # renaming columns for prophet modeling
colnames(oil)[2] <- 'y'

oil_train = oil[1:380,] # re-subsetting data - prophet requires dataframe instead of ts
oil_test = oil[381:386,]

prophet_fit <- prophet(oil_train) 
future <- make_future_dataframe(prophet_fit, periods = 6, freq = 'month') # make df for forecasted predictions
forecast <- predict(prophet_fit, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(prophet_fit, forecast)
prophet_plot_components(prophet_fit, forecast)
accuracy(forecast[c('yhat')][380:386,], test) # checking model accuracy

# Prophet model summary:
# fits better than exponential smoothing models but not as well as final ARIMA model


