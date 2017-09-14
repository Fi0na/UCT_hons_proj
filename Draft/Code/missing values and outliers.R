#----------HOW ROBUST IS PROPHET WHEN THERE ARE MISSING VALUES AND OUTLIERS COMPARED TO ARIMA?-----------------

# Prophet is said to be robust when there are missing values whereas arima requires Kalman smoothing and interpolation
# Let's test this out

missing_eval <- function(Data){
  
# Insert missing values
ExchangeRates <- ExchangeRates %>% select(TradeDate, DailyLogReturns)
ExchangeRates[c(4, 10, 17, 20, 35, 48, 50, 100, 110, 177, 140, 157, 200, 230, 245, 270, 286, 300, 323, 330),2] <- NA

# Insert outliers
ExchangeRates[c( 206, 280, 307, 327, 330),2] <- c(0.4, 0.45, 0.7, 0.55, 0.6)

# ---------------------- SPITTING DATASET INTO IN-SAMPLE AND OUT-OF-SAMPLE -------------------

# Subsetting for six month test set:

# 6 month Test Set
SixMonth_OutOfSample <- ExchangeRates %>% 
  dplyr::filter(TradeDate >= "2017-01-17")

# 6 month Training Set
SixMonth_InSample <- ExchangeRates %>% 
  dplyr::filter(TradeDate < "2017-01-17")

# --- HOW DO THE FORECASTS FROM ARIMA & PROPHET COMPARE OVER DIFFERENT FORECAST HORIZONS? -----

library(forecast)
library(tseries)
library(prophet)

# ------- Box-Jenkins methodology for selection of arima

# 1) Series made stationary by finding log returns

daily_logret <- ExchangeRates %>%
  select(DailyLogReturns)

# Formal test for stationarity: ADF unit root test
adf.test(na.omit(daily_logret[[1]]), alternative = "stationary")
# p value = 0.01 - reject null hypothesis of non-stationarity

# Formal test for degree of differencing required
ndiffs(daily_logret[[1]])
# d = 0

# 2) Identify order of p and q

# ACF Plot - identify order of MA
ggAcf(daily_logret, lag = 12, main = "ACF Plot of Daily Log Returns")
# lag 3 is significantly different from zero
# Serial autocorrelation of daily log returns are small

# Formal test for serial correlation: Portmanteu's test
Box.test(daily_logret, lag = 12, type = "Ljung")
# p - value = 0.1038
# Fail to reject the null hypothesis of no serial correlations in log returns

# PACF Plot - identify order of AR
ggPacf(daily_logret, lag = 12, main = "PACF Plot of Daily Log Returns")
# lag 3 is significantly different from zero
# Serial autocorrelation of daily log returns are small

# 3) Estimation of model parameters

# Fitting arima models:
SixMonth_InSampleRet <- SixMonth_InSample %>% 
  select(DailyLogReturns)

SixMonth_OutOfSampleRet <- SixMonth_OutOfSample %>%
  select(DailyLogReturns)

arima_1 <- Arima(SixMonth_InSampleRet, order = c(3,0,0))   
summary(arima_1) 
AIC(arima_1)
# arima(3,0,0)
# AIC = -838.2674

arima_2 <- Arima(SixMonth_InSampleRet, order = c(0,0,3))   
summary(arima_2)
AIC(arima_2)
# arima(0,0,3)  
# AIC = -838.8153

arima_3 <- Arima(SixMonth_InSampleRet, order = c(3,0,3))   
summary(arima_3) 
AIC(arima_3)
# arima(3,0,3) 
# AIC = -833.3524

arima_4 <- Arima(SixMonth_InSampleRet, order = c(2,0,3))    
summary(arima_4)
AIC(arima_4)
# arima(2,0,3)
# AIC = -835.3454

arima_6 <- Arima(SixMonth_InSampleRet, order = c(1,0,3))    
summary(arima_6) 
AIC(arima_6)
# arima(1,0,3)
# AIC = -836.8464

arima_5 <- Arima(SixMonth_InSampleRet, order = c(3,0,2))    
summary(arima_5) 
AIC(arima_5)
# arima(3,0,2)
# AIC = -834.7463

arima_6 <- Arima(SixMonth_InSampleRet, order = c(3,0,1))    
summary(arima_6) 
AIC(arima_6)
# arima(3,0,1)
# AIC = -836.4061

# Based on the AIC, the model chosen for 6 month forecasting is the arima(3,0,0).
# Although the arima(0,0,3) has a smaller AIC than the arima(3,0,0), it is not used so that a rolling forecast may be conducted.

# 4) Diagnostic Check

# Checking residuals of arima(3,0,0) with mean
checkresiduals(arima_1)
# p-value = 0.9332
# no serial correlation in residuals at the 5% level - acf plot agrees

# Fitting Prophet model:
# No max of returns so setting trend to be linear
# Include weekly seasonality to account for little trading activity from Friday to Sunday and high activity around Tuesday and Wednesday 
# Don't really know about any yearly seasonal patterns in trading Bitcoin so not including it
# Don't yet know the dates of events that might impact the price but once known would be useful to include
# Some events that could impact in 2017: Segwit, etfs
# Some events that could impact later on: Brexit, 2020 reward halving
# Events that have already impacted price but dates were only know in retrospect: Japan legalising, China banning, hacking
# All other default values used

# Forecasting one-step ahead for 6 month out-of-sample 
h = 1

# Create out-of-sample period sequence
SixMonth_OutOfSample_Dates <- seq.Date(as.Date("2017-01-17"), 
                                       by = "day" ,
                                       length.out = nrow(SixMonth_OutOfSampleRet))

One_Step_Results <- list()

for(i in 1:length(SixMonth_OutOfSample_Dates)){
  
  # In-sample for arima
  SixMonth_InSam_arima <- ExchangeRates %>%
    dplyr::filter(TradeDate < SixMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  # In-sample in format that Prophet likes
  SixMonth_InSam_proph <- ExchangeRates %>%
    dplyr::filter(TradeDate < SixMonth_OutOfSample_Dates[i]) %>% 
    select(ds = TradeDate, y = DailyLogReturns)
  
  # Forecast horizon of 1 day
  SixMonth_OutSam <- ExchangeRates %>%
    dplyr::filter(TradeDate == SixMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  # t+1 observations
  actual <- SixMonth_OutSam %>% as.numeric
  
  # Fitting arima and prophet to in-sample
  arima_7 <- Arima(SixMonth_InSam_arima, order = c(3,0,0))  
  prophet_1 <- prophet(df = SixMonth_InSam_proph,
                       growth = "linear", 
                       weekly.seasonality = TRUE, 
                       yearly.seasonality = FALSE)
  
  # t+1 forecasts with arima
  One_step_arima_pred <- forecast(arima_7, h = h)
  arima_forecast <- One_step_arima_pred$mean %>% as.numeric
  arima_upper <- One_step_arima_pred$upper[,1] %>% as.numeric
  arima_lower <- One_step_arima_pred$lower[,1] %>% as.numeric 
  
  # t+1 forecasts with prophet
  proph1_dataframe <- make_future_dataframe(prophet_1, periods = h)
  proph_pred <- predict(prophet_1, proph1_dataframe) %>% select(yhat) %>% tail(1) %>% as.numeric
  proph_upper <- predict(prophet_1, proph1_dataframe) %>% select(yhat_upper) %>% tail(1) %>% as.numeric
  proph_lower <- predict(prophet_1, proph1_dataframe) %>% select(yhat_lower) %>% tail(1) %>% as.numeric
  
  # Dataframe of results
  One_Step_Results[[i]] <- data.frame(date = SixMonth_OutOfSample_Dates[i],
                                      actual = actual,
                                      arima_forecast = arima_forecast,
                                      arima_upper = arima_upper,
                                      arima_lower = arima_lower,
                                      proph_forecast = proph_pred,
                                      proph_upper = proph_upper,
                                      proph_lower = proph_lower) %>%
    mutate(arima_errors = actual - arima_forecast, proph_errors = actual - proph_forecast)
}

One_Step_Results <- do.call(rbind, One_Step_Results)

# Plot of 1-step ahead ARIMA forecasts
plot_one_step_arima <- ggplot(ExchangeRates, aes(x = TradeDate, y = DailyLogReturns)) +
  geom_line() +
  geom_line(data = One_Step_Results, aes(x = date, y = arima_forecast), colour = "red") +
  geom_ribbon(data = One_Step_Results, aes(x = date, ymin = arima_lower, ymax = arima_upper), alpha = 0.25, inherit.aes = FALSE) +
  xlab("Trade Date") +
  ylab("Daily Log Returns") +
  theme_minimal() + 
  labs(title  = paste("One-step ahead forecasts from ARIMA(3,0,0)"))

# Plot of 1 step-ahead Prophet forecasts
plot_one_step_proph <- ggplot(ExchangeRates, aes(x = TradeDate, y = DailyLogReturns)) +
  geom_line() +
  geom_line(data = One_Step_Results, aes(x = date, y = proph_forecast), colour = "blue") +
  geom_ribbon(data = One_Step_Results, aes(x = date, ymin = proph_lower, ymax = proph_upper), alpha = 0.25, inherit.aes = FALSE) +
  xlab("Trade Date") +
  ylab("Daily Log Returns") +
  theme_minimal() + 
  labs(title  = paste("One-step ahead Prophet forecasts"))

grid.arrange(plot_one_step_arima,
             plot_one_step_proph,
             nrow = 2)

# Forecasting 30-steps ahead for 6 month out-of-sample 
h = 30

# Create out of sample period sequence
SixMonth_OutOfSample_Dates <- seq.Date(as.Date("2017-01-17"), 
                                       by = "day" ,
                                       length.out = nrow(SixMonth_OutOfSampleRet))

Thirty_Step_Results <- list()

for(i in 1:(length(SixMonth_OutOfSample_Dates)-h)){
  
  # In-sample for arima
  SixMonth_InSam_arima <- ExchangeRates %>%
    dplyr::filter(TradeDate < SixMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  # In-sample in format that Prophet likes
  SixMonth_InSam_proph <- ExchangeRates %>%
    dplyr::filter(TradeDate < SixMonth_OutOfSample_Dates[i]) %>% 
    select(ds = TradeDate, y = DailyLogReturns)
  
  # Forecast horizon of 30 days
  SixMonth_OutSam <- ExchangeRates %>%
    dplyr::filter(TradeDate >= SixMonth_OutOfSample_Dates[i], TradeDate < SixMonth_OutOfSample_Dates[i+h]) %>% 
    select(DailyLogReturns)
  
  # Fitting arima and prophet to in-sample
  arima_8 <- Arima(SixMonth_InSam_arima, order = c(3,0,0))  
  prophet_2 <- prophet(df = SixMonth_InSam_proph,
                       growth = "linear", 
                       weekly.seasonality = TRUE, 
                       yearly.seasonality = FALSE)
  
  # t+30 forecasts with arima
  Thirty_step_arima_pred <- forecast(arima_8, h = h)
  arima_forecast <- Thirty_step_arima_pred$mean %>% as.numeric %>% tail(1)
  arima_upper <- Thirty_step_arima_pred$upper[,1] %>% as.numeric %>% tail(1)
  arima_lower <- Thirty_step_arima_pred$lower[,1] %>% as.numeric %>% tail(1)
  
  # t+30 forecasts with prophet
  proph2_dataframe <- make_future_dataframe(prophet_2, periods = h)
  proph_pred <- predict(prophet_2, proph2_dataframe) %>% select(yhat) %>% tail(1) %>% as.numeric
  proph_upper <- predict(prophet_2, proph2_dataframe) %>% select(yhat_upper) %>% tail(1) %>% as.numeric
  proph_lower <- predict(prophet_2, proph2_dataframe) %>% select(yhat_lower) %>% tail(1) %>% as.numeric
  
  # t+30 observations
  actual <- SixMonth_OutSam %>% tail(1) %>% as.numeric
  
  # Dataframe of results
  Thirty_Step_Results[[i]] <- data.frame(date = SixMonth_OutOfSample_Dates[i+h-1],
                                         actual = actual,
                                         arima_forecast = arima_forecast,
                                         arima_upper = arima_upper,
                                         arima_lower = arima_lower,
                                         proph_forecast = proph_pred,
                                         proph_upper = proph_upper,
                                         proph_lower = proph_lower) %>%
    mutate(arima_errors = actual - arima_forecast, proph_errors = actual - proph_forecast)
}

Thirty_Step_Results <- do.call(rbind, Thirty_Step_Results)

# Plot of 30-steps ahead ARIMA forecast
plot_Thirty_step_arima <- ggplot(ExchangeRates, aes(x = TradeDate, y = DailyLogReturns)) +
  geom_line() +
  geom_line(data = Thirty_Step_Results, aes(x = date, y = arima_forecast), colour = "red") +
  geom_ribbon(data = Thirty_Step_Results, aes(x = date, ymin = arima_lower, ymax = arima_upper), alpha = 0.25, inherit.aes = FALSE) +
  xlab("Trade Date") +
  ylab("Daily Log Returns") +
  theme_minimal() + 
  labs(title  = paste("30-step ahead forecasts from ARIMA(3,0,0)"))

# Plot of 30-steps ahead Prophet forecast
plot_Thirty_step_proph <- ggplot(ExchangeRates, aes(x = TradeDate, y = DailyLogReturns)) +
  geom_line() +
  geom_line(data = Thirty_Step_Results, aes(x = date, y = proph_forecast), colour = "blue") +
  geom_ribbon(data = Thirty_Step_Results, aes(x = date, ymin = proph_lower, ymax = proph_upper), alpha = 0.25, inherit.aes = FALSE) +
  xlab("Trade Date") +
  ylab("Daily Log Returns") +
  theme_minimal() + 
  labs(title  = paste("30-steps ahead Prophet forecasts"))

grid.arrange(plot_Thirty_step_arima,
             plot_Thirty_step_proph,
             nrow = 2)

# Forecasting 90-steps ahead for 6 month out-of-sample 
h = 90

# Create out of sample period sequence
SixMonth_OutOfSample_Dates <- seq.Date(as.Date("2017-01-17"), 
                                       by = "day" ,
                                       length.out = nrow(SixMonth_OutOfSampleRet))

Ninety_Step_Results <- list()

for(i in 1:(length(SixMonth_OutOfSample_Dates)-h)){
  
  # In-sample for arima
  SixMonth_InSam_arima <- ExchangeRates %>%
    dplyr::filter(TradeDate < SixMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  # In-sample in format that Prophet likes
  SixMonth_InSam_proph <- ExchangeRates %>%
    dplyr::filter(TradeDate < SixMonth_OutOfSample_Dates[i]) %>% 
    select(ds = TradeDate, y = DailyLogReturns)
  
  # Forecast horizon of 90 days
  SixMonth_OutSam <- ExchangeRates %>%
    dplyr::filter(TradeDate >= SixMonth_OutOfSample_Dates[i], TradeDate < SixMonth_OutOfSample_Dates[i+h]) %>% 
    select(DailyLogReturns)
  
  # Fitting arima and prophet to in-sample
  arima_9 <- Arima(SixMonth_InSam_arima, order = c(3,0,0))  
  prophet_3 <- prophet(df = SixMonth_InSam_proph,
                       growth = "linear", 
                       weekly.seasonality = TRUE, 
                       yearly.seasonality = FALSE)
  
  # t+90 forecasts with arima
  Ninety_step_arima_pred <- forecast(arima_9, h = h)
  arima_forecast <- Ninety_step_arima_pred$mean %>% as.numeric %>% tail(1)
  arima_upper <- Ninety_step_arima_pred$upper[,1] %>% as.numeric %>% tail(1)
  arima_lower <- Ninety_step_arima_pred$lower[,1] %>% as.numeric %>% tail(1)
  
  # t+90 forecasts with prophet
  proph3_dataframe <- make_future_dataframe(prophet_3, periods = h)
  proph_pred <- predict(prophet_3, proph3_dataframe) %>% select(yhat) %>% tail(1) %>% as.numeric
  proph_upper <- predict(prophet_3, proph3_dataframe) %>% select(yhat_upper) %>% tail(1) %>% as.numeric
  proph_lower <- predict(prophet_3, proph3_dataframe) %>% select(yhat_lower) %>% tail(1) %>% as.numeric
  
  # t+90 observations
  actual <- SixMonth_OutSam %>% tail(1) %>% as.numeric
  
  # Dataframe of results
  Ninety_Step_Results[[i]] <- data.frame(date = SixMonth_OutOfSample_Dates[i+h-1],
                                         actual = actual,
                                         arima_forecast = arima_forecast,
                                         arima_upper = arima_upper,
                                         arima_lower = arima_lower,
                                         proph_forecast = proph_pred,
                                         proph_upper = proph_upper,
                                         proph_lower = proph_lower) %>%
    mutate(arima_errors = actual - arima_forecast, proph_errors = actual - proph_forecast)
}

Ninety_Step_Results <- do.call(rbind, Ninety_Step_Results)

# Plot of 90-steps ahead ARIMA forecast
plot_ninety_step_arima <- ggplot(ExchangeRates, aes(x = TradeDate, y = DailyLogReturns)) +
  geom_line() +
  geom_line(data = Ninety_Step_Results, aes(x = date, y = arima_forecast), colour = "red") +
  geom_ribbon(data = Ninety_Step_Results, aes(x = date, ymin = arima_lower, ymax = arima_upper), alpha = 0.25, inherit.aes = FALSE) +
  xlab("Trade Date") +
  ylab("Daily Log Returns") +
  theme_minimal() + 
  labs(title  = paste("90-step ahead forecasts from ARIMA(3,0,0)"))

# Plot of 90-steps ahead Prophet forecast
plot_ninety_step_proph <- ggplot(ExchangeRates, aes(x = TradeDate, y = DailyLogReturns)) +
  geom_line() +
  geom_line(data = Ninety_Step_Results, aes(x = date, y = proph_forecast), colour = "blue") +
  geom_ribbon(data = Ninety_Step_Results, aes(x = date, ymin = proph_lower, ymax = proph_upper), alpha = 0.25, inherit.aes = FALSE) +
  xlab("Trade Date") +
  ylab("Daily Log Returns") +
  theme_minimal() + 
  labs(title  = paste("90-steps ahead Prophet forecasts"))

grid.arrange(plot_ninety_step_arima,
             plot_ninety_step_proph,
             nrow = 2)

# ------ Evaluating forecast accuracy of arima models

library(broom)
library(car)

# Mincer-Zarnowitz test for 1-step ahead forecast:

# Regression of actual and forecasted observations
one_step_arima_mincer <- One_Step_Results %>%  
  lm(actual ~ arima_forecast, data = .)

summary(one_step_arima_mincer)

one_step_arima_mincercoeff <- coef(one_step_arima_mincer)
# intercept = 0.01185925: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -0.883227: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
one_step_arima_hyp <- linearHypothesis(one_step_arima_mincer, c("(Intercept) = 0", "arima_forecast = 1")) 
one_step_arima_mincerpval <- tidy(one_step_arima_hyp) %>% select(p.value)
# Small p-value of 0.00003182484
# Reject the null hypothesis of efficiency and unbiasedness 

# Plot of regression line vs one-to-one line
plot_1step_arima_mincer <- ggplot(One_Step_Results, aes(x = arima_forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  scale_color_manual("", breaks = c("regression line", "one-to-one line"), values = c("regression line" = "blue", "one-to-one line" = "purple")) +
  labs(title = paste("intercept =", round(one_step_arima_mincercoeff[1], digits = 5),
                     "slope =", round(one_step_arima_mincercoeff[2], digits = 5),
                     "p value =", round(one_step_arima_mincerpval[2,], digits = 5)))

plot_1step_arima_mincer

# Finding MSE of 1 step-ahead forecasts:
one_step_arima_accuracy <- tidy(accuracy(One_Step_Results[,3], One_Step_Results[,2]))
one_step_arima_rmse <- one_step_arima_accuracy %>% select(RMSE)
one_step_arima_mse <- (one_step_arima_rmse %>% select(MSE = RMSE))^2
# MSE = 0.002072583

# Mincer-Zarnowitz test for 30-step ahead forecasts:

# Regression of actual and forecasted observations
thirty_step_arima_mincer <- Thirty_Step_Results %>%  
  lm(actual ~ arima_forecast, data = .)

summary(thirty_step_arima_mincer)

thirty_step_arima_mincercoeff <- coef(thirty_step_arima_mincer)
# intercept = 0.11414036: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = : -12.9853346 far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
thirty_step_arima_hyp <- linearHypothesis(thirty_step_arima_mincer, c("(Intercept) = 0", "arima_forecast = 1")) 
thirty_step_arima_mincerpval <- tidy(thirty_step_arima_hyp) %>% select(p.value)
# Small p-value of 0.1075322
# Reject the null hypothesis of efficiency and unbiasedness 

# Plot of regression line vs one-to-one line
plot_30step_arima_mincer <- ggplot(Thirty_Step_Results, aes(x = arima_forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  scale_color_manual("", breaks = c("regression line", "one-to-one line"), values = c("regression line" = "blue", "one-to-one line" = "purple")) +
  labs(title = paste("intercept =", round(thirty_step_arima_mincercoeff[1], digits = 5),
                     "slope =", round(thirty_step_arima_mincercoeff[2], digits = 5),
                     "p value =", round(thirty_step_arima_mincerpval[2,], digits = 5)))

plot_30step_arima_mincer

# Finding MSE of 30-steps ahead forecasts:
thirty_step_arima_accuracy <- tidy(accuracy(Thirty_Step_Results[,3], Thirty_Step_Results[,2]))
thirty_step_arima_rmse <- thirty_step_arima_accuracy %>% select(RMSE)
thirty_step_arima_mse <- (thirty_step_arima_rmse %>% select(MSE = RMSE))^2
# MSE = 0.002003289

# Mincer-Zarnowitz test for 90-step ahead forecasts:

# Regression of actual and forecasted observations
ninety_step_arima_mincer <- Ninety_Step_Results %>%  
  lm(actual ~ arima_forecast, data = .)

summary(ninety_step_arima_mincer)

ninety_step_arima_mincercoeff <- coef(ninety_step_arima_mincer)
# intercept = -0.226672873: negative bias estimated 
# forecast systematically overestimates the actual observation
# slope = 27.0635249: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
ninety_step_arima_hyp <- linearHypothesis(ninety_step_arima_mincer, c("(Intercept) = 0", "arima_forecast = 1")) 
ninety_step_arima_mincerpval <- tidy(ninety_step_arima_hyp) %>% select(p.value)
# Large p-value of 0.0129409634
# Fail to reject the null hypothesis of efficiency and unbiasedness 

# Plot of regression line vs one-to-one line
plot_90step_arima_mincer <- ggplot(Ninety_Step_Results, aes(x = arima_forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  scale_color_manual("", breaks = c("regression line", "one-to-one line"), values = c("regression line" = "blue", "one-to-one line" = "purple")) +
  labs(title = paste("intercept =", round(ninety_step_arima_mincercoeff[1], digits = 5),
                     "slope =", round(ninety_step_arima_mincercoeff[2], digits = 5),
                     "p value =", round(ninety_step_arima_mincerpval[2,], digits = 5)))

plot_90step_arima_mincer

# Finding MSE of 90-steps ahead forecasts:
ninety_step_arima_accuracy <- tidy(accuracy(Ninety_Step_Results[,3], Ninety_Step_Results[,2]))
ninety_step_arima_rmse <- ninety_step_arima_accuracy %>% select(RMSE)
ninety_step_arima_mse <- (ninety_step_arima_rmse %>% select(MSE = RMSE))^2
# MSE = 0.001789444

# ------ Evaluating forecast accuracy of Prophet

# Mincer-Zarnowitz test for 1-step forecast:

# Regression of actual and forecasted observations
one_step_proph_mincer <- One_Step_Results %>%  
  lm(actual ~ proph_forecast, data = .)

summary(one_step_proph_mincer)

one_step_proph_mincercoeff <- coef(one_step_proph_mincer)
# intercept = 0.01480189: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -0.6761324: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
one_step_proph_hyp <- linearHypothesis(one_step_proph_mincer, c("(Intercept) = 0", "proph_forecast = 1")) 
one_step_proph_mincerpval <- tidy(one_step_proph_hyp) %>% select(p.value)
# Small p-value of 0.000005315724
# Reject the null hypothesis of efficiency and unbiasedness 

# Plot of regression line vs one-to-one line
plot_1step_proph_mincer <- ggplot(One_Step_Results, aes(x = proph_forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  scale_color_manual("", breaks = c("regression line", "one-to-one line"), values = c("regression line" = "blue", "one-to-one line" = "purple")) +
  labs(title = paste("intercept =", round(one_step_proph_mincercoeff[1], digits = 5),
                     "slope =", round(one_step_proph_mincercoeff[2], digits = 5),
                     "p value =", round(one_step_proph_mincerpval[2,], digits = 5)))

plot_1step_proph_mincer

# Finding MSE of 1 step-ahead forecasts:
one_step_proph_accuracy <- tidy(accuracy(One_Step_Results[,6], One_Step_Results[,2]))
one_step_proph_rmse <- one_step_proph_accuracy %>% select(RMSE)
one_step_proph_mse <- (one_step_proph_rmse %>% select(MSE = RMSE))^2
# MSE = 0.002135501

# Mincer-Zarnowitz test for 30-step forecast:

# Regression of actual and forecasted observations
thirty_step_proph_mincer <- Thirty_Step_Results %>%  
  lm(actual ~ proph_forecast, data = .)

summary(thirty_step_proph_mincer)

thirty_step_proph_mincercoeff <- coef(thirty_step_proph_mincer)
# intercept = 0.01439573: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -0.6516095: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
thirty_step_proph_hyp <- linearHypothesis(thirty_step_proph_mincer, c("(Intercept) = 0", "proph_forecast = 1")) 
thirty_step_proph_mincerpval <- tidy(thirty_step_proph_hyp) %>% select(p.value)
# Small p-value of 0.000001479928
# Reject the null hypothesis of efficiency and unbiasedness 

# Plot of regression line vs one-to-one line
plot_30step_proph_mincer <- ggplot(Thirty_Step_Results, aes(x = proph_forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  scale_color_manual("", breaks = c("regression line", "one-to-one line"), values = c("regression line" = "blue", "one-to-one line" = "purple")) +
  labs(title = paste("intercept =", round(thirty_step_proph_mincercoeff[1], digits = 5),
                     "slope =", round(thirty_step_proph_mincercoeff[2], digits = 5),
                     "p value =", round(thirty_step_proph_mincerpval[2,], digits = 5)))

plot_30step_proph_mincer

# Finding MSE of 30 step-ahead forecasts:
thirty_step_proph_accuracy <- tidy(accuracy(Thirty_Step_Results[,6], Thirty_Step_Results[,2]))
thirty_step_proph_rmse <- thirty_step_proph_accuracy %>% select(RMSE)
thirty_step_proph_mse <- (thirty_step_proph_rmse %>% select(MSE = RMSE))^2
# MSE = 0.002321602

# Mincer-Zarnowitz test for 90-step forecast:

# Regression of actual and forecasted observations
ninety_step_proph_mincer <- Ninety_Step_Results %>%  
  lm(actual ~ proph_forecast, data = .)

summary(ninety_step_proph_mincer)

ninety_step_proph_mincercoeff <- coef(ninety_step_proph_mincer)
# intercept = -0.009290786: negative bias estimated 
# forecast systematically overestimates the actual observation
# slope = : 0.6438489 far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
ninety_step_proph_hyp <- linearHypothesis(ninety_step_proph_mincer, c("(Intercept) = 0", "proph_forecast = 1")) 
ninety_step_proph_mincerpval <- tidy(ninety_step_proph_hyp) %>% select(p.value)
# p-value = 0.0007635404
# Fail to reject the null hypothesis of efficiency and unbiasedness 

# Plot of regression line vs one-to-one line
plot_90step_proph_mincer <- ggplot(Ninety_Step_Results, aes(x = proph_forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  scale_color_manual("", breaks = c("regression line", "one-to-one line"), values = c("regression line" = "blue", "one-to-one line" = "purple")) +
  labs(title = paste("intercept =", round(ninety_step_proph_mincercoeff[1], digits = 5),
                     "slope =", round(ninety_step_proph_mincercoeff[2], digits = 5),
                     "p value =", round(ninety_step_proph_mincerpval[2,], digits = 5)))

plot_90step_proph_mincer

# Finding MSE of 90 step-ahead forecasts:
ninety_step_proph_accuracy <- tidy(accuracy(Ninety_Step_Results[,6], Ninety_Step_Results[,2]))
ninety_step_proph_rmse <- ninety_step_proph_accuracy %>% select(RMSE)
ninety_step_proph_mse <- (ninety_step_proph_rmse %>% select(MSE = RMSE))^2
# MSE = 0.002030498


# -------- Comparing Prophet and Arima results over the different forecast horizons

library(knitr)

# Prophet results for 1-step ahead 
one_step_proph_mincercoef <- data.frame(one_step_proph_mincercoeff) %>% 
  select(Prophet = one_step_proph_mincercoeff)

one_step_proph_mincerp <- na.omit(one_step_proph_mincerpval %>% 
                                    select(Prophet = p.value))

one_step_proph_mse <- data.frame(one_step_proph_mse) %>% select(Prophet = MSE)

one_step_proph_mincer <- rbind(one_step_proph_mincercoef, one_step_proph_mincerp, one_step_proph_mse) 

# ARIMA results for 1-step ahead
one_step_arima_mincercoef <- data.frame(one_step_arima_mincercoeff) %>% 
  select(ARIMA = one_step_arima_mincercoeff)

one_step_arima_mincerp <- na.omit(one_step_arima_mincerpval %>% 
                                    select(ARIMA = p.value))

one_step_arima_mse <- data.frame(one_step_arima_mse) %>% select(ARIMA = MSE)

one_step_arima_mincer <- rbind(one_step_arima_mincercoef, one_step_arima_mincerp, one_step_arima_mse)

# Table of 1-step ahead results
mincer_table1 <- t(data.frame(one_step_arima_mincer, one_step_proph_mincer, row.names = c("intercept", "slope", "p-value", "MSE")))
kable(mincer_table1)
# Arima forecasts are less biased
# Prophet forecasts are more efficient
# Both arima and prophet forecasts are inefficient and biased
# 1-step forecasts are better for arima than prophet
# MSE of arima smaller than prophet - agrees with MZ test

# Prophet results for 30-steps ahead 
thirty_step_proph_mincercoef <- data.frame(thirty_step_proph_mincercoeff) %>% 
  select(Prophet = thirty_step_proph_mincercoeff)

thirty_step_proph_mincerp <- na.omit(thirty_step_proph_mincerpval %>% 
                                       select(Prophet = p.value))

thirty_step_proph_mse <- data.frame(thirty_step_proph_mse) %>% select(Prophet = MSE)

thirty_step_proph_mincer <- rbind(thirty_step_proph_mincercoef, thirty_step_proph_mincerp, thirty_step_proph_mse) 

# ARIMA results for 30-steps ahead
thirty_step_arima_mincercoef <- data.frame(thirty_step_arima_mincercoeff) %>% 
  select(ARIMA = thirty_step_arima_mincercoeff)

thirty_step_arima_mincerp <- na.omit(thirty_step_arima_mincerpval %>% 
                                       select(ARIMA = p.value))

thirty_step_arima_mse <- data.frame(thirty_step_arima_mse) %>% select(ARIMA = MSE)

thirty_step_arima_mincer <- rbind(thirty_step_arima_mincercoef, thirty_step_arima_mincerp, thirty_step_arima_mse)

# Table of 30-step ahead results
mincer_table2 <- t(data.frame(thirty_step_arima_mincer, thirty_step_proph_mincer, row.names = c("intercept", "slope", "p-value", "MSE")))
kable(mincer_table2)
# Prophet forecasts are less biased
# Prophet forecasts are more efficient
# Both arima and prophet forecasts are inefficient and biased
# 30-step forecasts are better for arima than prophet
# MSE of arima smaller than prophet - agrees with MZ test

# Prophet results for 90-steps ahead 
ninety_step_proph_mincercoef <- data.frame(ninety_step_proph_mincercoeff) %>% 
  select(Prophet = ninety_step_proph_mincercoeff)

ninety_step_proph_mincerp <- na.omit(ninety_step_proph_mincerpval %>% 
                                       select(Prophet = p.value))

ninety_step_proph_mse <- data.frame(ninety_step_proph_mse) %>% select(Prophet = MSE)

ninety_step_proph_mincer <- rbind(ninety_step_proph_mincercoef, ninety_step_proph_mincerp, ninety_step_proph_mse) 

# ARIMA results for 90-steps ahead
ninety_step_arima_mincercoef <- data.frame(ninety_step_arima_mincercoeff) %>% 
  select(ARIMA = ninety_step_arima_mincercoeff)

ninety_step_arima_mincerp <- na.omit(ninety_step_arima_mincerpval %>% 
                                       select(ARIMA = p.value))

ninety_step_arima_mse <- data.frame(ninety_step_arima_mse) %>% select(ARIMA = MSE)

ninety_step_arima_mincer <- rbind(ninety_step_arima_mincercoef, ninety_step_arima_mincerp, ninety_step_arima_mse)

# Table of 90-step ahead results
mincer_table3 <- t(data.frame(ninety_step_arima_mincer, ninety_step_proph_mincer, row.names = c("intercept", "slope", "p-value", "MSE")))
kable(mincer_table3)

# Table of Mincer-Zarnowitz results over all forecast horizons
table_pvalues <- cbind(data.frame(mincer_table1) %>% select(one_step = p.value),
                       data.frame(mincer_table2) %>% select(thirty_steps = p.value),
                       data.frame(mincer_table3) %>% select(ninety_steps = p.value)) 

kable(table_pvalues)

# Table of MSE's over all forecast horizons
table_MSE <- cbind(data.frame(mincer_table1) %>% select(one_step = MSE),
                   data.frame(mincer_table2) %>% select(thirty_steps = MSE),
                   data.frame(mincer_table3) %>% select(ninety_steps = MSE)) 

kable(table_MSE)
# MSE for ARIMA is lower than Prophet over all forecast horizons - agrees with Mincer-Zarnowitz results 


}

