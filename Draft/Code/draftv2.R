
# ------------------------------- READING IN DATASET --------------------------------------

# Loading required packages
library(rmsfuns)
packagestoload <- c("stats", "xts", "dplyr", "Dplyr2Xts", "PerformanceAnalytics", "ggplot2", "readr", "gridExtra", "lmtest")
load_pkg(packagelist = packagestoload) 

# Set relative path
setwd("~/R/UCT_hons_proj/Draft/Data")
ExchangeRates <- read.csv("bitxZAR.csv.gz", header = FALSE, sep = ",", dec = ".")

# Looking at first few rows of dataset
ExchangeRates %>% 
  data.frame %>% 
  head

# Looking at structure of dataset
dim(ExchangeRates)
str(ExchangeRates)

#--------------------------------- CLEANING THE DATASET --------------------------------------

# Make unixtime stamp a POSIX object 
ExchangeRates$V1 <- as.POSIXct(ExchangeRates$V1, tz = "GMT", origin = "1970-01-01")

# Convert dataframe to xts
ExchangeRates <- xts(ExchangeRates[,-1], order.by = ExchangeRates[,1])

# Agregate prices by day
ExchangeRates <- to.daily(ExchangeRates, period = "days", drop.time = TRUE, OHCL = TRUE, k = 1)

# Convert xts back to dataframe
ExchangeRates <- data.frame(date = index(ExchangeRates), coredata(ExchangeRates))

# Set column names of dataframe
ExchangeRates <- ExchangeRates %>% 
  select(TradeDate = date, 
         OpenPrice = ExchangeRates.Open, 
         HighPrice = ExchangeRates.High, 
         LowPrice = ExchangeRates.Low, 
         ClosePrice = ExchangeRates.Close)

str(ExchangeRates)

# Checking if there are any missing values
na_true <- is.na.data.frame(ExchangeRates)
which(na_true)

# Set total sample size
ExchangeRates <- ExchangeRates %>%
  dplyr::filter(TradeDate >= "2016-01-24")

# -------------------------------- EXPLORATORY DATA ANALYSIS ----------------------------------

# Plot of closing prices
plot_prices <- ggplot(ExchangeRates, aes(TradeDate, ClosePrice)) + 
  geom_line() +
  xlab("Trade Date") +
  ylab("Closing Prices") + 
  theme_minimal() + 
  labs(title  = paste("BTC Price from", 
                      ExchangeRates$TradeDate[1],
                      "to",
                      ExchangeRates$TradeDate[nrow(ExchangeRates)]))

plot_prices 
# No unusual observations
# There's a certain degree of variability and a general upward trend

# Calculating daily log returns
ExchangeRates <- 
  ExchangeRates %>%
  mutate(DailyLogReturns = c(NA, diff(log(ClosePrice), lag = 1)))

# Plot of daily log returns
plot_returns <- ggplot(ExchangeRates, aes(TradeDate, DailyLogReturns)) + 
  geom_line() +
  xlab("Trade Date") +
  ylab("Daily Log Returns") +
  theme_minimal() + 
  labs(title  = paste("BTC Returns from", 
                      ExchangeRates$TradeDate[1],
                      "to",
                      ExchangeRates$TradeDate[nrow(ExchangeRates)]))

plot_returns 
# Returns fluctutate around zero and lies within a fixed range with the exception of few extremes
# Range of daily log returns is roughly [-0.01 ; 0.1] throughout the sample span
# No trend and constant variance of daily log returns over time
# Daily log returns weakly stationary

# Summary statistics of closing prices and returns
library(fBasics)

close_prices <- ExchangeRates %>%
  select(ClosePrice)

basicStats(close_prices)

daily_logret <- ExchangeRates %>%
  select(DailyLogReturns)

basicStats(daily_logret)

# Testing for outliers
library(outliers)
outlier_check <- scores(daily_logret[,1], type = "chisq", prob = 0.9)
outlier_indices <- which(outlier_check)
outlier_values <- daily_logret[outlier_indices,1]
# No outliers present

# ---------------------- SPITTING DATASET INTO IN-SAMPLE AND OUT-OF-SAMPLE -------------------

# Subsetting for six month test set:

# 6 month Test Set
SixMonth_OutOfSample <- ExchangeRates %>% 
  dplyr::filter(TradeDate >= "2017-01-17")

# 6 month Training Set
SixMonth_InSample <- ExchangeRates %>% 
  dplyr::filter(TradeDate < "2017-01-17")

# Plots of 6 month training and test set closing prices
plot_6MonthTestPrices <- plot_prices %+% 
  SixMonth_OutOfSample + 
  ggtitle("6 Month Test Set Closing Prices")

plot_6MonthTrainPrices <- plot_prices %+% 
  SixMonth_InSample + 
  ggtitle("6 Month Training Set Closing Prices") + 
  theme_minimal()

grid.arrange(plot_6MonthTrainPrices,
             plot_6MonthTestPrices,
             nrow = 2)

# Plots of 6 month training and test set daily log returns
plot_6MonthTestReturns <- plot_returns %+% 
  SixMonth_OutOfSample + 
  ggtitle("6 Month Test Set Daily Log Returns")

plot_6MonthTrainReturns <- plot_returns %+% 
  SixMonth_InSample + 
  ggtitle("6 Month Training Set Daily Log Returns") + 
  theme_minimal()

grid.arrange(plot_6MonthTrainReturns,
             plot_6MonthTestReturns,
             nrow = 2)

# --- HOW DO THE FORECASTS FROM ARIMA & PROPHET COMPARE OVER DIFFERENT FORECAST HORIZONS? -----

library(forecast)
library(tseries)
library(prophet)

# ------- Box-Jenkins methodology for selection of arima

# 1) Series made stationary by finding log returns

# Formal test for stationarity: ADF unit root test
adf.test(na.omit(daily_logret[[1]]), alternative = "stationary")
# p value = 0.01 - reject null hypothesis of non-stationarity

# Formal test for degree of differencing required
ndiffs(daily_logret[[1]])
# d = 0

# 2) Identify order of p and q

# ACF Plot - identify order of MA
ggAcf(daily_logret, lag = 12, main = "ACF Plot of Daily Log Returns")
# lag 9 is significantly different from zero
# Serial autocorrelation of daily log returns are small

# Formal test for serial correlation: Portmanteu's test
Box.test(daily_logret, lag = 12, type = "Ljung")
# p - value = 0.1038
# Fail to reject the null hypothesis of no serial correlations in log returns

# PACF Plot - identify order of AR
ggPacf(daily_logret, lag = 12, main = "PACF Plot of Daily Log Returns")
# lag 9 is significantly different from zero
# Serial autocorrelation of daily log returns are small

# 3) Estimation of model parameters

# Fitting arima models:
SixMonth_InSampleRet <- SixMonth_InSample %>% 
  select(DailyLogReturns)

SixMonth_OutOfSampleRet <- SixMonth_OutOfSample %>%
  select(DailyLogReturns)

arima_1 <- Arima(na.omit(SixMonth_InSampleRet), order = c(0,0,0))   
summary(arima_1) 
AIC(arima_1)
# arima(0,0,0)
# AIC = -1617.458

arima_2 <- Arima(na.omit(SixMonth_InSampleRet), order = c(1,0,0))   
summary(arima_2)
AIC(arima_2)
# arima(1,0,0)  
# AIC = -1618.927

arima_3 <- Arima(na.omit(SixMonth_InSampleRet), order = c(0,0,1))   
summary(arima_3) 
AIC(arima_3)
# arima(0,0,1) 
# AIC = -1619.124

arima_4 <- Arima(na.omit(SixMonth_InSampleRet), order = c(1,0,1))    
summary(arima_4)
AIC(arima_4)
# arima(1,0,1)
# AIC = -1617.202

arima_5 <- Arima(na.omit(SixMonth_InSampleRet), order = c(2,0,1))    
summary(arima_5) 
AIC(arima_5)
# arima(2,0,1)
# AIC = -1615.317

arima_6 <- Arima(na.omit(SixMonth_InSampleRet), order = c(1,0,2))    
summary(arima_6) 
AIC(arima_6)
# arima(1,0,2)
# AIC = -1615.271

# Based on the AIC, the model chosen for 6 month forecasting is the arima(1,0,0).
# Although the arima(0,0,1) has a smaller AIC than the arima(1,0,0), it is not used so that a rolling forecast may be conducted.

# 4) Diagnostic Check

# Checking residuals of arima(1,0,0) with mean
checkresiduals(arima_2)
# p-value = 0.05537
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


# Check if model selection agrees with automatic model selection:

# Automatically selected arima model
auto_arima <- auto.arima(na.omit(SixMonth_InSampleRet), seasonal = FALSE) 
summary(SixMonth_AutoArima)
# arima(1,0,0) with zero mean fitted 
# manually chose arima(1,0,0) with a non-zero mean

# Automatic selection of Prophet model 
auto_proph <- prophet(df = SixMonth_InSample %>% select(ds = TradeDate, y = DailyLogReturns))
# Model fitted with linear trend and includes weekly seasonality
# Same model chosen manually

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
  arima_7 <- Arima(na.omit(SixMonth_InSam_arima), order = c(1,0,0))  
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
  labs(title  = paste("1-step ahead forecasts from ARIMA(1,0,0)"))

# Plot of 1 step-ahead Prophet forecasts
plot_one_step_proph <- ggplot(ExchangeRates, aes(x = TradeDate, y = DailyLogReturns)) +
  geom_line() +
  geom_line(data = One_Step_Results, aes(x = date, y = proph_forecast), colour = "blue") +
  geom_ribbon(data = One_Step_Results, aes(x = date, ymin = proph_lower, ymax = proph_upper), alpha = 0.25, inherit.aes = FALSE) +
  xlab("Trade Date") +
  ylab("Daily Log Returns") +
  theme_minimal() + 
  labs(title  = paste("1-step ahead Prophet forecasts"))

grid.arrange(plot_one_step_arima,
             plot_one_step_proph,
             nrow = 2)

# One-step Model Confidence Set
library(MCS)

# One-step squared error losses
one_step_arima_losses <- LossLevel(realized = One_Step_Results %>% select(actual),
                                   evaluated = One_Step_Results %>% select(arima_forecast), 
                                   which = "SE")

one_step_proph_losses <- LossLevel(realized = One_Step_Results %>% select(actual),
                                   evaluated = One_Step_Results %>% select(proph_forecast), 
                                   which = "SE")

one_step_losses <- data.frame(one_step_arima_losses, one_step_proph_losses)

one_step_losses <- one_step_losses %>% 
  select(ARIMA = arima_forecast, Prophet = proph_forecast)

# One-step MCS
one_step_MCS <- MCSprocedure(Loss = one_step_losses, alpha = 0.10, B = 5000, statistic = "TR")

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
  arima_8 <- Arima(na.omit(SixMonth_InSam_arima), order = c(1,0,0))  
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
  labs(title  = paste("30-step ahead forecasts from ARIMA(1,0,0)"))

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

# Thirty-step Model Confidence Set

# Thirty-step squared error losses
thirty_step_arima_losses <- LossLevel(realized = Thirty_Step_Results %>% select(actual),
                                   evaluated = Thirty_Step_Results %>% select(arima_forecast), 
                                   which = "SE")

thirty_step_proph_losses <- LossLevel(realized = Thirty_Step_Results %>% select(actual),
                                   evaluated = Thirty_Step_Results %>% select(proph_forecast), 
                                   which = "SE")

thirty_step_losses <- data.frame(thirty_step_arima_losses, thirty_step_proph_losses)

thirty_step_losses <- thirty_step_losses %>% 
  select(ARIMA = arima_forecast, Prophet = proph_forecast)

# Thirty-step MCS
thirty_step_MCS<- MCSprocedure(Loss = thirty_step_losses, alpha = 0.10, B = 5000, statistic = "TR")

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
  arima_9 <- Arima(na.omit(SixMonth_InSam_arima), order = c(1,0,0))  
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
  labs(title  = paste("90-step ahead forecasts from ARIMA(1,0,0)"))

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

# Ninety-step Model Confidence Set

# Ninety-step squared error losses
ninety_step_arima_losses <- LossLevel(realized = Ninety_Step_Results %>% select(actual),
                                      evaluated = Ninety_Step_Results %>% select(arima_forecast), 
                                      which = "SE")

ninety_step_proph_losses <- LossLevel(realized = Ninety_Step_Results %>% select(actual),
                                      evaluated = Ninety_Step_Results %>% select(proph_forecast), 
                                      which = "SE")

ninety_step_losses <- data.frame(ninety_step_arima_losses, ninety_step_proph_losses)

ninety_step_losses <- ninety_step_losses %>% 
  select(ARIMA = arima_forecast, Prophet = proph_forecast)

# Ninety-step MCS
ninety_step_MCS <- MCSprocedure(Loss = ninety_step_losses, alpha = 0.10, B = 5000, statistic = "TR")


# ------ Mincer Zarnowitz test and error statistics for ARIMA model

library(broom)
library(car)

# Mincer-Zarnowitz test for 1-step ahead forecast:

# Regression of actual and forecasted observations
one_step_arima_mincer <- One_Step_Results %>%  
  lm(actual ~ arima_forecast, data = .)

summary(one_step_arima_mincer)

one_step_arima_mincercoeff <- coef(one_step_arima_mincer)
# intercept = 0.009545697: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -2.005623768: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
one_step_arima_hyp <- linearHypothesis(one_step_arima_mincer, c("(Intercept) = 0", "arima_forecast = 1")) 
one_step_arima_mincerpval <- tidy(one_step_arima_hyp) %>% select(p.value)
# Small p-value of 0.02544883
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

# Finding RMSE, MSE, MAE and MAPE of 1 step-ahead forecasts:
one_step_arima_accuracy <- tidy(accuracy(One_Step_Results[,3], One_Step_Results[,2]))
one_step_arima_rmse <- one_step_arima_accuracy %>% select(RMSE)
# RMSE = 0.04406288

one_step_arima_mse <- (one_step_arima_rmse %>% select(MSE = RMSE))^2
# MSE = 0.001941537

one_step_arima_mae<- one_step_arima_accuracy %>% select(MAE)
# MAE = 0.03058991

one_step_arima_mape<- one_step_arima_accuracy %>% select(MAPE)
# MAPE = 142.3605

# Mincer-Zarnowitz test for 30-step ahead forecasts:

# Regression of actual and forecasted observations
thirty_step_arima_mincer <- Thirty_Step_Results %>%  
  lm(actual ~ arima_forecast, data = .)

summary(thirty_step_arima_mincer)

thirty_step_arima_mincercoeff <- coef(thirty_step_arima_mincer)
# intercept = 0.03132042: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -12.73675651: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
thirty_step_arima_hyp <- linearHypothesis(thirty_step_arima_mincer, c("(Intercept) = 0", "arima_forecast = 1")) 
thirty_step_arima_mincerpval <- tidy(thirty_step_arima_hyp) %>% select(p.value)
# Small p-value of 0.06135861
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

# Finding MSE, MAPE, RMSE and MAE of 30-steps ahead forecasts:
thirty_step_arima_accuracy <- tidy(accuracy(Thirty_Step_Results[,3], Thirty_Step_Results[,2]))
thirty_step_arima_rmse <- thirty_step_arima_accuracy %>% select(RMSE)
# RMSE = 0.04459219

thirty_step_arima_mse <- (thirty_step_arima_rmse %>% select(MSE = RMSE))^2
# MSE = 0.001988463

thirty_step_arima_mae <- thirty_step_arima_accuracy %>% select(MAE)
# MAE = 0.03205147

thirty_step_arima_mape <- thirty_step_arima_accuracy %>% select(MAPE)
# MAPE = 175.7479

# Mincer-Zarnowitz test for 90-step ahead forecasts:

# Regression of actual and forecasted observations
ninety_step_arima_mincer <- Ninety_Step_Results %>%  
  lm(actual ~ arima_forecast, data = .)

summary(ninety_step_arima_mincer)

ninety_step_arima_mincercoeff <- coef(ninety_step_arima_mincer)
# intercept = 0.006012467: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -0.938687158: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
ninety_step_arima_hyp <- linearHypothesis(ninety_step_arima_mincer, c("(Intercept) = 0", "arima_forecast = 1")) 
ninety_step_arima_mincerpval <- tidy(ninety_step_arima_hyp) %>% select(p.value)
# Large p-value of 0.8430658.
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

# Finding MSE, RMSE, MAPE and MAE of 90-steps ahead forecasts:
ninety_step_arima_accuracy <- tidy(accuracy(Ninety_Step_Results[,3], Ninety_Step_Results[,2]))
ninety_step_arima_rmse <- ninety_step_arima_accuracy %>% select(RMSE)
# RMSE = 0.04230991

ninety_step_arima_mse <- (ninety_step_arima_rmse %>% select(MSE = RMSE))^2
# MSE = 0.001790129

ninety_step_arima_mae <- ninety_step_arima_accuracy %>% select(MAE)
# MAE = 0.03200695

ninety_step_arima_mape <- ninety_step_arima_accuracy %>% select(MAPE)
# MAPE = 169.4606

# ------ Mincer Zarnowitz test and error statistics for Prophet

# Mincer-Zarnowitz test for 1-step forecast:

# Regression of actual and forecasted observations
one_step_proph_mincer <- One_Step_Results %>%  
  lm(actual ~ proph_forecast, data = .)

summary(one_step_proph_mincer)

one_step_proph_mincercoeff <- coef(one_step_proph_mincer)
# intercept = 0.0137019: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -1.7097443: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
one_step_proph_hyp <- linearHypothesis(one_step_proph_mincer, c("(Intercept) = 0", "proph_forecast = 1")) 
one_step_proph_mincerpval <- tidy(one_step_proph_hyp) %>% select(p.value)
# Small p-value of 0.007152494
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

# Finding MSE, RMSE, MAPE and MAE of 1 step-ahead forecasts:
one_step_proph_accuracy <- tidy(accuracy(One_Step_Results[,6], One_Step_Results[,2]))
one_step_proph_rmse <- one_step_proph_accuracy %>% select(RMSE)
# RMSE = 0.04425043

one_step_proph_mse <- (one_step_proph_rmse %>% select(MSE = RMSE))^2
# MSE = 0.001958101

one_step_proph_mae <- one_step_proph_accuracy %>% select(MAE)
# MAE = 0.03049741

one_step_proph_mape <- one_step_proph_accuracy %>% select(MAPE)
# MAPE = 173.3229

# Mincer-Zarnowitz test for 30-step forecast:

# Regression of actual and forecasted observations
thirty_step_proph_mincer <- Thirty_Step_Results %>%  
  lm(actual ~ proph_forecast, data = .)

summary(thirty_step_proph_mincer)

thirty_step_proph_mincercoeff <- coef(thirty_step_proph_mincer)
# intercept = 0.01716876: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -2.62191428: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
thirty_step_proph_hyp <- linearHypothesis(thirty_step_proph_mincer, c("(Intercept) = 0", "proph_forecast = 1")) 
thirty_step_proph_mincerpval <- tidy(thirty_step_proph_hyp) %>% select(p.value)
# Small p-value of 0.0003772506
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

# Finding MSE, RMSE, MAE, MAPE of 30-step ahead forecasts:
thirty_step_proph_accuracy <- tidy(accuracy(Thirty_Step_Results[,6], Thirty_Step_Results[,2]))
thirty_step_proph_rmse <- thirty_step_proph_accuracy %>% select(RMSE)
# RMSE = 0.04555755

thirty_step_proph_mse <- (thirty_step_proph_rmse %>% select(MSE = RMSE))^2
# MSE = 0.002075491

thirty_step_proph_mae <- thirty_step_proph_accuracy %>% select(MAE)
# MAE = 0.03236893

thirty_step_proph_mape <- thirty_step_proph_accuracy %>% select(MAPE)
# MAPE = 237.2627

# Mincer-Zarnowitz test for 90-step forecast:

# Regression of actual and forecasted observations
ninety_step_proph_mincer <- Ninety_Step_Results %>%  
  lm(actual ~ proph_forecast, data = .)

summary(ninety_step_proph_mincer)

ninety_step_proph_mincercoeff <- coef(ninety_step_proph_mincer)
# intercept = 0.006953398: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -0.615434821: far from 1
# forecasts are inefficient 

# Joint hypothesis to test int = 0 and slope = 1
ninety_step_proph_hyp <- linearHypothesis(ninety_step_proph_mincer, c("(Intercept) = 0", "proph_forecast = 1")) 
ninety_step_proph_mincerpval <- tidy(ninety_step_proph_hyp) %>% select(p.value)
# p-value = 0.4086752
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

# Finding MSE, RMSE, MAPE and MAE of 90-step ahead forecasts:
ninety_step_proph_accuracy <- tidy(accuracy(Ninety_Step_Results[,6], Ninety_Step_Results[,2]))
ninety_step_proph_rmse <- ninety_step_proph_accuracy %>% select(RMSE)
# RMSE = 0.0425902

ninety_step_proph_mse <- (ninety_step_proph_rmse %>% select(MSE = RMSE))^2
# MSE = 0.001813925

ninety_step_proph_mae <- ninety_step_proph_accuracy %>% select(MAE)
# MAE = 0.03176367

ninety_step_proph_mape <- ninety_step_proph_accuracy %>% select(MAPE)
# MAPE = 166.8346

# -------- Comparing Prophet and Arima results over the different forecast horizons

library(knitr)

# Prophet results for 1-step ahead 
one_step_proph_mincercoef <- data.frame(one_step_proph_mincercoeff) %>% 
  select(Prophet = one_step_proph_mincercoeff)

one_step_proph_mincerp <- na.omit(one_step_proph_mincerpval %>% 
                                      select(Prophet = p.value))

one_step_proph_mse <- data.frame(one_step_proph_mse) %>% select(Prophet = MSE)

one_step_proph_mincer <- rbind(one_step_proph_mincercoef, 
                               one_step_proph_mincerp, 
                               one_step_proph_mse) 

# ARIMA results for 1-step ahead
one_step_arima_mincercoef <- data.frame(one_step_arima_mincercoeff) %>% 
  select(ARIMA = one_step_arima_mincercoeff)

one_step_arima_mincerp <- na.omit(one_step_arima_mincerpval %>% 
                                      select(ARIMA = p.value))

one_step_arima_mse <- data.frame(one_step_arima_mse) %>% select(ARIMA = MSE)

one_step_arima_mincer <- rbind(one_step_arima_mincercoef, 
                               one_step_arima_mincerp, 
                               one_step_arima_mse)

# Table of 1-step ahead results
mincer_table1 <- t(data.frame(one_step_arima_mincer, 
                              one_step_proph_mincer, 
                              row.names = c("intercept", "slope", "p-value", "MSE")))
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

thirty_step_proph_mincer <- rbind(thirty_step_proph_mincercoef, 
                                  thirty_step_proph_mincerp, 
                                  thirty_step_proph_mse) 

# ARIMA results for 30-steps ahead
thirty_step_arima_mincercoef <- data.frame(thirty_step_arima_mincercoeff) %>% 
  select(ARIMA = thirty_step_arima_mincercoeff)

thirty_step_arima_mincerp <- na.omit(thirty_step_arima_mincerpval %>% 
                                       select(ARIMA = p.value))

thirty_step_arima_mse <- data.frame(thirty_step_arima_mse) %>% select(ARIMA = MSE)

thirty_step_arima_mincer <- rbind(thirty_step_arima_mincercoef, 
                                  thirty_step_arima_mincerp, 
                                  thirty_step_arima_mse)

# Table of 30-step ahead results
mincer_table2 <- t(data.frame(thirty_step_arima_mincer, 
                              thirty_step_proph_mincer, 
                              row.names = c("intercept", "slope", "p-value", "MSE")))
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

ninety_step_proph_mincer <- rbind(ninety_step_proph_mincercoef, 
                                  ninety_step_proph_mincerp, 
                                  ninety_step_proph_mse) 

# ARIMA results for 90-steps ahead
ninety_step_arima_mincercoef <- data.frame(ninety_step_arima_mincercoeff) %>% 
  select(ARIMA = ninety_step_arima_mincercoeff)

ninety_step_arima_mincerp <- na.omit(ninety_step_arima_mincerpval %>% 
                                       select(ARIMA = p.value))

ninety_step_arima_mse <- data.frame(ninety_step_arima_mse) %>% select(ARIMA = MSE)

ninety_step_arima_mincer <- rbind(ninety_step_arima_mincercoef, 
                                  ninety_step_arima_mincerp, 
                                  ninety_step_arima_mse)

# Table of 90-step ahead results
mincer_table3 <- t(data.frame(ninety_step_arima_mincer, 
                              ninety_step_proph_mincer, 
                              row.names = c("intercept", "slope", "p-value", "MSE")))
kable(mincer_table3)
# Arima forecasts are less biased
# Prophet forecasts are more efficient
# Both arima and prophet forecasts are efficient and unbiased
# 90-step forecasts are better for arima than prophet
# MSE of arima smaller than prophet - agrees with MZ test

# Table of Mincer-Zarnowitz results over all forecast horizons
table_pvalues <- cbind(data.frame(mincer_table1) %>% select(one_step = p.value),
      data.frame(mincer_table2) %>% select(thirty_steps = p.value),
      data.frame(mincer_table3) %>% select(ninety_steps = p.value)) 

kable(table_pvalues)
# p-value for arima gets larger as the forecast horizon gets longer
# p-value for prophet also gets larger as the forecast horizon gets longer
# p-value for arima larger than prophet over all forecast horizons

# Table of MSE's over all forecast horizons
table_MSE <- cbind(data.frame(mincer_table1) %>% select(one_step = MSE),
                       data.frame(mincer_table2) %>% select(thirty_steps = MSE),
                       data.frame(mincer_table3) %>% select(ninety_steps = MSE)) 

kable(table_MSE)
# MSE for ARIMA is lower than Prophet over all forecast horizons - agrees with Mincer-Zarnowitz results 

#------ HOW ROBUST IS PROPHET WHEN THERE ARE MISSING VALUES AND OUTLIERS COMPARED TO ARIMA? -----

source("~/R/UCT_hons_proj/Draft/Code/missing values and outliers.R")
missing_eval(ExchangeRates)

# --------- Is THERE AN OPPORTUNITY IN FORECASTING BITCOIN? A LOOK AT THE EMH ---------

# 1-step ahead trading profit with prophet
one_step_proph_profit <- sum(with(One_Step_Results, (proph_forecast>0)*actual + (proph_forecast<0)*-actual))
one_step_proph_profit
# profit = 0.1894012

# 1-step ahead trading profit with arima
one_step_arima_profit <- sum(with(One_Step_Results, (arima_forecast>0)*actual + (arima_forecast<0)*-actual))
one_step_arima_profit
# profit = 0.4903809

# Profit from random trade signals
num_simulations <- 10000

random_return <- numeric(length = num_simulations)

for(i in seq(num_simulations)) {
  random_vector <- rnorm(nrow(One_Step_Results))
  
  random_return[i] <- sum(with(One_Step_Results, (random_vector>0)*actual +(random_vector<0)*-actual))
}

# Comparison of 1-step ahead profit
comp_1_step_profit <- data.frame(random_return, one_step_proph_profit, one_step_arima_profit)

plot_1_step_profit <- ggplot(comp_1_step_profit, aes(random_return)) +
  geom_histogram() +
  geom_vline(aes(xintercept = one_step_proph_profit, colour = "prophet profit"), size = 1) +
  geom_vline(aes(xintercept = one_step_arima_profit, colour = "arima profit"), size = 1) +
  scale_color_manual("", breaks = c("prophet profit", "arima profit"), values = c("prophet profit" = "blue", "arima profit" = "purple")) +
  labs(title = paste("Comparison of Profit from Random Trade Signals")) +
  xlab("returns from random trade signals") +
  ylab("no. of simulations")

plot_1_step_profit

# 30-step ahead trading profit with prophet
thirty_step_proph_profit <- sum(with(Thirty_Step_Results, (proph_forecast>0)*actual + (proph_forecast<0)*-actual))
thirty_step_proph_profit
# profit = 0.2005354

# 30-step ahead trading profit with arima
thirty_step_arima_profit <- sum(with(Thirty_Step_Results, (arima_forecast>0)*actual + (arima_forecast<0)*-actual))
thirty_step_arima_profit
# profit = 0.574857

# Profit from random trade signals
num_simulations <- 10000

random_return <- numeric(length = num_simulations)

for(i in seq(num_simulations)) {
  random_vector <- rnorm(nrow(Thirty_Step_Results))
  
  random_return[i] <- sum(with(Thirty_Step_Results, (random_vector>0)*actual +(random_vector<0)*-actual))
}

# Comparison of 30-step ahead profit
comp_30_step_profit <- data.frame(random_return, thirty_step_proph_profit, thirty_step_arima_profit)

plot_30_step_profit <- ggplot(comp_30_step_profit, aes(random_return)) +
  geom_histogram() +
  geom_vline(aes(xintercept = thirty_step_proph_profit, colour = "prophet profit"), size = 1) +
  geom_vline(aes(xintercept = thirty_step_arima_profit, colour = "arima profit"), size = 1) +
  scale_color_manual("", breaks = c("prophet profit", "arima profit"), values = c("prophet profit" = "blue", "arima profit" = "purple")) +
  labs(title = paste("Comparison of Profit from Random Trade Signals")) +
  xlab("returns from random trade signals") +
  ylab("no. of simulations")

plot_30_step_profit

# 90-step ahead trading profit with prophet
ninety_step_proph_profit <- sum(with(Ninety_Step_Results, (proph_forecast>0)*actual + (proph_forecast<0)*-actual))
ninety_step_proph_profit
# profit = 0.2453142

# 90-step ahead trading profit with arima
ninety_step_arima_profit <- sum(with(Ninety_Step_Results, (arima_forecast>0)*actual + (arima_forecast<0)*-actual))
ninety_step_arima_profit
# profit = 0.3997449

# Profit from random trade signals
num_simulations <- 10000

random_return <- numeric(length = num_simulations)

for(i in seq(num_simulations)) {
  random_vector <- rnorm(nrow(Ninety_Step_Results))
  
  random_return[i] <- sum(with(Ninety_Step_Results, (random_vector>0)*actual +(random_vector<0)*-actual))
}

# Comparison of 90-step ahead profit
comp_90_step_profit <- data.frame(random_return, ninety_step_proph_profit, ninety_step_arima_profit)

plot_90_step_profit <- ggplot(comp_90_step_profit, aes(random_return)) +
  geom_histogram() +
  geom_vline(aes(xintercept = ninety_step_proph_profit, colour = "prophet profit"), size = 1) +
  geom_vline(aes(xintercept = ninety_step_arima_profit, colour = "arima profit"), size = 1) +
  scale_color_manual("", breaks = c("prophet profit", "arima profit"), values = c("prophet profit" = "blue", "arima profit" = "purple")) +
  labs(title = paste("Comparison of Profit from Random Trade Signals")) +
  xlab("returns from random trade signals") +
  ylab("no. of simulations")

plot_90_step_profit

# Trading profit to be made with ARIMA is more than with Prophet over all forecast horizons
# Trading profit for Prophet improves with longer forecast horizons
# Trading profit with ARIMA is better at shorter forecast horizons


