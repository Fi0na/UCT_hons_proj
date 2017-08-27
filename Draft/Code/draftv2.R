
# -------------------------------READING IN DATASET------------------------------------------------

# Loading required packages
library(rmsfuns)
packagestoload <- c("stats", "xts", "dplyr", "Dplyr2Xts", "PerformanceAnalytics", "ggplot2", "readr", "gridExtra", "lmtest")
load_pkg(packagelist = packagestoload) 

setwd("~/R/UCT_hons_proj/Draft/Data")

# Set relative path 
ExchangeRates <- read_csv2("prices.csv", col_names = TRUE)

# Looking at first few rows of dataset
ExchangeRates %>% 
  data.frame %>% 
  head

# Looking at structure of dataset
dim(ExchangeRates)
str(ExchangeRates)

#--------------------------------- CLEANING THE DATASET--------------------------------------------

# Order dates in ascending order
ExchangeRates <- ExchangeRates %>% 
  arrange(TradeDate)

# Make date column time-based
ExchangeRates$TradeDate <- as.Date(ExchangeRates$TradeDate)

# Checking if there are any missing values
na_true <- is.na.data.frame(ExchangeRates)
which(na_true)

# --------------------------------EXPLORATORY DATA ANALYSIS---------------------------------------

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
# no unusual observations
# there's a certain degree of variability and an upward trend

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
# Range of daily log returns is roughly [-0.05 ; 0.5] throughout the sample span
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

# ----------------------SPITTING DATASET INTO IN-SAMPLE AND OUT-OF-SAMPLE-------------------

# Subsetting for forecast horizon of one month:

# Test Set
OneMonth_OutOfSample <- ExchangeRates %>%
  dplyr::filter(TradeDate >= "2017-06-15")

# Training Set
OneMonth_InSample <- ExchangeRates %>%
  dplyr::filter(TradeDate < "2017-06-15")

# Plots of training and test sets closing prices
plot_1MonthTestPrices <- plot_prices %+% 
  OneMonth_OutOfSample + ggtitle("1 Month Test Set Closing Prices")

plot_1MonthTrainPrices <- plot_prices %+% 
  OneMonth_InSample + ggtitle("1 Month Training Set Closing Prices") +
  theme_minimal()

grid.arrange(plot_1MonthTrainPrices,
             plot_1MonthTestPrices,
             nrow = 2)


# Plots of training and test sets daily log returns
plot_1MonthTestReturns <- plot_returns %+% OneMonth_OutOfSample + ggtitle("1 Month Test Set Daily Log Returns")
plot_1MonthTrainReturns <- plot_returns %+% OneMonth_InSample + ggtitle("1 Month Training Set Daily Log Returns") + theme_minimal()

grid.arrange(plot_1MonthTrainReturns,
             plot_1MonthTestReturns,
             nrow = 2)

# Subsetting for forecast horizon of three months:

# Test Set
ThreeMonth_OutOfSample <- ExchangeRates %>% 
  dplyr::filter(TradeDate >= "2017-04-15")

# Training Set
ThreeMonth_InSample <- ExchangeRates %>% 
  dplyr::filter(TradeDate < "2017-04-15")

# Plots of training and test sets closing prices
plot_3MonthTestPrices <- plot_prices %+% 
  ThreeMonth_OutOfSample + 
  ggtitle("3 Month Test Set Closing Prices")

plot_3MonthTrainPrices <- plot_prices %+% 
  ThreeMonth_InSample + 
  ggtitle("3 Month Training Set Closing Prices") + 
  theme_minimal()

grid.arrange(plot_3MonthTrainPrices,
             plot_3MonthTestPrices,
             nrow = 2)

# Plots of training and test sets daily log returns
plot_3MonthTestReturns <- plot_returns %+% 
  ThreeMonth_OutOfSample + 
  ggtitle("3 Month Test Set Daily Log Returns")

plot_3MonthTrainReturns <- plot_returns %+% 
  ThreeMonth_InSample + 
  ggtitle("3 Month Training Set Daily Log Returns") + 
  theme_minimal()

grid.arrange(plot_3MonthTrainReturns,
             plot_3MonthTestReturns,
             nrow = 2)

# ---HOW DO THE FORECASTS FROM ARIMA & PROPHET COMPARE OVER DIFFERENT FORECAST HORIZONS-----

library(forecast)
library(tseries)

# ------- Box-Jenkins methodology for selection of arima

# 1) Series made stationary by finding log returns

# Formal test for stationarity: ADF unit root test
adf.test(na.omit(daily_logret[[1]]), alternative = "stationary")
# small p value - reject null of non-stationarity

# Formal test for degree of differencing required
ndiffs(daily_logret[[1]])
# d=0

# 2) Identify order of p and q

# ACF Plot - identify order of MA
Acf(daily_logret, lag = 12, main = "ACF Plot of Daily Log Returns")
# None significantly different from 0
# Serial autocorrelation of daily log returns are small
# Acf indicates white noise

# Formal test for serial correlation: Portmanteu's test
Box.test(daily_logret, lag = 12, type = "Ljung")
# Large p - value: fail to reject the null hypothesis of no serial correlations in log returns

# PACF Plot - idetify order of AR
Pacf(daily_logret, lag = 12, main = "PACF Plot of Daily Log Returns")
# None significantly different from zero

# arima(0,0,0) chosen based on acf and pacf plot

# 3) Estimation of model parameters

# Fitting arima model for 1 month forecast horizon:

OneMonth_InSampleRet <- OneMonth_InSample %>% 
  select(DailyLogReturns)

OneMonth_OutOfSampleRet <- OneMonth_OutOfSample %>% 
  select(DailyLogReturns)

# One step ahead forecasts for 1 month out of sample without reestimation
OneMonth_OutOfSample_Dates <- seq.Date(as.Date("2017-06-15"), 
                                       by = "day" ,
                                       length.out = nrow(OneMonth_OutOfSampleRet))

OneMonth_NoRest_Results <- list()

for(i in 1:length(OneMonth_OutOfSample_Dates)){
  OneMonth_InSam_NoRest <- ExchangeRates %>%
    dplyr::filter(TradeDate < OneMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  OneMonth_OutSam_NoRest <- ExchangeRates %>%
    dplyr::filter(TradeDate == OneMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  arima_9 <- Arima(na.omit(OneMonth_InSam_NoRest), order = c(0,0,0))  
  OneMonth_ArimaPred_NoRest <- forecast(arima_9, h = 1)
  
  plot(OneMonth_ArimaPred_NoRest)
  
  OneMonth_NoRest_Results[[i]] <- data.frame(date = OneMonth_OutOfSample_Dates[i],
                                             actual = as.numeric(OneMonth_OutSam_NoRest),
                                             forecast = as.numeric(OneMonth_ArimaPred_NoRest$mean)) %>%
    mutate(errors = actual - forecast)
}

OneMonth_NoRest_Results <- do.call(rbind, OneMonth_NoRest_Results)

# Fitting arima model for 3 month forecast horizon:
ThreeMonth_InSampleRet <- ThreeMonth_InSample %>% 
  select(DailyLogReturns)

ThreeMonth_OutOfSampleRet <- ThreeMonth_OutOfSample %>%
  select(DailyLogReturns)

# One step ahead forecasts for 3 month out of sample without reestimation
ThreeMonth_OutOfSample_Dates <- seq.Date(as.Date("2017-04-15"), 
                                         by = "day" ,
                                         length.out = nrow(ThreeMonth_OutOfSampleRet))

ThreeMonth_NoRest_Results <- list()

for(i in 1:length(ThreeMonth_OutOfSample_Dates)){
  ThreeMonth_InSam_NoRest <- ExchangeRates %>%
    dplyr::filter(TradeDate < ThreeMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  ThreeMonth_OutSam_NoRest <- ExchangeRates %>%
    dplyr::filter(TradeDate == ThreeMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  arima_10 <- Arima(na.omit(ThreeMonth_InSam_NoRest), order = c(0,0,0))  
  ThreeMonth_ArimaPred_NoRest <- forecast(arima_10, h = 1)
  
  plot(ThreeMonth_ArimaPred_NoRest)
  
  ThreeMonth_NoRest_Results[[i]] <- data.frame(date = ThreeMonth_OutOfSample_Dates[i],
                                               actual = as.numeric(ThreeMonth_OutSam_NoRest),
                                               forecast = as.numeric(ThreeMonth_ArimaPred_NoRest$mean)) %>%
    mutate(errors = actual - forecast)
}

ThreeMonth_NoRest_Results <- do.call(rbind, ThreeMonth_NoRest_Results)

# 4) Diagnostic check

# Diagnostic of arima_9
checkresiduals(arima_9)
# No serial correlation 

# Diagnostic of arima_10
checkresiduals(arima_10)
# No serial correlation

# ------ Evaluating forecast accuracy of arima models
library(broom)

# Mincer-Zarnowits test for 1 month forecast:

# Regression of actual and forecasted observations
OneMonth_mincer <- OneMonth_NoRest_Results %>%  
  lm(actual ~ forecast, data = .)

summary(OneMonth_mincer)

OneMonth_arima_mincercoeff <- tidy(summary(OneMonth_mincer)) %>% 
  select(estimate)
# intercept = 0.04311: positive bias estimated 
# forecast systematically underrestimates the actual observation
# slope = -11.88458 - super far from 1

# Joint hypothesis to test int=0 and slope=1
library(car)
OneMonth_hyp <- linearHypothesis(OneMonth_mincer, c("(Intercept) = 0", "forecast = 1")) 
OneMonth_arima_mincerpval <- tidy(OneMonth_hyp) %>% select(p.value)
# Large p-value
# Fail to reject the null hypothesis of int=0 and slope=1

# Finding MSE of 1 month forecasts:

# Squared errors for 1 month
OneMonth_errors <- OneMonth_NoRest_Results %>% select(errors) 
OneMonth_squared_errors = OneMonth_errors^2

# 1 month MSE
OneMonth_mse <- mean(OneMonth_squared_errors)
# 0.001097266

# Mincer-Zarnowits test for 3 month forecast:

# Regression of actual and forecasted observations
ThreeMonth_mincer <- ThreeMonth_NoRest_Results %>%  
  lm(actual ~ forecast, data = .)

summary(ThreeMonth_mincer)

ThreeMonth_arima_mincercoeff <- tidy(summary(ThreeMonth_mincer)) %>% 
  select(estimate)
# intercept = 0,04078528: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -9.74766964 - very far from 1

# Joint hypothesis to test int=0 and slope=1
ThreeMonth_hyp <- linearHypothesis(ThreeMonth_mincer, c("(Intercept) = 0", "forecast = 1")) 
ThreeMonth_arima_mincerpval <- tidy(ThreeMonth_hyp) %>% select(p.value)

# Finding MSE of 3 month forecasts:

# Squared errors for 3 months
ThreeMonth_errors <- ThreeMonth_NoRest_Results %>% select(errors) 
ThreeMonth_squared_errors = ThreeMonth_errors^2

# 3 month MSE
ThreeMonth_mse <- mean(ThreeMonth_squared_errors)
# 0.001547404

# MSE 3 months > MSE 1 month forecasts
# ARIMA produces better forecasts over shorter forecast horizon

# ------ Forecasting with Prophet

library(prophet)

# Put data into format that prophet likes
OneMonth_InSam_Proph <- OneMonth_InSample %>% select(ds = TradeDate, y = DailyLogReturns)
ThreeMonth_InSam_Proph <- ThreeMonth_InSample %>% select(ds = TradeDate, y = DailyLogReturns)

# Choosing Prophet model:

# Fitting model for 1 month
prophet_1 <- prophet(df = OneMonth_InSam_Proph, 
                     growth = "linear", 
                     weekly.seasonality = TRUE, 
                     yearly.seasonality = FALSE)
# No max of returns so setting trend to be linear

# Include weekly seasonality to account for little trading activity from Friday to Sunday and high activity around Tuesday and Wednesday 
# Don't really know about any yearly seasonal patterns in trading Bitcoin so not including it

# Don't yet know the dates of events that might impact the price but once known would be useful to include
# Some events that could impact in 2017: Segwit, etfs
# Some events that could impact later on: Brexit, 2020 reward halving
# Events that have already impacted price but dates were only know in retrospect: Japan legalising, China banning, hacking

# All other default values used

# Fitting model for 3 months
prophet_2 <- prophet(df = ThreeMonth_InSam_Proph, 
                     growth = "linear", 
                     weekly.seasonality = TRUE, 
                     yearly.seasonality = FALSE)

# Creating dataframe for forecasting
OneMonth_future1 <- make_future_dataframe(prophet_1, periods = nrow(OneMonth_OutOfSample))
tail(OneMonth_future1)

ThreeMonth_future1 <- make_future_dataframe(prophet_2, periods = nrow(ThreeMonth_OutOfSample))
tail(ThreeMonth_future1)

# Forecasting for 1 month 
OneMonth_proph1_pred <- predict(prophet_1, OneMonth_future1)
tail(OneMonth_proph1_pred[c("ds", "yhat", "yhat_lower", "yhat_upper")])

# Forecasting for 3 months 
ThreeMonth_proph2_pred <- predict(prophet_2, ThreeMonth_future1)
tail(ThreeMonth_proph2_pred[c("ds", "yhat", "yhat_lower", "yhat_upper")])

# Looking at plots
plot(prophet_1, OneMonth_proph1_pred)
# Doesn't follow data very well
# Wide uncertainty intervals

plot(prophet_2, ThreeMonth_proph2_pred)
# Doesn't follow data well again 
# Narrower uncertainty interval than 1 month prophet forecasts

prophet_plot_components(prophet_1, OneMonth_proph1_pred)
# Returns have been increasing over the past year
# Can see higher returns around Tues and lower returns towards Friday due to amount of trading activity

prophet_plot_components(prophet_2, ThreeMonth_proph2_pred)

# -------- Evaluating forecast accuracy of Prophet models

# Mincer-Zarnowits test for 1 month forecast:

# one month point forecasts
OneMonth_proph_yhat <- OneMonth_proph1_pred %>% dplyr::filter(ds >= as.Date("2017-06-15")) %>%
  select(ds, yhat)

# Dataframe of one month results
OneMonth_proph_results <- data.frame(OneMonth_proph_yhat, OneMonth_OutOfSampleRet)

# Regression of actual and forecasted observations
OneMonth_proph_mincer <- lm(DailyLogReturns ~ yhat, data = OneMonth_proph_results)
summary(OneMonth_proph_mincer)
OneMonth_proph_mincercoeff <- tidy(summary(OneMonth_proph_mincer)) %>% select(estimate)
# intercept = -0.2234: negative bias estimated 
# forecast systematically overestimates the actual observation
# slope = 2.39114

# Joint hypothesis to test null: int = 0 and slope = 1
OneMonth_proph_hyp <- linearHypothesis(OneMonth_proph_mincer, c("(Intercept) = 0", "yhat = 1")) 
OneMonth_proph_mincerpval <- tidy(OneMonth_proph_hyp) %>% select(p.value)
# p = 0.1368
# Fail to reject null hypothesis 

# Finding MSE of 1 month forecasts:

# Gives accuracy statistics for 1 month
OneMonth_proph_accuracy <- tbl_df(accuracy(OneMonth_proph_results[,2], OneMonth_proph_results[,3]))

# Get the RMSE
OneMonth_proph_rmse <- OneMonth_proph_accuracy %>% select(RMSE) 

# Calculate the MSE
OneMonth_proph_mse <- (OneMonth_proph_rmse)^2
# MSE = 0.001110611

# Mincer-Zarnowits test for 3 month forecast:

# 3 month point forecasts
ThreeMonth_proph_yhat <- ThreeMonth_proph2_pred %>% dplyr::filter(ds >= as.Date("2017-04-15")) %>%
  select(ds, yhat)

# Dataframe of 3 month results
ThreeMonth_proph_results <- data.frame(ThreeMonth_proph_yhat, ThreeMonth_OutOfSampleRet)

# Regression of actual and forecasted observations
ThreeMonth_proph_mincer <- lm(DailyLogReturns ~ yhat, data = ThreeMonth_proph_results)
summary(ThreeMonth_proph_mincer)
ThreeMonth_proph_mincercoeff <- tidy(summary(ThreeMonth_proph_mincer)) %>% select(estimate)
# intercept = -0.0006977: negative bias estimated 
# slope = 1.7641593
# intercept closer to 0 and slope closer to 1 than 1 month forecast

# Joint hypothesis to test null: int = 0 and slope = 1
ThreeMonth_proph_hyp <- linearHypothesis(ThreeMonth_proph_mincer, c("(Intercept) = 0", "yhat = 1")) 
ThreeMonth_proph_mincerpval <- tidy(ThreeMonth_proph_hyp) %>% select(p.value)
# p = 0.6625 > 1 month p value
# Fail to reject null hypothesis 

# Finding MSE of 3 month forecasts:

# Gives accuracy statistics for 3 month
ThreeMonth_proph_accuracy <- tbl_df(accuracy(ThreeMonth_proph_results[,2], ThreeMonth_proph_results[,3]))

# Get the RMSE
ThreeMonth_proph_rmse <- ThreeMonth_proph_accuracy %>% select(RMSE) 

# Calculate the MSE
ThreeMonth_proph_mse <- (ThreeMonth_proph_rmse)^2
# MSE = 0.001503105 > MSE of 1 month prophet forecast

# Prophet produces better forecasts over the shorter forecast horizon

# -------- Comparing Prophet and Arima results over two time horizons

# Mincer-zarnowits results for one month - prophet
OneMonth_proph_mincercoef <- OneMonth_proph_mincercoeff %>% select(Prophet = estimate)
OneMonth_proph_mincerp <- na.omit(OneMonth_proph_mincerpval %>% select(Prophet = p.value))
OneMonth_proph_mincer <- rbind(OneMonth_proph_mincercoef, OneMonth_proph_mincerp) 

# Mincer-zarnowits results for one month - arima
OneMonth_arima_mincercoef <- OneMonth_arima_mincercoeff %>% select(ARIMA = estimate)
OneMonth_arima_mincerp <- na.omit(OneMonth_arima_mincerpval %>% select(ARIMA = p.value))
OneMonth_arima_mincer <- rbind(OneMonth_arima_mincercoef, OneMonth_arima_mincerp)

# Table of 1 month Mincer-Zarnowits results
mincer_table1 <- data.frame(OneMonth_arima_mincer, OneMonth_proph_mincer, row.names = c("intercept", "slope", "p value"))

# Mincer-zarnowits results for 3 months - prophet
ThreeMonth_proph_mincercoef <- ThreeMonth_proph_mincercoeff %>% select(Prophet = estimate)
ThreeMonth_proph_mincerp <- na.omit(ThreeMonth_proph_mincerpval %>% select(Prophet = p.value))
ThreeMonth_proph_mincer <- rbind(ThreeMonth_proph_mincercoef, ThreeMonth_proph_mincerp) 

# Mincer-zarnowits results for 3 months - arima
ThreeMonth_arima_mincercoef <- ThreeMonth_arima_mincercoeff %>% select(ARIMA = estimate)
ThreeMonth_arima_mincerp <- na.omit(ThreeMonth_arima_mincerpval %>% select(ARIMA = p.value))
ThreeMonth_arima_mincer <- rbind(ThreeMonth_arima_mincercoef, ThreeMonth_arima_mincerp)

# Table of 3 month Mincer-Zarnowits results
mincer_table2 <- data.frame(ThreeMonth_arima_mincer, ThreeMonth_proph_mincer, row.names = c("intercept", "slope", "p value"))

# Table of Mincer-zarnowits results for both forecast horizons
mincer_table3 <- data.frame(mincer_table1, mincer_table2)
# Prophet - intercept gets closer to 0, slope gets closer to 1, and p value increases for longer forecast horizon
# ARIMA - intercept gets closer to 0, slope gets closer to 1, and p value decreases for longer forecast horizon
# Prophet's slope is closer to 1 and intercept is closer to 0 for both forecast horizons

# Arima MSE's
arima_mse <- data.frame(OneMonth_mse, ThreeMonth_mse)

# Prophet MSE's
prophet_mse <- data.frame(OneMonth_proph_mse, ThreeMonth_proph_mse)

# Table of MSE's
mse_table <- matrix(data.frame(arima_mse, prophet_mse), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("ARIMA", "Prophet"), c("One Month MSE", "Three Month MSE")))
# MSE for Prophet is lower than ARIMA for longer forecast horizons
# MSE for Prophet lower for shorter forecast horizon
# MSE for ARIMA lower for shorter forecast horizon

# ------------HOW DO PROPHET'S AND ARIMA'S AUTOMATIC SELECTION PROCEDURE COMPARE----------

# Automatically selected arima model for 1 month forecast
OneMonth_AutoArima <- auto.arima(na.omit(OneMonth_InSampleRet), seasonal = FALSE) 
summary(OneMonth_AutoArima)
# arima(0,0,0) with non-zero mean fitted
# same model chosen by box-jenkins methodology 

# Automatically selected arima model for 3 month forecast
ThreeMonth_AutoArima <- auto.arima(na.omit(ThreeMonth_InSampleRet), seasonal = FALSE)
summary(ThreeMonth_AutoArima)
# arima(0,0,0) with zero mean fitted
# box-jenkins methodology included mean

# Automatic selection of Prophet model for 1 month
OneMonth_autoproph <- prophet(df = OneMonth_InSam_Proph)
# Model fitted with linear trend, excludes yearly seasonality and includes weekly seasonality
# Same model chosen manually

# Automatic selection of Prophet model for 3 months
ThreeMonth_autoproph <- prophet(df = ThreeMonth_InSam_Proph)
# # Model fitted with linear trend, excludes yearly seasonality and includes weekly seasonality
# Same model chosen manually

# The models automatically selected are the same as those chosen manually for both Prophet and ARIMA

#----------HOW ROBUST IS PROPHET WHEN THERE ARE MISSING VALUES COMPARED TO ARIMA?-----------------

# Prophet is said to be robust when there are missing values whereas arima requires Kalman smoothing and interpolation
# Want to test this out

# Missing values and Prophet 
OneMonth_naInSam_Proph <- OneMonth_InSample %>% select(ds = TradeDate, y = DailyLogReturns)
OneMonth_naInSam_Proph [c(4, 10, 17, 20, 35, 48, 50, 100, 110, 177, 140, 157, 200, 230, 245, 270, 286, 300, 323, 330),2] <- NA

ThreeMonth_naInSam_Proph <- ThreeMonth_InSample %>% select(ds = TradeDate, y = DailyLogReturns)
ThreeMonth_naInSam_Proph [c(4, 10, 17, 20, 35, 48, 50, 100, 110, 177, 140, 157, 200, 230, 245, 270),2] <- NA

# -------Is THERE AN OPPORTUNITY IN FORECASTING BITCOIN WITH PROPHET? A LOOK AT THE EMH----

# 3 month trading profit with prophet
ThreeMonth_prophprofit <- sum(with(ThreeMonth_proph_results, (yhat>0)*DailyLogReturns + (yhat<0)*-DailyLogReturns))
ThreeMonth_prophprofit
# profit = 0.660265

# Profit from random trade signals
num_simulations <- 10000

random_return <- numeric(length = num_simulations)

for(i in seq(num_simulations)) {
  random_vector <- rnorm(nrow(ThreeMonth_proph_results))
  
  random_return[i] <- sum(with(ThreeMonth_proph_results, (random_vector>0)*DailyLogReturns +(random_vector<0)*-DailyLogReturns))
}

# Comparison of profit
comp_profit <- data.frame(random_return, ThreeMonth_prophprofit)

plot_ThreeMonth_prophprofit <- ggplot(comp_profit, aes(random_return)) +
  geom_histogram() +
  geom_vline(aes(xintercept = ThreeMonth_prophprofit), size = 1) +
  labs(x = "Returns from random trade signals")

plot_ThreeMonth_prophprofit





