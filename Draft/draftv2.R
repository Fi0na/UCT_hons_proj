
# -------------------------------READING IN DATASET------------------------------------------------

# Loading required packages
library(rmsfuns)
packagestoload <- c("stats", "xts", "readr", "dplyr", "Dplyr2Xts", "PerformanceAnalytics", "ggplot2", "readr", "gridExtra", "lmtest")
load_pkg(packagelist = packagestoload) 

# Hanjo - add library readr

setwd("~/R/UCT_hons_proj/Draft/Data")

# Hanjo - Set relative path thanks to proj
ExchangeRates <- read_csv2("Draft/Data/prices.csv")

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

load_pkg("lubridate")

# Subsetting for forecast horizon of one month:
# Shortened notation
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

# 3) Estimation of model parameters

# Fitting arima models for 1 month forecast horizon:

OneMonth_InSampleRet <- OneMonth_InSample %>% select(DailyLogReturns)

arima_1 <- Arima(na.omit(OneMonth_InSampleRet), order = c(0,0,0))  
summary(arima_1)
# white noise
# AICC = -1355.96

arima_2 <- Arima(na.omit(OneMonth_InSampleRet), order = c(1,0,0))  
summary(arima_2)
# arima(1,0,0) 
# AICC = -1353.93 

arima_3 <- Arima(na.omit(OneMonth_InSampleRet), order = c(0,0,1))  
summary(arima_3)
# arima(0,0,1)
# AICC = -1353.93

arima_4 <- Arima(na.omit(OneMonth_InSampleRet), order = c(1,0,1))  
summary(arima_4)

# Hanjo - Although smallest AIC was mean-model, for the purpose of this paper we employ the ARIMA(1,0,1) as our benchmark so that statistical inference can be conducted. Analysis of the p-values indicate that the AR and MA parameters of the model are statistical relevant in the explanation of the movement of BTC/ZAR.
# As a further point of research, statistical techniques can be conducted to determine whether ARIMA(0, 0, 0) and ARIMA(1, 0, 1) differ. These do not get addressed in this paper as it is outside of the research scope. 
lmtest::coeftest(arima_4)

# arima(1,0,1)
# AICC = -1352.4

# Final arima model selected by lowest corrected AIC
# Final arima model - arima(0,0,0): arima_1

# Fitting arima models for 3 month forecast horizon:

ThreeMonth_InSampleRet <- ThreeMonth_InSample %>% select(DailyLogReturns)

arima_5 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(0,0,0))  
summary(arima_5)
# white noise
# AICC = -1158.86

arima_6 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(1,0,0))  
summary(arima_6)
# arima(1,0,0) 
# AICC = -1156.88

arima_7 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(0,0,1))  
summary(arima_7)
# arima(0,0,1)
# AICC = -1156.89 

arima_8 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(1,0,1))  
summary(arima_8)
# arima(0,0,1)
# AICC = -1352.4

# Final arima model selected by lowest corrected AIC
# Final arima model - arima(0,0,0): arima_5

# 4) Diagnostic check

# Diagnostic of arima_1
checkresiduals(arima_1)
# No serial correlation 

# Diagnostic of arima_5
checkresiduals(arima_5)
# No serial correlation

# Forecasting with ARIMA Model over 1 month and 3 months

OneMonth_OutOfSampleRet <- OneMonth_OutOfSample %>% 
  select(DailyLogReturns) %>% 
  unlist

ThreeMonth_OutOfSampleRet <- ThreeMonth_OutOfSample %>% 
  select(DailyLogReturns)

# One step ahead forecasts without re-estimation
# pred_arima1 <- Arima(OneMonth_OutOfSampleRet, model = arima_1)
# OneMonth_arima_pred <- fitted(pred_arima1)

# This is what I want to do but things weren't working out :(

# One step ahead forecasts for 1 month
OneMonth_arima_pred <- forecast(arima_1, h = nrow(OneMonth_OutOfSampleRet))
dataframe_OneMonth_arima_pred <- summary(OneMonth_arima_pred)
plot(OneMonth_arima_pred)
# Constant mean of 0.003906162 is forecasted

# Hanjo
fore_arima <- forecast(arima_4, h = 30)
plot(fore_arima)
accuracy(fore_arima, x = OneMonth_OutOfSampleRet)
# -------------------------------

# One step ahead forecasts for 3 months
ThreeMonth_arima_pred <- forecast(arima_5, h = nrow(ThreeMonth_OutOfSampleRet))
dataframe_ThreeMonth_arima_pred <- summary(ThreeMonth_arima_pred)
plot(ThreeMonth_arima_pred)
# Constant mean of 0.002072763 is forecasted
# -------------------------------

# Hanjo - you need to itteratively loop over the prediction period and test the difference time-horizons (1, 5, 20 days)

# Hanjo - t+1 forecasts 1 month out of sample
fore_sample <- seq.Date(as.Date("2017-06-15"), 
                          by = "day" ,
                          length.out = 30)

all_forecasts_error <- list()
for(i in 1:length(fore_sample)){
  in_sample <- ExchangeRates %>%
    dplyr::filter(TradeDate < fore_sample[i]) %>% 
    select(DailyLogReturns)
  
  
  out_sample <- ExchangeRates %>%
    dplyr::filter(TradeDate == fore_sample[i]) %>% 
    select(DailyLogReturns)
  
  arima_model <- Arima(in_sample, order = c(1,0,1))  
  fore_arima <- forecast(arima_model, h = 1)
  # plot(fore_arima)
  for_res <- accuracy(fore_arima, x = as.numeric(out_sample))
  all_forecasts_error[[i]] <- data.frame(actual = as.numeric(out_sample),
                                         forecast = as.numeric(fore_arima$mean),
                                         error = for_res[2,1])
}

all_forecasts_error <- do.call(rbind, all_forecasts_error)

# Hanjo - t+5 forecasts 1 month out of sample
# Obviously make your out of sample period a lot longer -+ 3 month
fore_sample <- seq.Date(as.Date("2017-06-15"), 
                        to = as.Date("2017-07-14"),
                        by = "+5 day")

all_forecasts_error <- list()
for(i in 1:(length(fore_sample) - 1)){
  in_sample <- ExchangeRates %>%
    dplyr::filter(TradeDate < fore_sample[i]) %>% 
    select(DailyLogReturns)
  
  
  out_sample <- ExchangeRates %>%
    dplyr::filter(TradeDate >= fore_sample[i], TradeDate < fore_sample[i+1]) %>% 
    select(DailyLogReturns)
  
  arima_model <- Arima(in_sample, order = c(1,0,1))  
  fore_arima <- forecast(arima_model, h = 5)
  # plot(fore_arima)
  for_res <- accuracy(fore_arima, x = unlist(out_sample))
  all_forecasts_error[[i]] <- data.frame(actual = unlist(out_sample),
                                         forecast = as.numeric(fore_arima$mean),
                                         error = unlist(out_sample)- as.numeric(fore_arima$mean))
}

all_forecasts_error_t5 <- do.call(rbind, all_forecasts_error)



# ------ Evaluating forecast accuracy of arima models
library(broom)

# Mincer-Zarnowits test for 1 month forecast:

# one month point forecasts
OneMonth_yhat <- dataframe_OneMonth_arima_pred %>% select(starts_with("Point"))

# One month trade dates
OneMonth_dates <- OneMonth_OutOfSample %>% select(TradeDate)

# Dataframe of one month results
OneMonth_results <- data.frame(OneMonth_dates, OneMonth_OutOfSampleRet, OneMonth_yhat)

# Regression of actual and forecasted observations
OneMonth_mincer <- all_forecasts_error %>%  
  lm(actual ~ forecast, data = .)
summary(OneMonth_mincer)
OneMonth_arima_mincercoeff <- tidy(summary(OneMonth_mincer))
# intercept = -0,002394: negative bias estimated 
# forecast systematically overestimates the actual observation
# Can't find estimate of slope :(
# Hanjo - Fixed it, now slope is included


# Joint hypothesis to test int=0 and slope=1
library(car)
OneMonth_hyp <- linearHypothesis(OneMonth_mincer, c("(Intercept) = 0", "forecast = 1")) 
# Can't do F-test because of missing coefficient estimate

# Finding MSE of 1 month forecasts:

# Gives accuracy statistics for 1 month
OneMonth_accuracy_stats <- tbl_df(accuracy(OneMonth_results[,3], OneMonth_results[,2]))

# Get the RMSE
OneMonth_rmse <- OneMonth_accuracy_stats %>% select(RMSE) 

# Calculate the MSE
OneMonth_mse <- (OneMonth_rmse)^2
# MSE = 0.00109739

# Mincer-Zarnowits test for 3 month forecast:

# Three month point forecasts
ThreeMonth_yhat <- dataframe_ThreeMonth_arima_pred %>% select(starts_with("Point"))

# Three month trade dates
ThreeMonth_dates <- ThreeMonth_OutOfSample %>% select(TradeDate)

# Dataframe of 3 month results
ThreeMonth_results <- data.frame(ThreeMonth_dates, ThreeMonth_OutOfSampleRet, ThreeMonth_yhat)

# Regression of actual and forecasted observations
ThreeMonth_mincer <- lm(DailyLogReturns ~ Point.Forecast, data = ThreeMonth_results)
summary(ThreeMonth_mincer)
ThreeMonth_arima_mincercoeff <- tidy(summary(ThreeMonth_mincer)) %>% select(estimate)
# intercept = 0,007329: positive bias estimated 
# forecast systematically underestimates the actual observation
# Can't find estimate of slope :(

# Joint hypothesis to test int=0 and slope=1
ThreeMonth_hyp <- linearHypothesis(ThreeMonth_mincer, c("(Intercept) = 0", "Point.Forecast = 1")) 

# Finding MSE of 3 month forecasts:

# Gives accuracy statistics for 3 months
ThreeMonth_accuracy_stats <- tbl_df(accuracy(ThreeMonth_results[,3], ThreeMonth_results[,2]))

# Get the RMSE
ThreeMonth_rmse <- ThreeMonth_accuracy_stats %>% select(RMSE) 

# Calculate the MSE
ThreeMonth_mse <- (ThreeMonth_rmse)^2
# MSE = 0.001549515

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
prophet_2 <- prophet(df = ThreeMonth_InSam_Proph, growth = "linear", weekly.seasonality = TRUE, yearly.seasonality = FALSE)

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
# Unlike arima, doesn't forecast a constant value into future
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
OneMonth_proph_accuracy <- tbl_df(accuracy(OneMonth_proph_results[,2], OneMonth_results[,3]))

# Get the RMSE
OneMonth_proph_rmse <- OneMonth_proph_accuracy %>% select(RMSE) 

# Calculate the MSE
OneMonth_proph_mse <- (OneMonth_proph_rmse)^2
# MSE = 0.00003613082

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
ThreeMonth_proph_accuracy <- tbl_df(accuracy(ThreeMonth_proph_results[,2], ThreeMonth_results[,3]))

# Get the RMSE
ThreeMonth_proph_rmse <- ThreeMonth_proph_accuracy %>% select(RMSE) 

# Calculate the MSE
ThreeMonth_proph_mse <- (ThreeMonth_proph_rmse)^2
# MSE = 0.00001661949 < MSE of 1 month prophet forecast

# Prophet produces better forecasts over longer forecast horizon

# -------- Comparing Prophet and Arima results over two time horizons

# Mincer-zarnowits results for one month - prophet
OneMonth_proph_mincercoef <- OneMonth_proph_mincercoeff %>% select(Prophet = estimate)
OneMonth_proph_mincerp <- na.omit(OneMonth_proph_mincerpval %>% select(Prophet = p.value))
OneMonth_proph_mincer <- rbind(OneMonth_proph_mincercoef, OneMonth_proph_mincerp) 

# Mincer-zarnowits results for one month - arima
OneMonth_arima_mincercoef <- OneMonth_arima_mincercoeff %>% select(ARIMA = estimate)
OneMonth_arima_mincer <- rbind(OneMonth_arima_mincercoef, NA, NA)

# Table of 1 month Mincer-Zarnowits results
mincer_table1 <- data.frame(OneMonth_arima_mincer, OneMonth_proph_mincer, row.names = c("intercept", "slope", "p value"))

# Mincer-zarnowits results for 3 months - prophet
ThreeMonth_proph_mincercoef <- ThreeMonth_proph_mincercoeff %>% select(Prophet = estimate)
ThreeMonth_proph_mincerp <- na.omit(ThreeMonth_proph_mincerpval %>% select(Prophet = p.value))
ThreeMonth_proph_mincer <- rbind(ThreeMonth_proph_mincercoef, ThreeMonth_proph_mincerp) 

# Mincer-zarnowits results for 3 months - arima
ThreeMonth_arima_mincercoef <- ThreeMonth_arima_mincercoeff %>% select(ARIMA = estimate)
ThreeMonth_arima_mincer <- rbind(ThreeMonth_arima_mincercoef, NA, NA)

# Table of 3 month Mincer-Zarnowits results
mincer_table2 <- data.frame(ThreeMonth_arima_mincer, ThreeMonth_proph_mincer, row.names = c("intercept", "slope", "p value"))

# Table of Mincer-zarnowits results for both forecast horizons
mincer_table3 <- data.frame(mincer_table1, mincer_table2)
# Prophet - intercept gets closer to 0, slope gets closer to 1, and p value increases for longer forecast horizon
# Can't make comparison with arima because of missing values :(

# Arima MSE's
arima_mse <- data.frame(OneMonth_mse, ThreeMonth_mse)

# Prophet MSE's
prophet_mse <- data.frame(OneMonth_proph_mse, ThreeMonth_proph_mse)

# Table of MSE's
mse_table <- matrix(data.frame(arima_mse, prophet_mse), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("ARIMA", "Prophet"), c("One Month MSE", "Three Month MSE")))
# MSE for Prophet is lower than ARIMA for both forecast horizons
# MSE for Prophet lower for longer forecast horizon
# MSE for ARIMA lower for shorter forecast horizon

# -----HOW DO THE FORECASTS FROM PROPHET'S AND ARIMA'S AUTOMATIC SELECTION PROCEDURE COMPARE-----

# Automatically selected arima model for 1 month forecast
OneMonth_AutoArima <- auto.arima(na.omit(OneMonth_InSampleRet), seasonal = FALSE) 
summary(OneMonth_AutoArima)
# arima(0,0,0) fitted
# same model chosen by box-jenkins methodology 

# Automatically selected arima model for 3 month forecast
ThreeMonth_AutoArima <- auto.arima(na.omit(ThreeMonth_InSampleRet), seasonal = FALSE)
summary(ThreeMonth_AutoArima)
# arima(0,0,0) fitted 

# Automatic selection of Prophet model for 1 month
OneMonth_autoproph <- prophet(df = OneMonth_InSam_Proph)
# Model fitted without yearly seasonality, linear trend, and includes weekly seasonality
# Same model chosen manually

# Automatic selection of Prophet model for 3 months
ThreeMonth_autoproph <- prophet(df = ThreeMonth_InSam_Proph)
# Model fitted without yearly seasonality, linear trend, and includes weekly seasonality
# Same model chosen manually


# The models automatically selected are the same as those chosen manually for both Prophet and ARIMA
# Evaluation and comparison of forecasts will therefore be the same as before:
# The automatically selected Prophet model produces better forecasts over auto.arima over both forecast horizons
# Forecasts of automatically selected Prophet model improves over longer forecast horizon
# Forecasts of auto.arima are better over shorter forecast horizons

#----------HOW ROBUST IS PROPHET WHEN THERE ARE MISSING VALUES COMPARED TO ARIMA?-----------------

# Prophet is said to be robust when there are missing values whereas arima requires Kalman smoothing and interpolation
# Want to test this out

# Missing values and Prophet 
OneMonth_naInSam_Proph <- OneMonth_InSample %>% select(ds = TradeDate, y = DailyLogReturns)
OneMonth_naInSam_Proph [c(4, 10, 17, 20, 35, 48, 50, 100, 110, 177, 140, 157, 200, 230, 245, 270, 286, 300, 323, 330),2] <- NA

ThreeMonth_naInSam_Proph <- ThreeMonth_InSample %>% select(ds = TradeDate, y = DailyLogReturns)
ThreeMonth_naInSam_Proph [c(4, 10, 17, 20, 35, 48, 50, 100, 110, 177, 140, 157, 200, 230, 245, 270),2] <- NA

# -------Is THERE AN OPPORTUNITY IF FORECASTING BITCOIN WITH PROPHET? A LOOK AT THE EMH----

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





