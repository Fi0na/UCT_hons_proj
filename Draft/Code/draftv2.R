
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
ExchangeRates <- ExchangeRates %>% select(TradeDate = date, OpenPrice = ExchangeRates.Open, HighPrice = ExchangeRates.High, LowPrice = ExchangeRates.Low, ClosePrice = ExchangeRates.Close)

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

# Subsetting for forecast horizon of one month:

# 1 Month Test Set
OneMonth_OutOfSample <- ExchangeRates %>%
  dplyr::filter(TradeDate >= "2017-06-17")

# 1 Month Training Set
OneMonth_InSample <- ExchangeRates %>%
  dplyr::filter(TradeDate < "2017-06-17")

# Plots of 1 month training and test set closing prices
plot_1MonthTestPrices <- plot_prices %+% 
  OneMonth_OutOfSample + ggtitle("1 Month Test Set Closing Prices") +
  theme_minimal()

plot_1MonthTrainPrices <- plot_prices %+% 
  OneMonth_InSample + ggtitle("1 Month Training Set Closing Prices") +
  theme_minimal()

grid.arrange(plot_1MonthTrainPrices,
             plot_1MonthTestPrices,
             nrow = 2)


# Plots of 1 month training and test set daily log returns
plot_1MonthTestReturns <- plot_returns %+% 
  OneMonth_OutOfSample + 
  ggtitle("1 Month Test Set Daily Log Returns")

plot_1MonthTrainReturns <- plot_returns %+% 
  OneMonth_InSample + 
  ggtitle("1 Month Training Set Daily Log Returns") + 
  theme_minimal()

grid.arrange(plot_1MonthTrainReturns,
             plot_1MonthTestReturns,
             nrow = 2)

# Subsetting for forecast horizon of three months:

# 3 month Test Set
ThreeMonth_OutOfSample <- ExchangeRates %>% 
  dplyr::filter(TradeDate >= "2017-04-17")

# 3 month Training Set
ThreeMonth_InSample <- ExchangeRates %>% 
  dplyr::filter(TradeDate < "2017-04-17")

# Plots of 3 month training and test set closing prices
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

# Plots of 3 month training and test set daily log returns
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

# Subsetting for forecast horizon of six months:

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

# --- HOW DO THE FORECASTS FROM ARIMA & PROPHET COMPARE OVER DIFFERENT FORECAST HORIZONS -----

library(forecast)
library(tseries)

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
Acf(daily_logret, lag = 12, main = "ACF Plot of Daily Log Returns")
# None significantly different from 0
# Serial autocorrelation of daily log returns are small
# Acf indicates white noise

# Formal test for serial correlation: Portmanteu's test
Box.test(daily_logret, lag = 12, type = "Ljung")
# p - value = 0.1038 : fail to reject the null hypothesis of no serial correlations in log returns

# PACF Plot - identify order of AR
Pacf(daily_logret, lag = 12, main = "PACF Plot of Daily Log Returns")
# None significantly different from zero


# 3) Estimation of model parameters

# Fitting arima model for 1 month forecast horizon:

OneMonth_InSampleRet <- OneMonth_InSample %>% 
  select(DailyLogReturns)

OneMonth_OutOfSampleRet <- OneMonth_OutOfSample %>% 
  select(DailyLogReturns)

arima_1 <- Arima(na.omit(OneMonth_InSampleRet), order = c(0,0,0))   
summary(arima_1) 
AIC(arima_1)
# mean model
# AICc = -2081.34
   
arima_2 <- Arima(na.omit(OneMonth_InSampleRet), order = c(1,0,0))   
summary(arima_2)
AIC(arima_2)
# arima(1,0,0)  
# AICc = -2080.39
  
arima_3 <- Arima(na.omit(OneMonth_InSampleRet), order = c(0,0,1))   
summary(arima_3) 
AIC(arima_3)
# arima(0,0,1) 
# AICc = -2080.31
   
arima_4 <- Arima(na.omit(OneMonth_InSampleRet), order = c(1,0,1))    
summary(arima_4)
AIC(arima_4)
# arima(1,0,1)
# AICc = -2078.52

arima_5 <- Arima(na.omit(OneMonth_InSampleRet), order = c(2,0,1))    
summary(arima_5) 
AIC(arima_5)
# arima(2,0,1)
# AICc = -2084.97

arima_6 <- Arima(na.omit(OneMonth_InSampleRet), order = c(1,0,2))    
summary(arima_6) 
AIC(arima_6)
# arima(1,0,2)
# AICc = -2085.54

arima_7 <- Arima(na.omit(OneMonth_InSampleRet), order = c(2,0,2))    
summary(arima_7)
AIC(arima_7)
# arima(2,0,2)
# AICc = -2090.6 

# Based on the AICc, the model chosen for 1 month forecasting is the arima(2,0,2)
# Oh the irony of it changing from arima(0,0,0) with the previous data :( Sigh

# Fitting arima model for 3 month forecast horizon:
ThreeMonth_InSampleRet <- ThreeMonth_InSample %>% 
  select(DailyLogReturns)

ThreeMonth_OutOfSampleRet <- ThreeMonth_OutOfSample %>%
  select(DailyLogReturns)

arima_8 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(0,0,0))   
summary(arima_8) 
AIC(arima_8)
# mean model
# AICc = -1894.836

arima_9 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(1,0,0))   
summary(arima_9)
AIC(arima_9)
# arima(1,0,0)  
# AICc = -1893.05

arima_10 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(0,0,1))   
summary(arima_10) 
AIC(arima_10)
# arima(0,0,1) 
# AICc = -1893.087

arima_11 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(1,0,1))    
summary(arima_11)
AIC(arima_11)
# arima(1,0,1)
# AICc = -1891.155

arima_12 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(2,0,1))    
summary(arima_12) 
AIC(arima_12)
# arima(2,0,1)
# AICc = -1900.62

arima_13 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(1,0,2))    
summary(arima_13) 
AIC(arima_13)
# arima(1,0,2)
# AICc = -1901.62

arima_14 <- Arima(na.omit(ThreeMonth_InSampleRet), order = c(2,0,2))    
summary(arima_14)
AIC(arima_14)
# arima(2,0,2)
# AICc = -1907.15

# Based on the AICc, the model chosen for 3 month forecasting is the arima(2,0,2)
# Oh the irony of it changing from arima(0,0,0) again :( double sigh

# Fitting arima model for 6 month forecast horizon:
SixMonth_InSampleRet <- SixMonth_InSample %>% 
  select(DailyLogReturns)

SixMonth_OutOfSampleRet <- SixMonth_OutOfSample %>%
  select(DailyLogReturns)

arima_15 <- Arima(na.omit(SixMonth_InSampleRet), order = c(0,0,0))   
summary(arima_15) 
AIC(arima_15)
# mean model
# AICc = -1617.42

arima_16 <- Arima(na.omit(SixMonth_InSampleRet), order = c(1,0,0))   
summary(arima_16)
AIC(arima_16)
# arima(1,0,0)  
# AICc = -1618.86

arima_17 <- Arima(na.omit(SixMonth_InSampleRet), order = c(0,0,1))   
summary(arima_17) 
AIC(arima_17)
# arima(0,0,1) 
# AICc = -1619.06

arima_18 <- Arima(na.omit(SixMonth_InSampleRet), order = c(1,0,1))    
summary(arima_18)
AIC(arima_18)
# arima(1,0,1)
# AICc = -1617.09

arima_19 <- Arima(na.omit(SixMonth_InSampleRet), order = c(2,0,1))    
summary(arima_19) 
AIC(arima_19)
# arima(2,0,1)
# AICc = -1615.15

arima_20 <- Arima(na.omit(SixMonth_InSampleRet), order = c(1,0,2))    
summary(arima_20) 
AIC(arima_20)
# arima(1,0,2)
# AICc = -1615.1

arima_21 <- Arima(na.omit(SixMonth_InSampleRet), order = c(2,0,2))    
summary(arima_21)
AIC(arima_21)
# arima(2,0,2)
# AICc = -1624.3

# Based on the AICc, the model chosen for 6 month forecasting is the arima(2,0,2)


# 4) Diagnostic Check

# Checking 1 month residuals:
checkresiduals(arima_7)
# p-value = 0.2861 - No serial correlation in the residuals

# Checking the significance of model parameters
coeftest(arima_7)
# All AR and MA parameters are significant 

# Final model chosen for 1 month forecasting is the arima(2,0,2)

# Checking 3 month residuals
checkresiduals(arima_14)
# p-value = 0.03078 - reject the null hypothesis of no serial correlation in residuals

# Check the model with the next best AICc
checkresiduals(arima_13)
# p-value = 0.1098 - no serial correlation in the residuals

# Checking significance of model parameters
coeftest(arima_13)
# All AR and MA parameters are significant

# Final model chosen for 3 month forecasting is the arima(1,0,2)

# Checking 6 month residuals
checkresiduals(arima_21)
# p-value = 0.03918 - reject the null hypothesis of no serial correlation in residuals

# Check the residuals of the model with the next best AICc
checkresiduals(arima_17)
# p-value = 0.05576 - no serial correlation in the residuals

# Checking the significance of model parameters
coeftest(arima_17)
# MA parameter is signicant at the 5% level

# Final model chosen for 6 month forecasting is the arima(0,0,1)


# 5) Forecasting with the final model

# One-step ahead forecasts for 1 month out-of sample with re-estimation
OneMonth_OutOfSample_Dates <- seq.Date(as.Date("2017-06-17"), 
                                       by = "day" ,
                                       length.out = nrow(OneMonth_OutOfSampleRet))

OneMonth_Rest_Results <- list()

for(i in 1:length(OneMonth_OutOfSample_Dates)){
  OneMonth_InSam_Rest <- ExchangeRates %>%
    dplyr::filter(TradeDate < OneMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  OneMonth_OutSam_Rest <- ExchangeRates %>%
    dplyr::filter(TradeDate == OneMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  arima_22 <- Arima(na.omit(OneMonth_InSam_Rest), order = c(2,0,2))  
  OneMonth_ArimaPred_Rest <- forecast(arima_22, h = 1)
  
  OneMonth_Rest_Results[[i]] <- data.frame(date = OneMonth_OutOfSample_Dates[i],
                                             actual = as.numeric(OneMonth_OutSam_Rest),
                                             forecast = as.numeric(OneMonth_ArimaPred_Rest$mean)) %>%
    mutate(errors = actual - forecast)
}

OneMonth_Rest_Results <- do.call(rbind, OneMonth_Rest_Results)

# Plot of 1 month ARIMA forecast
ts.plot(OneMonth_Rest_Results %>% select(actual, forecast), 
        lty = 1:2, 
        col = c("BLUE", "RED"),
        main = "1 Month Forecasts from ARIMA(2,0,2)",
        xlab = "Time",
        ylab = "Daily Log Returns")

legend("topleft", 
       legend = c("actual", "forecast"), 
       lty = 1:2, 
       col = c("BLUE", "RED"))

# Forecasts doesn't follow the peaks and dips well. They're fairly constant.
# It also fails to capture the upward trend towards the end of the forecast horizon.

# One-step ahead forecasts for 3 month out-of sample with re-estimation
ThreeMonth_OutOfSample_Dates <- seq.Date(as.Date("2017-04-17"), 
                                         by = "day" ,
                                         length.out = nrow(ThreeMonth_OutOfSampleRet))

ThreeMonth_Rest_Results <- list()

for(i in 1:length(ThreeMonth_OutOfSample_Dates)){
  ThreeMonth_InSam_Rest <- ExchangeRates %>%
    dplyr::filter(TradeDate < ThreeMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  ThreeMonth_OutSam_Rest <- ExchangeRates %>%
    dplyr::filter(TradeDate == ThreeMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  arima_23 <- Arima(na.omit(ThreeMonth_InSam_Rest), order = c(1,0,2))  
  ThreeMonth_ArimaPred_Rest <- forecast(arima_23, h = 1)
  
  ThreeMonth_Rest_Results[[i]] <- data.frame(date = ThreeMonth_OutOfSample_Dates[i],
                                               actual = as.numeric(ThreeMonth_OutSam_Rest),
                                               forecast = as.numeric(ThreeMonth_ArimaPred_Rest$mean)) %>%
    mutate(errors = actual - forecast)
}

ThreeMonth_Rest_Results <- do.call(rbind, ThreeMonth_Rest_Results)

# Plot of 3 month ARIMA forecast
ts.plot(ThreeMonth_Rest_Results %>% select(actual, forecast), 
        lty = 1:2, 
        col = c("BLUE", "RED"),
        main = "3 Month Forecasts from ARIMA(1,0,2)",
        xlab = "Time",
        ylab = "Daily Log Returns")

legend("topleft", 
       legend = c("actual", "forecast"), 
       lty = 1:2, 
       col = c("BLUE", "RED"))


# Similar to the 1 month forecasts, the arima doesn't follow the peaks and dips well. They're fairly constant.
# It also fails to capture the upward trend towards the end of the forecast horizon.

# One-step ahead forecasts for 6 month out-of sample with re-estimation
SixMonth_OutOfSample_Dates <- seq.Date(as.Date("2017-01-17"), 
                                         by = "day" ,
                                         length.out = nrow(SixMonth_OutOfSampleRet))

SixMonth_Rest_Results <- list()

for(i in 1:length(SixMonth_OutOfSample_Dates)){
  SixMonth_InSam_Rest <- ExchangeRates %>%
    dplyr::filter(TradeDate < SixMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  SixMonth_OutSam_Rest <- ExchangeRates %>%
    dplyr::filter(TradeDate == SixMonth_OutOfSample_Dates[i]) %>% 
    select(DailyLogReturns)
  
  arima_24 <- Arima(na.omit(SixMonth_InSam_Rest), order = c(0,0,1))  
  SixMonth_ArimaPred_Rest <- forecast(arima_24, h = 1)
  
  SixMonth_Rest_Results[[i]] <- data.frame(date = SixMonth_OutOfSample_Dates[i],
                                             actual = as.numeric(SixMonth_OutSam_Rest),
                                             forecast = as.numeric(SixMonth_ArimaPred_Rest$mean)) %>%
    mutate(errors = actual - forecast)
}

SixMonth_Rest_Results <- do.call(rbind, SixMonth_Rest_Results)

# Plot of 6 month ARIMA forecast
ts.plot(SixMonth_Rest_Results %>% select(actual, forecast), 
        lty = 1:2, 
        col = c("BLUE", "RED"),
        main = "6 Month Forecasts from ARIMA(0,0,1)",
        xlab = "Time",
        ylab = "Daily Log Returns")

legend("topleft", 
       legend = c("actual", "forecast"), 
       lty = 1:2, 
       col = c("BLUE", "RED"))

# Forecasts doesn't follow the peaks and dips well. They're fairly constant.
# It also fails to capture the upward trend towards the end of the forecast horizon.

# ------ Evaluating forecast accuracy of arima models

library(broom)

# Mincer-Zarnowitz test for 1 month forecast:

# Regression of actual and forecasted observations
OneMonth_mincer <- OneMonth_Rest_Results %>%  
  lm(actual ~ forecast, data = .)

summary(OneMonth_mincer)


# Coefficients from regression
OneMonth_arima_mincercoeff <- coef(OneMonth_mincer)
# intercept = -0.005561: negative bias estimated 
# forecast systematically overestimates the actual observation
# slope = -0.062666: far from 1

# Joint hypothesis to test int = 0 and slope = 1
library(car)
OneMonth_hyp <- linearHypothesis(OneMonth_mincer, c("(Intercept) = 0", "forecast = 1")) 
OneMonth_arima_mincerpval <- tidy(OneMonth_hyp) %>% select(p.value)
# Large p-value of 0.5462326
# Fail to reject the null hypothesis of int=0 and slope=1

# Plot of regression line vs one-to-one line
plot_1MonthReg <- ggplot(OneMonth_Rest_Results, aes(x = forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  labs(title = paste("intercept =", round(OneMonth_arima_mincercoeff[1], digits = 3),
                     "slope =", round(OneMonth_arima_mincercoeff[2], digits = 3),
                     "p value =", round(OneMonth_arima_mincerpval[2,], digits = 3)))

# Finding MSE of 1 month forecasts:

# 1 month MSE
OneMonth_accuracy <- tidy(accuracy(OneMonth_Rest_Results[,3], OneMonth_Rest_Results[,2]))
OneMonth_rmse <- OneMonth_accuracy %>% select(RMSE)
OneMonth_mse <- (OneMonth_rmse %>% select(MSE = RMSE))^2
# MSE = 0.002519608

# Mincer-Zarnowitz test for 3 month forecast:

# Regression of actual and forecasted observations
ThreeMonth_mincer <- ThreeMonth_Rest_Results %>%  
  lm(actual ~ forecast, data = .)

summary(ThreeMonth_mincer)

ThreeMonth_arima_mincercoeff <- coef(ThreeMonth_mincer)
# intercept = 0.005190: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = 0.384396 - far from 1

# Joint hypothesis to test int = 0 and slope = 1
ThreeMonth_hyp <- linearHypothesis(ThreeMonth_mincer, c("(Intercept) = 0", "forecast = 1")) 
ThreeMonth_arima_mincerpval <- tidy(ThreeMonth_hyp) %>% select(p.value)
# Large p-value of 0.6019783
# Fail to reject the null hypothesis of int=0 and slope=1 

# Plot of regression line vs one-to-one line
plot_3MonthReg <- ggplot(ThreeMonth_Rest_Results, aes(x = forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  labs(title = paste("intercept =", round(ThreeMonth_arima_mincercoeff[1], digits = 3),
                     "slope =", round(ThreeMonth_arima_mincercoeff[2], digits = 3),
                     "p value =", round(ThreeMonth_arima_mincerpval[2,], digits = 3)))


# Finding MSE of 3 month forecasts:

# 3 month MSE
ThreeMonth_accuracy <- tidy(accuracy(ThreeMonth_Rest_Results[,3], ThreeMonth_Rest_Results[,2]))
ThreeMonth_rmse <- ThreeMonth_accuracy %>% select(RMSE)
ThreeMonth_mse <- (ThreeMonth_rmse %>% select(MSE = RMSE))^2
# MSE = 0.002112285

# Mincer-Zarnowitz test for 6 month forecast:

# Regression of actual and forecasted observations
SixMonth_mincer <- SixMonth_Rest_Results %>%  
  lm(actual ~ forecast, data = .)

summary(SixMonth_mincer)

SixMonth_arima_mincercoeff <- coef(SixMonth_mincer)
# intercept = 0.009779: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -2.099246: far from 1

# Joint hypothesis to test int = 0 and slope = 1
SixMonth_hyp <- linearHypothesis(SixMonth_mincer, c("(Intercept) = 0", "forecast = 1")) 
SixMonth_arima_mincerpval <- tidy(SixMonth_hyp) %>% select(p.value)
# Small p-value of 0.03197636
# Reject the null hypothesis of int=0 and slope=1 

# Plot of regression line vs one-to-one line
plot_6MonthReg <- ggplot(SixMonth_Rest_Results, aes(x = forecast, y = actual)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  labs(title = paste("intercept =", round(SixMonth_arima_mincercoeff[1], digits = 3),
                     "slope =", round(SixMonth_arima_mincercoeff[2], digits = 3),
                     "p value =", round(SixMonth_arima_mincerpval[2,], digits = 3)))


# Finding MSE of 6 month forecasts:

# 6 month MSE
SixMonth_accuracy <- tidy(accuracy(SixMonth_Rest_Results[,3], SixMonth_Rest_Results[,2]))
SixMonth_rmse <- SixMonth_accuracy %>% select(RMSE)
SixMonth_mse <- (SixMonth_rmse %>% select(MSE = RMSE))^2
# MSE = 0.001937929


# ------ Forecasting with Prophet

library(prophet)

# Put data into format that prophet likes
OneMonth_InSam_Proph <- OneMonth_InSample %>% 
  select(ds = TradeDate, y = DailyLogReturns)

ThreeMonth_InSam_Proph <- ThreeMonth_InSample %>% 
  select(ds = TradeDate, y = DailyLogReturns)

SixMonth_InSam_Proph <- SixMonth_InSample %>% 
  select(ds = TradeDate, y = DailyLogReturns)

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

# Fitting model for 6 months
prophet_3 <- prophet(df = SixMonth_InSam_Proph, 
                     growth = "linear", 
                     weekly.seasonality = TRUE, 
                     yearly.seasonality = FALSE)

# Creating dataframe for forecasting
OneMonth_future1 <- make_future_dataframe(prophet_1, periods = nrow(OneMonth_OutOfSample))
tail(OneMonth_future1)

ThreeMonth_future1 <- make_future_dataframe(prophet_2, periods = nrow(ThreeMonth_OutOfSample))
tail(ThreeMonth_future1)

SixMonth_future1 <- make_future_dataframe(prophet_3, periods = nrow(SixMonth_OutOfSample))
tail(SixMonth_future1)

# Forecasting for 1 month 
OneMonth_proph1_pred <- predict(prophet_1, OneMonth_future1)
tail(OneMonth_proph1_pred[c("ds", "yhat", "yhat_lower", "yhat_upper")])

# Forecasting for 3 months 
ThreeMonth_proph2_pred <- predict(prophet_2, ThreeMonth_future1)
tail(ThreeMonth_proph2_pred[c("ds", "yhat", "yhat_lower", "yhat_upper")])

# Forecasting for 6 months 
SixMonth_proph3_pred <- predict(prophet_3, SixMonth_future1)
tail(SixMonth_proph3_pred[c("ds", "yhat", "yhat_lower", "yhat_upper")])

# Looking at plots
plot(prophet_1, OneMonth_proph1_pred)
# Doesn't follow data very well
# Wide uncertainty intervals

plot(prophet_2, ThreeMonth_proph2_pred)
# Doesn't follow data well again 
# Wide uncertainty interval

plot(prophet_3, SixMonth_proph3_pred)
# Doesn't follow data well 
# Wide uncertainty interval

prophet_plot_components(prophet_1, OneMonth_proph1_pred)
# Returns have been increasing over the past year
# Prophet forecasts lower returns around Tues-Wed
# This differs to our prior knowledge of lower trading activity around the weekend and higher activity around Tues-Thurs

prophet_plot_components(prophet_2, ThreeMonth_proph2_pred)
# Returns have been increasing over the past year
# Prophet forecasts higher returns around the weekend and lower returns during the week
# This differs to our prior knowledge of lower trading activity around the weekend and higher activity around Tues-Thurs

prophet_plot_components(prophet_3, SixMonth_proph3_pred)
# Returns have been increasing over the past year
# Prophet forecasts higher returns around the weekend and lower returns during the week
# This differs to our prior knowledge of lower trading activity around the weekend and higher activity around Tues-Thurs

# -------- Evaluating forecast accuracy of Prophet models

# Mincer-Zarnowits test for 1 month forecast:

# one month point forecasts
OneMonth_proph_yhat <- OneMonth_proph1_pred %>% dplyr::filter(ds >= as.Date("2017-06-17")) %>%
  select(ds, yhat)

# Dataframe of one month results
OneMonth_proph_results <- data.frame(OneMonth_proph_yhat, OneMonth_OutOfSampleRet)

# Regression of actual and forecasted observations
OneMonth_proph_mincer <- lm(DailyLogReturns ~ yhat, data = OneMonth_proph_results)
summary(OneMonth_proph_mincer)

OneMonth_proph_mincercoeff <- coef(OneMonth_proph_mincer)
# intercept = -0.02610: negative bias estimated 
# forecast systematically overestimates the actual observation
# slope = 2.40105: far from 1

# Joint hypothesis to test null: int = 0 and slope = 1
OneMonth_proph_hyp <- linearHypothesis(OneMonth_proph_mincer, c("(Intercept) = 0", "yhat = 1")) 
OneMonth_proph_mincerpval <- tidy(OneMonth_proph_hyp) %>% select(p.value)
# p = 0.2896413
# Fail to reject null hypothesis 

# Plot of regression line vs one-to-one line
plot_1Month_ProphReg <- ggplot(OneMonth_proph_results, aes(x = yhat, y = DailyLogReturns)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  xlab("forecast") +
  ylab("actual") +
  labs(title = paste("intercept =", round(OneMonth_proph_mincercoeff[1], digits = 3),
                     "slope =", round(OneMonth_proph_mincercoeff[2], digits = 3),
                     "p value =", round(OneMonth_proph_mincerpval[2,], digits = 3)))

# Finding MSE of 1 month forecasts:

# Gives accuracy statistics for 1 month
OneMonth_proph_accuracy <- tbl_df(accuracy(OneMonth_proph_results[,2], OneMonth_proph_results[,3]))

# Get the RMSE
OneMonth_proph_rmse <- OneMonth_proph_accuracy %>% 
  select(RMSE) 

# Calculate the MSE
OneMonth_proph_mse <- (OneMonth_proph_accuracy %>% 
                         select(MSE = RMSE))^2
# MSE = 0.002598327

# Mincer-Zarnowits test for 3 month forecast:

# 3 month point forecasts
ThreeMonth_proph_yhat <- ThreeMonth_proph2_pred %>% 
  dplyr::filter(ds >= as.Date("2017-04-17")) %>%
  select(ds, yhat)

# Dataframe of 3 month results
ThreeMonth_proph_results <- data.frame(ThreeMonth_proph_yhat, ThreeMonth_OutOfSampleRet)

# Regression of actual and forecasted observations
ThreeMonth_proph_mincer <- lm(DailyLogReturns ~ yhat, data = ThreeMonth_proph_results)
summary(ThreeMonth_proph_mincer)

ThreeMonth_proph_mincercoeff <- coef(ThreeMonth_proph_mincer)
# intercept = 0.01177: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -1.28427: far from 1

# Joint hypothesis to test null: int = 0 and slope = 1
ThreeMonth_proph_hyp <- linearHypothesis(ThreeMonth_proph_mincer, c("(Intercept) = 0", "yhat = 1")) 
ThreeMonth_proph_mincerpval <- tidy(ThreeMonth_proph_hyp) %>% select(p.value)
# p = 0.398618 
# Fail to reject null hypothesis 

# Plot of regression line vs one-to-one line
plot_3Month_ProphReg <- ggplot(ThreeMonth_proph_results, aes(x = yhat, y = DailyLogReturns)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  xlab("forecast") +
  ylab("actual") +
  labs(title = paste("intercept =", round(ThreeMonth_proph_mincercoeff[1], digits = 3),
                     "slope =", round(ThreeMonth_proph_mincercoeff[2], digits = 3),
                     "p value =", round(ThreeMonth_proph_mincerpval[2,], digits = 3)))

# Finding MSE of 3 month forecasts:

# Gives accuracy statistics for 3 month
ThreeMonth_proph_accuracy <- tbl_df(accuracy(ThreeMonth_proph_results[,2], ThreeMonth_proph_results[,3]))

# Get the RMSE
ThreeMonth_proph_rmse <- ThreeMonth_proph_accuracy %>% select(RMSE) 

# Calculate the MSE
ThreeMonth_proph_mse <- (ThreeMonth_proph_accuracy %>% select(MSE = RMSE))^2
# MSE = 0.002124234 

# Mincer-Zarnowits test for 6 month forecast:

# 6 month point forecasts
SixMonth_proph_yhat <- SixMonth_proph3_pred %>% 
  dplyr::filter(ds >= as.Date("2017-01-17")) %>%
  select(ds, yhat)

# Dataframe of 6 month results
SixMonth_proph_results <- data.frame(SixMonth_proph_yhat, SixMonth_OutOfSampleRet)

# Regression of actual and forecasted observations
SixMonth_proph_mincer <- lm(DailyLogReturns ~ yhat, data = SixMonth_proph_results)
summary(SixMonth_proph_mincer)

SixMonth_proph_mincercoeff <- coef(SixMonth_proph_mincer)
# intercept = 0.010117: positive bias estimated 
# forecast systematically underestimates the actual observation
# slope = -1.504738: far from 1

# Joint hypothesis to test null: int = 0 and slope = 1
SixMonth_proph_hyp <- linearHypothesis(SixMonth_proph_mincer, c("(Intercept) = 0", "yhat = 1")) 
SixMonth_proph_mincerpval <- tidy(SixMonth_proph_hyp) %>% select(p.value)
# p = 0.002234718
# Reject null hypothesis 

# Plot of regression line vs one-to-one line
plot_6Month_ProphReg <- ggplot(SixMonth_proph_results, aes(x = yhat, y = DailyLogReturns)) +
  stat_smooth(aes(colour = "regression line"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_abline(aes(colour = "one-to-one line"), intercept = 0, slope = 1) +
  xlab("forecast") +
  ylab("actual") +
  labs(title = paste("intercept =", round(SixMonth_proph_mincercoeff[1], digits = 3),
                     "slope =", round(SixMonth_proph_mincercoeff[2], digits = 3),
                     "p value =", round(SixMonth_proph_mincerpval[2,], digits = 3)))

# Finding MSE of 6 month forecasts:

# Gives accuracy statistics for 6 month
SixMonth_proph_accuracy <- tbl_df(accuracy(SixMonth_proph_results[,2], SixMonth_proph_results[,3]))

# Get the RMSE
SixMonth_proph_rmse <- SixMonth_proph_accuracy %>% select(RMSE) 

# Calculate the MSE
SixMonth_proph_mse <- (SixMonth_proph_accuracy %>% select(MSE = RMSE))^2
# MSE = 0.001979009

# -------- Comparing Prophet and Arima results over the different forecast horizons

# Mincer-Zarnowitz results for one month - prophet
OneMonth_proph_mincercoef <- data.frame(OneMonth_proph_mincercoeff) %>% 
  select(Prophet = OneMonth_proph_mincercoeff)

OneMonth_proph_mincerp <- na.omit(OneMonth_proph_mincerpval %>% 
                                    select(Prophet = p.value))

OneMonth_proph_mincer <- rbind(OneMonth_proph_mincercoef, OneMonth_proph_mincerp) 

# Mincer-Zarnowitz results for one month - arima
OneMonth_arima_mincercoef <- data.frame(OneMonth_arima_mincercoeff) %>% 
  select(ARIMA = OneMonth_arima_mincercoeff)

OneMonth_arima_mincerp <- na.omit(OneMonth_arima_mincerpval %>% 
                                    select(ARIMA = p.value))

OneMonth_arima_mincer <- rbind(OneMonth_arima_mincercoef, OneMonth_arima_mincerp)

# Table of 1 month Mincer-Zarnowitz results
mincer_table1 <- data.frame(OneMonth_arima_mincer, OneMonth_proph_mincer, row.names = c("intercept", "slope", "p value"))

# Mincer-Zarnowitz results for 3 months - prophet
ThreeMonth_proph_mincercoef <- data.frame(ThreeMonth_proph_mincercoeff) %>% 
  select(Prophet = ThreeMonth_proph_mincercoeff)

ThreeMonth_proph_mincerp <- na.omit(ThreeMonth_proph_mincerpval %>% 
                                      select(Prophet = p.value))

ThreeMonth_proph_mincer <- rbind(ThreeMonth_proph_mincercoef, ThreeMonth_proph_mincerp) 

# Mincer-Zarnowitz results for 3 months - arima
ThreeMonth_arima_mincercoef <- data.frame(ThreeMonth_arima_mincercoeff) %>% 
  select(ARIMA = ThreeMonth_arima_mincercoeff)

ThreeMonth_arima_mincerp <- na.omit(ThreeMonth_arima_mincerpval %>% 
                                      select(ARIMA = p.value))

ThreeMonth_arima_mincer <- rbind(ThreeMonth_arima_mincercoef, ThreeMonth_arima_mincerp)

# Table of 3 month Mincer-Zarnowitz results
mincer_table2 <- data.frame(ThreeMonth_arima_mincer, ThreeMonth_proph_mincer, row.names = c("intercept", "slope", "p value"))

# Mincer-Zarnowitz results for 6 months - prophet
SixMonth_proph_mincercoef <- data.frame(SixMonth_proph_mincercoeff) %>% 
  select(Prophet = SixMonth_proph_mincercoeff)

SixMonth_proph_mincerp <- na.omit(SixMonth_proph_mincerpval %>% 
                                      select(Prophet = p.value))

SixMonth_proph_mincer <- rbind(SixMonth_proph_mincercoef, SixMonth_proph_mincerp) 

# Mincer-Zarnowitz results for 6 months - arima
SixMonth_arima_mincercoef <- data.frame(SixMonth_arima_mincercoeff) %>% 
  select(ARIMA = SixMonth_arima_mincercoeff)

SixMonth_arima_mincerp <- na.omit(SixMonth_arima_mincerpval %>% 
                                      select(ARIMA = p.value))

SixMonth_arima_mincer <- rbind(SixMonth_arima_mincercoef, SixMonth_arima_mincerp)

# Table of 6 month Mincer-Zarnowitz results
mincer_table3 <- data.frame(SixMonth_arima_mincer, SixMonth_proph_mincer, row.names = c("intercept", "slope", "p value"))


# Table of Mincer-Zarnowitz results for all forecast horizons
mincer_table4 <- data.frame(mincer_table1, mincer_table2, mincer_table3)
# Prophet - intercept gets closest to 0, slope gets closest to 1, and p value is highes for the 3 month forecast horizon
# ARIMA - intercept gets closest to 0, slope gets closest to 1, and p value is highes for the 3 month forecast horizon
# ARIMA outperforms Prophet over all forecast horizons

# Arima MSE's
arima_mse <- data.frame(OneMonth_mse, ThreeMonth_mse, SixMonth_mse)

# Prophet MSE's
prophet_mse <- data.frame(OneMonth_proph_mse, ThreeMonth_proph_mse, SixMonth_proph_mse)

# Table of MSE's
mse_table <- matrix(data.frame(arima_mse, prophet_mse), 
                    nrow = 2, 
                    ncol = 3, 
                    byrow = TRUE, 
                    dimnames = list(c("ARIMA", "Prophet"), c("One Month MSE", "Three Month MSE", "Six Month MSE")))

# MSE for ARIMA improves as the forecast horizon gets longer
# MSE for Prophet also improves as the forecast horizon gets longer
# MSE for ARIMA is lower than Prophet over all forecast horizons - agrees with Mincer-Zarnowitz results 


# ------------HOW DO PROPHET'S AND ARIMA'S AUTOMATIC SELECTION PROCEDURE COMPARE----------

# Automatically selected arima model for 1 month forecast
OneMonth_AutoArima <- auto.arima(na.omit(OneMonth_InSampleRet), seasonal = FALSE) 
summary(OneMonth_AutoArima)
# arima(0,0,0) with non-zero mean fitted
# manually chose arima(2,0,2) which has a lower AICc 

# Automatically selected arima model for 3 month forecast
ThreeMonth_AutoArima <- auto.arima(na.omit(ThreeMonth_InSampleRet), seasonal = FALSE)
summary(ThreeMonth_AutoArima)
# arima(2,0,2) with zero mean fitted
# initially chose same model with mean but also found that residuals had serial correlation so arima(1,0,2) was chosen instead
# analyst would have to know diagnostic checking to see this!

# Automatically selected arima model for 6 month forecast
SixMonth_AutoArima <- auto.arima(na.omit(SixMonth_InSampleRet), seasonal = FALSE) 
summary(SixMonth_AutoArima)
# arima(1,0,0) with zero mean fitted 
# manually chose arima(0,0,1) which has a marginally higher AICc of 0.9 

# Automatic selection of Prophet model for 1 month
OneMonth_autoproph <- prophet(df = OneMonth_InSam_Proph)
# Model fitted with linear trend, excludes yearly seasonality and includes weekly seasonality
# Same model chosen manually

# Automatic selection of Prophet model for 3 months
ThreeMonth_autoproph <- prophet(df = ThreeMonth_InSam_Proph)
# Model fitted with linear trend, excludes yearly seasonality and includes weekly seasonality
# Same model chosen manually

# Automatic selection of Prophet model for 6 months
SixMonth_autoproph <- prophet(df = SixMonth_InSam_Proph)
# Model fitted with linear trend, excludes yearly seasonality and includes weekly seasonality
# Same model chosen manually

# The models automatically selected for Prophet are the same as those chosen manually 
# The models automatically selected for ARIMA differ to those chosen manually, and shows that it is useful to have an analyst in the loop


# -------Is THERE AN OPPORTUNITY IN FORECASTING BITCOIN? A LOOK AT THE EMH----

# 3 month trading profit with prophet
ThreeMonth_prophprofit <- sum(with(ThreeMonth_proph_results, (yhat>0)*DailyLogReturns + (yhat<0)*-DailyLogReturns))
ThreeMonth_prophprofit
# profit = 0.4023691

# 3 month trading profit with arima
ThreeMonth_arimaprofit <- sum(with(ThreeMonth_Rest_Results, (forecast>0)*actual + (forecast<0)*-actual))
ThreeMonth_arimaprofit
# profit = 0.455721

# Profit from random trade signals
num_simulations <- 10000

random_return <- numeric(length = num_simulations)

for(i in seq(num_simulations)) {
  random_vector <- rnorm(nrow(ThreeMonth_proph_results))
  
  random_return[i] <- sum(with(ThreeMonth_proph_results, (random_vector>0)*DailyLogReturns +(random_vector<0)*-DailyLogReturns))
}

# Comparison of profit
comp_profit <- data.frame(random_return, ThreeMonth_prophprofit, ThreeMonth_arimaprofit)

plot_ThreeMonth_profit <- ggplot(comp_profit, aes(random_return)) +
  geom_histogram() +
  geom_vline(aes(xintercept = ThreeMonth_prophprofit), size = 1, colour = "blue") +
  geom_vline(aes(xintercept = ThreeMonth_arimaprofit), size = 1, colour = "purple") +
  labs(title = paste("Comparison of Profit from Random Trade Signals")) +
  xlab("returns from random trade signals") +
  ylab("no. of simulations")

plot_ThreeMonth_profit





