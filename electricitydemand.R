#
# Load libraries 
library(zoo)
library(xts)
library(TTR)
library(Rcpp)
library(rlang)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)
library(forecast)
library(expsmooth)
library(stats)
library(urca)
library(reshape)
library(shiny)
#
# Read in Data
electricitymonthly <- read.csv(file = "electricitymonthly.CSV", head=TRUE, sep = ",")
head(electricitymonthly)
# Time series function
electricitymonthlykilohours <- electricitymonthly$value
electricitymonthlykilohours <- ts(electricitymonthlykilohours, frequency = 12, start = c(1973,1))
electricitymonthlykilohours
# Preliminary (exploratory) analysis
hist(electricitymonthlykilohours) # distribution slightly right skewed
ggseasonplot(electricitymonthlykilohours, year.labels=TRUE, year.labels.left=TRUE) + # Seasonal Plot
  ylab("Kilowatt hours (Billions)") +
  ggtitle("Seasonal plot: Electricity Monthly Total Net Generation")
ggseasonplot(electricitymonthlykilohours, polar = TRUE)
  ylab("Kilowatt hours (Billions)") +
  ggtitle("Seasonal plot: Electricity Monthly Total Net Generation")
# Seasonal Subseries Plots 
ggsubseriesplot(electricitymonthlykilohours) +
    ylab("Kilowatt hours (Billions)") +
    ggtitle("Seasonal subseries plot: Electricity Monthly Total Net Generation")
electriclag <- window(electricitymonthlykilohours, start=1973)
gglagplot(electriclag)
# removed **ggAcf(electriclag)**
# Plotting 
plot(electricitymonthlykilohours, main = "Electricity Monthly Total Net Generation: Jan 1973 - Dec 2012", 
     ylab ="Kilowatt hours (Billions)", xlab = "Time", sub = "Source: US Energy Information Administration")
#  transform the time series by calculating the natural log of the original dat
logelectricitymonthlykilohours <- log(electricitymonthlykilohours)
plot.ts(logelectricitymonthlykilohours)
#
# Decomposing Non-Seasonal Data
# N/A
# Decomposing Seasonal Data
electricitymonthlykilohourscomp <- decompose(electricitymonthlykilohours)
electricitymonthlykilohourscomp
plot(electricitymonthlykilohourscomp)
#
# Decomposing Seasonal Data Adjusted 
electricitymonthlykilohourscompadj <- electricitymonthlykilohours - electricitymonthlykilohourscomp$seasonal
plot(electricitymonthlykilohourscompadj)
#
# Holt-Winters Exponential Smoothing
#
electricitymonthlykilohours <- log(electricitymonthlykilohours)
electricitymonthlykilohoursforecasts <- HoltWinters(electricitymonthlykilohours)
electricitymonthlykilohoursforecasts
electricitymonthlykilohoursforecasts$SSE
plot(electricitymonthlykilohoursforecasts)
#
# Forecast for future times 
#
test <- HoltWinters(electricitymonthlykilohours)
plot(test)
forecast <- predict(test, n.ahead = 24, prediction.interval = T, level = 0.95)
plot(test, forecast)
summary(forecast)

electricitymonthlykilohoursforecasts2 <- forecast:::forecast.HoltWinters(electricitymonthlykilohoursforecasts, h=24)
electricitymonthlykilohoursforecasts2
plot(electricitymonthlykilohoursforecasts2)
plot.forecast(electricitymonthlykilohoursforecasts2) # Error running function *!|*!!!
#
# ARIMA model
#
electricitymonthlykilohoursdiff1 <- diff(electricitymonthlykilohours, differences=1) # Run a KPSS test to confirm if data is stationary
plot.ts(electricitymonthlykilohoursdiff1) # no need to difference the time series twice as the first difference appears to be stationary 
# An Arima (p,1,q) model is apprpriate for the time  series. The trend has been removed and we are now with an irregular component, thus enabling us to see if there is correlations between sucessive erms of the irregular components.
#
# Selecting the correct Arima model and examine the corrlegram and partial correlogram 
#
acf(electricitymonthlykilohoursdiff1, lag.max = 20) # plot a correlogram
acf(electricitymonthlykilohoursdiff1, lag.max = 20, plot = FALSE) # get the autocorrelation values
#
pacf(electricitymonthlykilohoursdiff1, lag.max = 20) # plot the partial correlogram
pacf(electricitymonthlykilohoursdiff1, lag.max = 20, plot = FALSE) # get the autocorrelation values
# Ljung-Box Test
Box.test(electricitymonthlykilohours, lag =24, fitdf = 0, type = "Lj") # check that time series information exists in this data and its not purley white noise.
#
# Auto.Arima() Function
ts <- auto.arima(electricitymonthlykilohours)
ts
plot(electricitymonthlykilohours)
forecast1 <- forecast(ts, h=24)
forecast1
plot(forecast1)
plot(forecast1$residuals)
qqnorm(forecast1$residuals)
acf(forecast1$residuals)
pacf(forecast1$residuals)
summary(ts)

###new code 

forecast.predict <- predict(electricitymonthlykilohoursforecasts, n.ahead = 12, prediction.interval = TRUE)
forecast.predict
summary(forecast.predict)
plot(forecast.predict)




t <- ts(electricitymonthlykilohours, frequency = 12, start = c(1973,1))
plot(t)
tt <- HoltWinters(t)
plot(tt)
ttt <- predict(tt, n.ahead = 24, prediction.interval = T, level = 0.95)
plot(tt, ttt)
HWplot <- function(electricitymonthlykilohours, n.ahead=24, prediction.interval = T, level = 0.95)

tbats(electricitymonthlykilohours)

lambda <- BoxCox.lambda(electricitymonthlykilohours)
swfc <- forecast(electricitymonthlykilohours,h=52, lambda = lambda, robust = TRUE)
plot(swfc)
