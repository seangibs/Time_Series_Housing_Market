library(fpp2)
library(readxl)
library(lubridate)
library(scales)
library(janitor)
library(timetk)
library(forecast)
library(pracma)
library(caret)
library(sos)
library(dplyr)
library(tidyverse)
library(fabletools)
library(fable)
library(tsibble)
library(plotly)
library(feasts)



data <- read.csv("NewHouseRegistrations_Ireland.csv")

colnames(data) <- c("Year","NewHouseRegistrations")

Y <- ts(data[,2], start = c(1978,1), frequency = 1)


autoplot(Y) + geom_point() +
  labs(title = "New House Registrations Ireland 1978 - 2019") +
  ylab("Houses") +
  xlab("") +
  scale_x_continuous(n.breaks = 15)


# Data has a strong trend
# Transform data to get rid of trend

# Take the first difference of the data to remove the trend

DY <- diff(Y)

# Time Plot of differenced data

autoplot(DY) + geom_point() +
  labs(title = "New House Registrations Ireland 1978 - 2019") +
  ylab("Houses") +
  xlab("") +
  scale_x_continuous(n.breaks = 21)

# Series appears trend-stationary, use to investigate seasonality
# clear seasonal patterns in the data


# won't be seasonal because it is yearly

ggseasonplot(DY) + 
  ggtitle("Seasonal Plot: Change in overseas trips") +
  ylab("Trips x 1000")


# another seasonal plot. This shows the changes by quarter
ggsubseriesplot(DY)

acf(Y)

pacf(Y)



############################################################
# Our series Y, has trend and seasonality
# To remove the trend, we take the first difference
# The first differenced series still has seasonality
#
# Forecast with various methods
############################################################

########
# Use a benchmark method to forecast
# Let's use the seasonal naive method as our benchmark.
# y_t = y_{t-s} + e_t e.g. value in Q1 2015 = value in Q1 2015 plus some random error
#######

fit <- naive(Y)

# resid sd is how well the model is fitting, the closer to 0 the better
summary(fit) # Resid SD - 8123.74 

checkresiduals(fit) # gives us a sense of how well this model is fitting the data
# want the data to look random in the first plot
# ACF - want to see any autocorrelation and want all bars to be within the two blue lines (95% confidence interval)
# model looks good


fcst <- forecast(fit,h=3)
autoplot(fcst) +
  ylab("") +
  xlab("") +
  scale_x_continuous(n.breaks = 17)
print(summary(fcst))

################
# Fit ETS method
################

fit_ets <- ets(Y)

fcst_ets <- forecast(fit_ets,h=3)
autoplot(fcst_ets) +
  ylab("") +
  xlab("") +
  scale_x_continuous(n.breaks = 17)

print(summary(fcst_ets))
summary(fit_ets) # Residual SD = 0.3675
checkresiduals(fit_ets) # 


####################
# Fit on ARIMA model
# Model has to be stationary
####################

fit_arima <- auto.arima(Y,d=1,stepwise=FALSE, approximation = FALSE, trace = TRUE) 
# before fitting arima model you need to take the first difference to remove trend
# Can also get rid of seasonality by D=1
# Taking the regular difference and the seasonal difference in order to get rid of the trend and seasonality so our data is stationary

summary(fit_arima) # Returns the squared error of 6085 which is 78.00641
checkresiduals(fit_arima) # 


##########################################
# Forecast with ETS
##########################################


fcst_arima <- forecast(fit_arima,h=3)
autoplot(fcst_arima) + geom_point() +
  ylab("") +
  xlab("") +
  scale_x_continuous(n.breaks = 15)

print(summary(fcst_arima))






