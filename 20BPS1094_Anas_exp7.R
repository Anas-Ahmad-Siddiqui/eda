# Author: Anas Ahmad Siddiqui
# Branch: CSE CPS
# Registration Number: 20BPS1094
# Group: G1

###################################################
options(prompt="Anas 20BPS1094 > ", continue = " ")
###################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(corrplot)
library(gridExtra)
library(forecast)
library(tseries)
library(TSA)
library(tibble)
library(TTR)

data <- read.csv("TCS.csv")

data[is.na(data)] <- 0

data$date_numeric <- as.numeric(as.Date(data$Date))
data$date <- as.Date(data$Date)

head(data)

summary(data)

plot(data$date, data$Close, type = "l", col = "red", xlab = "Time", ylab = "Price", main = "TCS stocks over the years")
lines(data$date, data$Open, col = "blue")
legend("topright", legend = c("Closing Price", "Opening Price"), lty = c(1, 1), col = c("red", "blue"))

plot(data$date, data$High, type = "l", col = "red", xlab = "Time", ylab = "Price", main = "TCS stocks over the years")
lines(data$date, data$Low, col = "blue")
legend("topright", legend = c("High", "Low"), lty = c(1, 1), col = c("red", "blue"))

library(zoo)
rolling_avg <- rollmean(data$Close, k = 100,align = "right", fill = NA)
plot(data$date, rolling_avg, type = "l", xlab = "Time", ylab = "Price", main = "Rolling Average")



dates <- seq(as.Date("2004-08-25"), by = "day", length.out = length(data$date))
ts_obj <- ts(data$Close, start = c(2004, 8), frequency = 365.25)

plot.ts(ts_obj, xlab = "Time", ylab = "High value", main = "Time Series", col = "red")

adf.test(data$Close, alternative = "stationary", k = 0)

i_tsdiff1 <- diff(ts_obj, differences=1)
plot.ts(i_tsdiff1, col = "red")

acf(i_tsdiff1, lag.max=60)

acf(i_tsdiff1, lag.max=60, plot=FALSE) 

pacf(i_tsdiff1, lag.max=60)             

pacf(i_tsdiff1, lag.max=60, plot=FALSE) 

i_tsarima <- auto.arima(ts_obj, max.p = 3, max.q = 3, max.d = 3)
i_tsarima

i_tsforecasts <- forecast(i_tsarima, h = 60)
plot(i_tsforecasts, col = "red")

plot.ts(i_tsforecasts$residuals)          

ggplot(data.frame(residuals = i_tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram


