# Time Series Analysis
# read csv file:
# 1. get user's home directory
home = setwd(Sys.getenv("HOME"));
home
# 2. construct path to file
fpath = file.path(home, "SAP_CA2_040123", "datasets/Departure.csv")
# read file without header
df_departures <- read.csv(file=fpath, header = TRUE)
# delete the header and rename the column names
names(df_departures)<-NULL
colnames(df_departures)<-c("Months", "Departures")

# create a new dataframe
var1 <- seq(as.Date("2010-01-01"), as.Date("2022-09-01"), by="months")
var2 <- df_departures$Departures
var2 <- as.numeric(as.character(var2))
df_departures <- data.frame(c(var1), c(var2))
colnames(df_departures)<-c("Months", "Departures")
head(df_departures)

# get a vector a numbers
departures <- c(var2)
# Load fpp2 package to do time series analysis
library(fpp2)
# create a time series object of departures of all data
ts_departures <- ts(departures, start = c(2010,1), end = c(2022, 9), frequency=12)
ts_departures
# draw a raw plot of ts_departures
plot(ts_departures)
# from the raw plot it is not a stationary ts
# we can see the trend from 2010 to 2020
# a drop down happened at the beginning of 2020
# from 2010 to 2020 there was a seasonal pattern
monthplot(ts_departures)
ggseasonplot(ts_departures, col=rainbow(12), year.labels=TRUE)

# seasonal decomposition using decompose() classic approaches
fit.decaddi<- decompose(ts_departures, type = "additive")
fit.decaddi
plot(fit.decaddi)

fit.decmult <- decompose(ts_departures, type = "multiplicative")
fit.decmult
plot(fit.decmult)
# classic methods have drawbacks
# seasonal decompostion using stl()
# Seasonal and Trend decomposition using Loess, STL
log_departures <- log(ts_departures)
plot(ts_departures)
plot(log_departures, ylab = "log(ts_departures)")
fit_stl <- stl(log_departures, s.window = "period")
plot(fit_stl)
fit_stl$time.series

# Forecast
ts_training <- window(ts_departures, start = 2010, end = 2021)
ts_test <- window(ts_departures, start = 2021)

library(forecast)
#############################################################
# simple forecasting methods                                #
# 1. Average method using function meanf(y, h)              #
# 2. Naive method (Random Walk) using function naive(y, h). #
# 3. Seasonal naive method using function snaive(y, h).     #
# 4. Drift method using function rwf(y, h, drift= TRUE).    #
#############################################################
autoplot(ts_departures) +
  autolayer(meanf(ts_departures, h=12),
            series="Mean", PI=FALSE) +
  autolayer(naive(ts_departures, h=12),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(ts_departures, h=12),
            series="Seasonal naïve", PI=FALSE) +
  autolayer(rwf(ts_departures, h= 12, drift = TRUE),
            series="Drift", PI=FALSE)+
  ggtitle("Forecasts for departures from Ireland airport (Simple Forecast Methods) ") +
  xlab("Year") + ylab("Departures") +
  guides(colour=guide_legend(title="Forecast"))
######################################################################
# 5. Linear trend models y_t = b_0 +b_1+e_t, t= 1, 2, ..., T         #
# 6. Log-linear trend models ln(y_t) = b_0 +b_1+e_t, t= 1, 2, ..., T #
# Durbin-Watson test on residuals to determine if use trend models.  #
# 7. weighted moving averages                                        #
# load car package to do Durbin-Watson test                          #                                                                #
######################################################################
library(car)     
lt_model <- lm(Departures~Months, data = df_departures)
summary(lt_model)
durbinWatsonTest(lt_model)

log_lt_model <- lm(log(Departures)~Months, data =df_departures)
summary(log_lt_model)
durbinWatsonTest(log_lt_model)
############################################################
# Exponential Smoothing Models                             #
# weighted averages of past observations                   #
# 1. A simple exponential smoothing model(Single),         #
# only level, no trend or seasonal                         #
# 2. A double exponential smoothing model(Holt),           #
# level and trend                                          #
# 3. A triple exponential smoothing model(Holt-Winters),   #
# level, trend and seasonal                                #
# ets model Z for automatically selected                   #
############################################################
fit_ets <- ets(ts_departures, model="ZZZ")
fit_ets
fc_fit_ets <- forecast(fit_ets, 6)
fc_fit_ets
round(accuracy(fit_ets),2)

plot(fc_fit_ets)

# Evaluation by comparing AIC
fit_single <- ets(ts_departures, model = "ANN")
fit_single  # AIC 2327.001
fit_holt <- ets(ts_departures, model = "AAN")
fit_holt     # AIC 2301.351
fit_hw <- ets(ts_departures, model = "AAA")
fit_hw       # AIC 2230.789 
             # has the smallest AIC/AICc/BIC

#Holt-Winters model
fit_hw_addi <- hw(ts_departures, seasonal="additive")
fit_hw_mult <- hw(ts_departures, seasonal = "multiplicative")
autoplot(ts_departures)+
  autolayer(fit_hw_addi, 
            series = "additive", 
            PI=FALSE)+
  autolayer(fit_hw_mult, 
            series = "multiplicative", 
            PI=FALSE)+
  ggtitle("Forecasts departures from Ireland airport (Holt-Winters Model)")+
  xlab("Year")+ylab("Departures")+
  guides(colour=guide_legend(title = "Forecast"))
# Compare RMSE, the lower of RMSE, the better of model fits 
# the additive model is fit more in this model

round(accuracy(fit_hw_addi),2)# RMSE = 109.57
round(accuracy(fit_hw_mult),2)# RMSE = 127.18

# forecast departures for Oct 2022 to Mar 2023
fc_fit_hw_addi <- forecast(fit_hw_addi, h=6)
fc_fit_hw_addi
plot(fc_fit_hw_addi)

###################################################################
# ARIMA/SARIMA
# ARIMA models aim to describe the autocorrelations in the data 
# ARIMA models are designed to fit stationary time series (or time
# series that can be made stationary)
# Time series with trends, or with seasonality, are not stationary.
# We can use a statistical procedure called 
# the Augmented Dickey-Fuller (ADF) test to evaluate the assumption 
# of stationarity.
###################################################################
library(tseries)
adf.test(ts_departures)
# p-value = 0.4725 > 0.05 shows it is not significant
# ts_departures is not stationarity.
# the logarithms stabilise the variance
log_departures
# the seasonal differences remove the seaonality and trend
# the function ndiffs(ts) is used to check the order of differencing
# or determine the value d
ndiffs(log_departures) # result is 1
# differencing is an approach to making a non-stationary 
# time series stationary
stat_departures <- diff(log_departures, 1)

adf.test(stat_departures)
# p-value = 0.01 < 0.05 is significant
# stat_departures is stationary

#par(mfrow=c(3,1))
plot(ts_departures)
plot(log_departures)
plot(stat_departures)

# we use other tests to determine whether the errors in a time series
# model are serially correlated
# whether the autocorrelations of the error term are significantly
# different from zero

# the goal is to identify the parameters p and q
# 1. the classic auto-regression model has an ACF that slowly 
#    approaches 0 and PACF that spikes at lag p. 
#    p = the number of spikes
# 2. the classic moving average model has an ACF that spikes on 
#    the first q lags and a PACF that declines slowly.
#    q = the number of spikes
# 3. 
#par(mfrow=c(1,2))
# a correlogram
Acf(stat_departures)# indicates MA(1) one largest lag at 3
# a partial autocorrelation at lag k
Pacf(stat_departures)# indicates AR(1) one largest lag at 3

# fitting an ARIMA model
fit_arima <- arima(stat_departures, order=c(1,1,1))
fit_arima # aic = 157.21

fit_arima1 <- arima(stat_departures, order=c(0,1,1))
fit_arima1 # aic = 159.91

fit_arima2 <- arima(stat_departures, order =c(4,1,3))
fit_arima2 # aic = 154.82

#evaluating model fit
qqnorm(fit_arima2$residuals)
qqline(fit_arima2$residuals)
# the result shows the residuals normally and independently distributed
Box.test(fit_arima2$residuals, type = "Ljung-Box")
# the result is not significant, the autocorrelations don't
# differ from zero.
# the ARIMA model appears to fit the data well

fc_arima2 <- forecast(fit_arima2, h=12)
plot(fc_arima2)
round(accuracy(fit_arima2),2)# RMSE = 0.38

# automated ARIMA fitting on ts_departures
fit_auto_ts <- auto.arima(ts_departures)
fit_auto_ts 
round(accuracy(fit_auto_ts),2)# RMSE = 104.27
fc_auto_ts <- forecast(fit_auto_ts, h=24)
fc_auto_ts
plot(fc_auto_ts)

# automated ARIMA fitting on log_departures
fit_auto_log <-auto.arima(log_departures)
fit_auto_log
round(accuracy(fit_auto_log),2)# RMSE = 0.39
fc_auto_log <- forecast(fit_auto_log, h=24)
fc_auto_log
plot(fc_auto_log)




