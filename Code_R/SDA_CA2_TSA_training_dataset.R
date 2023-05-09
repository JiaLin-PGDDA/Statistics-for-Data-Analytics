# Time Series Analysis
# read csv file:
# 1. get user's home directory
home = setwd(Sys.getenv("HOME"));
home
# 2. construct path to file
fpath = file.path(home, "Desktop", "SAP_CA2_040123/datasets/Departure.csv")
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
fit_stl_ts <- stl(ts_departures, s.window = "period")
plot(fit_stl_ts)
fit_stl <- stl(log_departures, s.window = "period")
plot(fit_stl)
fit_stl$time.series

# Forecast
ts_training <- window(ts_departures, start = c(2010,1), end = c(2020,12))
ts_test <- window(ts_departures, start = c(2021,1))
ts_training
ts_test
plot(ts_training)

library(forecast)
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
fit_ets <- ets(ts_training, model="ZZZ")
fit_ets
fc_fit_ets <- forecast(fit_ets, 6)
fc_fit_ets  
round(accuracy(fit_ets),2)
plot(fc_fit_ets)

# Evaluation by comparing AIC
fit_single <- ets(ts_training, model = "ANN")
fit_single 
fit_holt <- ets(ts_training, model = "AAN")
fit_holt 
#Holt-Winters model
fit_hw_addi <- hw(ts_training, damped=TRUE, seasonal="additive")
fit_hw_mult <- hw(ts_training, damped=TRUE, seasonal="multiplicative")
autoplot(ts_departures)+
  autolayer(fit_hw_addi, 
            series = "additive", 
            PI=FALSE)+
  autolayer(fit_hw_mult,
            series = "multiplicative",
            PI=FALSE)+
  ggtitle("Forecasts departures from Ireland airport (Holt-Winters Model)")+
  xlab("Year")+ylab("Departures")+
  guides(colour=guide_legend(title = "Model"))

round(accuracy(fit_hw_addi),2)
round(accuracy(fit_hw_mult),2)
##################################################################
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
adf.test(ts_training)
# p-value = 0.7658 > 0.05 shows it is not significant
# ts_training is not stationarity.
plot(ts_training)
# the seasonal differences remove the seaonality and trend
# the function ndiffs(ts) is used to check the order of differencing
# or determine the value d
ndiffs(ts_training) # result is 1
# differencing is an approach to making a non-stationary 
# time series stationary
stat_training <- diff(ts_training, 1)
adf.test(stat_training)
# p-value = 0.01 < 0.05 is significant
# stat_training is stationary
plot(ts_training)
plot(stat_training)
# a correlogram
Acf(stat_training)
# a partial autocorrelation at lag k
Pacf(stat_training)
# we use other tests to determine whether the errors in a time series
# model are serially correlated
# whether the autocorrelations of the error term are significantly
# different from zero
# the goal is to identify the parameters p and q

# automated ARIMA fitting on ts_training
fit_auto_ts_training <- auto.arima(ts_training)
fit_auto_ts_training 

fit_auto_stat_training <- auto.arima(stat_training)
fit_auto_stat_training 
#evaluating model fit
qqnorm(fit_auto_ts_training$residuals)
qqline(fit_auto_ts_training$residuals)
#hist(fit_auto_ts_training$residuals, prob=TRUE)
#lines(density(fit_auto_ts_training$residuals), col = 4, lwd = 2)
# the result shows the residuals normally and independently distributed
Box.test(fit_auto_ts_training$residuals, type = "Ljung-Box")
# the result is not significant, the autocorrelations don't
# differ from zero.
# the ARIMA model appears to fit the data well

round(accuracy(fit_auto_ts_training),2)
fc_auto_ts_training <- forecast(fit_auto_ts_training, h=6)
fc_auto_ts_training
plot(fc_auto_ts_training)

fc_auto_ts_longer <- forecast(fit_auto_ts_training, h=24)
fc_auto_ts_longer
plot(fc_auto_ts_longer)
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
round(accuracy(lt_model),2)

log_lt_model <- lm(log(Departures)~Months, data =df_departures)
summary(log_lt_model)
durbinWatsonTest(log_lt_model)
round(accuracy(lt_model),2)
