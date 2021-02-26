currency <- USD_INR_Historical_Data ### 9 years data from 11/01/2010 (2370 total obs)
str(currency)
class(currency)
setDT(currency)
head(currency)
##erate[,Date:=as.Date(Date,'%d-%b-%y')]
currency[,Date:=as.Date(Date,'%d-%b-%y')]
tscurr <- ts(currency$Price, start = c(2010,11), frequency = 260)
View(tscurr)
plot(tscurr)
tscurr

train <- window(tscurr, start = c(2010,11),end = c(2017,10)) ##training data from 11/1/2010 till 10/20/2017

## 3 mehtods ##
tail(train)
autoplot(tscurr) +
  autolayer(meanf(train,h=780), series="MEAN", PI=FALSE) +
  autolayer(naive(train,h=780), series="NAIVE", PI=FALSE) +
  autolayer(rwf(train,h=780,drift = TRUE), series="DRIFT", PI=FALSE) +
  ggtitle("Forecast INR rate for next 780 days(3 years)") +
  xlab("Time") + ylab("USDINR rate")

test <- window(tscurr, start = c(2017,10))###### test data from 10/23/2017 until 11/29/2019
View(test)
ggAcf(train)#### on my training data

tail(test)

meantscurr <- meanf(train,h=780)
naivetscurr <- naive(train,h=780)
drifttscurr <- rwf(train,h=780,drift = TRUE)

meantscurr ##
naivetscurr ## last obs 65.040
drifttscurr ##

summary(meantscurr)
summary(naivetscurr)
summary(drifttscurr)


accuracy(meantscurr,test)######
accuracy(naivetscurr,test)#####  
accuracy(drifttscurr,test)###### comparing mean, naive and drift
## for both training and test data, drift provides the best estimate



##4 decomposition using stl##################################

fitdecomp <- stl(tscurr,s.window = 5)
fitdecomp
plot(tscurr) ## original time series
lines(fitdecomp$time.series[,2],col="blue",ylab="Trend") ## Blue line is the trend comp
summary(fitdecomp)

plot(tscurr,col='grey',xlab="Time",ylab="USDINR Rate",main='Seasonally adjusted data with original series')
lines(seasadj(fitdecomp),col="red",ylab="Seasonally adjusted")### Trend + error
lines(fitdecomp$time.series[,2],col="blue",ylab="Trend")## only Trend line

# 4 Naive forecasts of seasonally adjusted data
fitsadj <- stl(tscurr, t.window=13, s.window="periodic", robust=TRUE)
sadj <- seasadj(fitsadj)
#fit %>% seasadj() %>% naive() %>% autoplot() + ylab("New orders index") + ggtitle("Naive forecasts of seasonally adjusted data")
plot(naive(sadj,h=44), xlab="Time",main="Naive forecasts of seasonally adjusted data")
naive(sadj,h=44)#### 71.78824
summary(fitsadj)

# 5 SES ##########

## default alpha##
train <- window(tscurr, start = c(2010,11),end = c(2017,10)) ##training data from 11/1/2010 till 10/20/2017
fitses <-ses(train, h=780) ##Simple Exponential Smoothing, suitable for no trend + linear
fitses
plot(fitses,xlab="Time",ylab="INR per USD") ## ## forecast
fitses$model ### alpha = 0.9999 and l = 44.4691
# AIC     AICc      BIC
# 9062.529 9062.543 9079.049

summary(fitses)

# Error measures:
#   ME        RMSE       MAE        MPE      MAPE       MASE       ACF1
# 0.01130381 0.282172 0.1914352 0.01973781 0.3293246 0.04698499 0.03317511

## with alpha = 0.5 ##
plot(tscurr,col='grey',xlab="Time",ylab="USDINR Rate",main='Forecasts from Simple exponential smoothing')
fitses1 <-ses(train, alpha=0.5, initial="simple", h=780)
lines(fitted(fitses1), col="blue") ## plot the model
plot(fitses1,xlab="Time",ylab="USDINR Rate") ## ## forecast
fitses1$model ## alpha = 0.5, l= 44.47
summary(fitses1)

# Error measures:
#       ME      RMSE      MAE        MPE      MAPE       MASE      ACF1
# 0.02257764 0.3228092 0.217963 0.03945011 0.3758764 0.05349585 0.4834031

## 6 Holt's linear
plot(train,xlab="Time",ylab="USD/INR Exchange Rate ",main='Holt linear model')
fitholt <-holt(train, h=780)## Holt's linear
fitholt

fitr <- fitholt$residuals
fitr
ggAcf(fitr)############# indicates that the correlation between residuals are statistically zero

#fcholt$residuals[1]

plot(train,xlab="Time",ylab="USD/INR Exchange Rate ",main='Holt linear model')
fcholt=forecast(fitholt,5)
lines(fcholt$mean,col='blue') ## forecast
fcholt$upper
fcholt$lower
plot(fcholt) ## forecast with CI

plot(train,xlab="Time",ylab="USD/INR Exchange Rate ",main='Holt linear model')
lines(fitted(fitholt),col='red') ## plot the model
fitholt$model ## alpha = 0.9999 and beta  = 1e-04
summary(fitholt)
# AIC     AICc      BIC
# 9069.610 9069.643 9097.143

# Error measures:
#       ME     RMSE       MAE          MPE      MAPE       MASE       ACF1
# -0.001503539 0.282411 0.1924859 -0.002151882 0.3313205 0.04724287 0.03254038

autoplot(tscurr) +
  autolayer(fcholt, series="Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("USD/INR Exchange Rate ") +
  guides(colour=guide_legend(title="Forecast"))


##7 Forecasting with ARIMA ####################

# 1 - KPSS Test for Level Stationarity

library(urca)

Test=ur.kpss(tscurr)
summary(Test)

# Value of test-statistic is: 22.3652
#
# Critical value for a significance level of:
#   10pct  5pct 2.5pct  1pct
# critical values 0.347 0.463  0.574 0.739

# Since F-Statistic (22.3652) > critical values at all significance,
# hence, we reject H0 (null hypethesis) i.e. non-stationary,
# hence differencing is required



ndiffs(tscurr) ## to determine how many order differencing is required
train1 <- diff(tscurr)
Test1 <- ur.kpss(train1)
summary(Test1)
# Value of test-statistic is: 0.1102
#
# Critical value for a significance level of:
#   10pct  5pct 2.5pct  1pct
# critical values 0.347 0.463  0.574 0.739

# Now F-statistic (0.1102) is less than all significance level,
# hence we fail to reject H0, i.e. dataset is stationary now

Acf(tscurr)

arimamodel <- auto.arima(tscurr,seasonal = FALSE)
summary(arimamodel) ## ARIMA(5,1,0) with drift

# sigma^2 estimated as 0.07568:  log likelihood=-300.96
# AIC=615.92   AICc=615.97   BIC=656.31
# Training set error measures:
#     ME      RMSE       MAE        MPE     MAPE      MASE         ACF1
# 0.01110614 0.2749137 0.1917473 0.01849779 0.317764 0.9986467 -0.002575817

plot(train,xlab="Time",ylab="USD/INR Exchange Rate ",main='Arima model')
model1 <- arima(tscurr,order=c(5,1,0))
model1
summary(model1)
fcasta <- forecast(model1,h=780)
plot(fcasta)
model1
