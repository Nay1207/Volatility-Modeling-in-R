## Volatility-Modeling-in-R
#libraries required libraries
library(quantmod)
library(urca)
library(PerformanceAnalytics)
library(moments)
library(tseries)
library(forecast)
library(FinTS)
library(rugarch)

#Retrieve FTSE 100 stock index data
getSymbols("^FTSE",from = "2020-01-01", to = Sys.Date())

#Check for missing values
sum(is.na(FTSE))

#Remove missing values
FTSE <- na.omit(FTSE)

#Extract adjusted closing prices
FTSE <- Ad(FTSE)

#Plot FTSE daily prices
plot.zoo(FTSE, main = "FTSE Daily prices",
        xlab = "Time",
        ylab = "Prices",
        lwd = 2,
        col = "blue")
        
#Stationarity check 
adf_FTSE = ur.df(FTSE, type = "drift", selectlags = "AIC")
summary(adf_FTSE)

#compute log returns
ret.FTSE = Return.calculate(FTSE, method = "log")[-1]

#plot log returns
plot.zoo(ret.FTSE, lwd = 2, col = "blue")

#Perform ADF test on log returns to check for stationarity
adf2_FTSE = ur.df(ret.FTSE, type = "drift", selectlags = "AIC")
summary(adf2_FTSE)

#Descriptive statistics
summary(ret.FTSE)
skewness(ret.FTSE)
kurtosis(ret.FTSE)
sd(ret.FTSE)

##checking normality of returns
#Histogram of log returns with normal distribution overly 
hist(ret.FTSE, prob = T, breaks = 100,
      main = "FTSE 100 Log Returns Histogram",
      col = "cornflowerblue")

#Overlay normal distribution curve
mu<-mean(ret.FTSE)
sigma<-sd(ret.FTSE)
x<-seq(min(ret.FTSE),max(ret.FTSE),length=80)
y<-dnorm(x,mu,sigma)
lines(x,y,lwd=2,col="red")

#Perform Jarque-Bera test for normality 
jarque.bera.test(ret.FTSE)

#Autocorrelation and partial autocorrelation of returns
acf(ret.FTSE)
pacf(ret.FTSE)

#Perform Ljung-Box test autocorrelation in returns
Box.test(ret.FTSE, lag = 10, type = "Ljung-Box", fitdf = 0)

#Fitting ARIMA model
fit <- auto.arima(ret.FTSE, ic = "aic", stepwise = FALSE, approximation = FALSE)
summary(fit)

#check residuals of the ARIMA model
checkresiduals(fit)

#Extract residuals
residuals = residuals(fit)

#Autocorrelation check on residuals
Box.test(residuals, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals^2, lag = 10, type = "Ljung-Box", fitdf = 0)

#check for ARCH effects 
ArchTest(ret.FTSE)

##ARIMA-GARCH model fitting
#Checking for the lags for GARCH
acf(residuals^2, main="ACF of Squared Residuals")
pacf(residuals^2, main="PACF of Squared Residuals")

# Define different ARIMA-GARCH model specification
spec1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 2), include.mean = FALSE),
  distribution.model = "std")
  
spec2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
  mean.model = list(armaOrder = c(3, 2), include.mean = FALSE),
  distribution.model = "std")
  
spec3 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(3, 2), include.mean = FALSE),
  distribution.model = "std")
  
spec4 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
  mean.model = list(armaOrder = c(3, 2), include.mean = FALSE),
  distribution.model = "std")
  
# Fit the ARIMA-GARCH model
fit.garch.1 <- ugarchfit(spec = spec1, data = ret.FTSE)
fit.garch.2 <- ugarchfit(spec = spec2, data = ret.FTSE)
fit.garch.3 <- ugarchfit(spec = spec3, data = ret.FTSE)
fit.garch.4 <- ugarchfit(spec = spec4, data = ret.FTSE)

##Model selection using information critarion
#model.list = list("garch(1,1)" = fit.garch.1,
                  "garch(2,1)" = fit.garch.2,
                  "garch(1,2)" = fit.garch.3,
                  "garch(2,2)" = fit.garch.4)
model.mat = sapply(model.list, infocriteria)
rownames(model.mat) = rownames(infocriteria(fit.garch.1))
model.mat

## Diagnostic checking of residuals
residuals.1 <- residuals(fit.garch.1, standardize = TRUE)
residuals.2 <- residuals(fit.garch.3, standardize = TRUE)
residuals.3 <- residuals(fit.garch.1)
residuals.4 <- residuals(fit.garch.3)

#Autocorrelation check on residuals
Box.test(residuals.1, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.1^2, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.2, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.2^2, lag = 10, type = "Ljung-Box", fitdf = 0)

#Checking autocorrelation in raw residuals
Box.test(residuals.3, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.3^2, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.4, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.4^2, lag = 10, type = "Ljung-Box", fitdf = 0)#Raw residuals still show autocorrelation, we have to revisit ARIMA

## forecasting GARCH(1,2) Model
forecast <- ugarchforecast(fit.garch.3, n.ahead = 10)
print(forecast)

