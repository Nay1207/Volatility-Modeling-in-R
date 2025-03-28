## Volatility-Modeling-in-R
#libraries required
library(quantmod)
library(urca)
library(PerformanceAnalytics)
library(moments)
library(tseries)
library(forecast)
library(FinTS)
library(rugarch)
getSymbols("^FTSE",from = "2020-01-01", to = Sys.Date())
sum(is.na(FTSE))
FTSE <- na.omit(FTSE)
FTSE <- Ad(FTSE)
plot.zoo(FTSE, main = "FTSE Daily prices",
        xlab = "Time",
        ylab = "Prices",
        lwd = 2,
        col = "blue")
#Stationarity test
adf_FTSE = ur.df(FTSE, type = "drift", selectlags = "AIC")
summary(adf_FTSE)
#log returns
ret.FTSE = Return.calculate(FTSE, method = "log")[-1]
plot.zoo(ret.FTSE, lwd = 2, col = "blue")
adf2_FTSE = ur.df(ret.FTSE, type = "drift", selectlags = "AIC")
summary(adf2_FTSE)
#Descriptive stats
summary(ret.FTSE)
skewness(ret.FTSE)
kurtosis(ret.FTSE)
sd(ret.FTSE)
#checking normality
hist(ret.FTSE, prob = T, breaks = 100,
      main = "FTSE 100 Log Returns Histogram",
      col = "cornflowerblue")
mu<-mean(ret.FTSE)
sigma<-sd(ret.FTSE)
x<-seq(min(ret.FTSE),max(ret.FTSE),length=80)
y<-dnorm(x,mu,sigma)
lines(x,y,lwd=2,col="red")
jarque.bera.test(ret.FTSE)
acf(ret.FTSE)
pacf(ret.FTSE)
k = trunc((length(ret.FTSE)-1)^(1/3))
Box.test(ret.FTSE, lag = 10, type = "Ljung-Box", fitdf = 0)
#Fitting ARIMA model
fit <- auto.arima(ret.FTSE, ic = "aic", stepwise = FALSE, approximation = FALSE)
summary(fit)
checkresiduals(fit)
residuals = residuals(fit)
Box.test(residuals, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals^2, lag = 10, type = "Ljung-Box", fitdf = 0)
ArchTest(ret.FTSE)
##Model using ARIMA-GARCH model
#Checking for the lags for GARCH
acf(residuals^2, main="ACF of Squared Residuals")
pacf(residuals^2, main="PACF of Squared Residuals")
# Define the ARIMA-GARCH model specification
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
#Model selection using information critarion
model.list = list("garch(1,1)" = fit.garch.1,
                  "garch(2,1)" = fit.garch.2,
                  "garch(1,2)" = fit.garch.3,
                  "garch(2,2)" = fit.garch.4)
model.mat = sapply(model.list, infocriteria)
rownames(model.mat) = rownames(infocriteria(fit.garch.1))
model.mat
#Dignostic checking
residuals.1 <- residuals(fit.garch.1, standardize = TRUE)
residuals.2 <- residuals(fit.garch.3, standardize = TRUE)
residuals.3 <- residuals(fit.garch.1)
residuals.4 <- residuals(fit.garch.3)
Box.test(residuals.1, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.1^2, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.2, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.2^2, lag = 10, type = "Ljung-Box", fitdf = 0)
#Checking autocorrelation in raw residuals
Box.test(residuals.3, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.3^2, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.4, lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals.4^2, lag = 10, type = "Ljung-Box", fitdf = 0)#Raw residuals still show autocorrelation, we have to revisit ARIMA
#forecasting garch(1,2)
forecast <- ugarchforecast(fit.garch.3, n.ahead = 10)
print(forecast)

