library(readr)
library(tseries)
library(lmtest)
library(sandwich)
data <- as.data.frame(read_csv("~/Desktop/forecast-competition/forecast-competition-training.csv"))

# Plot TARGET variable
plot(data[,"TIME"],data[,"TARGET"], col="red",type="l")

lines(data[,"TIME"],ma1$residuals + data[,"TARGET"], col="blue",type="l")
abline(h=0, lwd=2)

# Check if TARGET is statioary
adf.test(data[,"TARGET"]) # Evidence of stationarity

# Check normality of TARGET
jarque.bera.test(data[,"TARGET"]) # Evidence of normality

# Sample autocorrelation
acf(data[,"TARGET"], ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2') # Exponential decrease

# Sample partial autocorrelation
pacf(data[,"TARGET"], ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2') # Exponential decrease

# Linear model using one lag and no predictors
lm1 <- lm(data[2:nrow(data),"TARGET"] ~ data[1:(nrow(data)-1),"TARGET"])
coeftest(lm1,NeweyWest(lm1)) # Statistical significance
summary(lm1)$r.squared # R squared
Box.test(summary(lm1)$residuals, lag=22, type="Ljung-Box") # Reject the null hypothesis of independence of the residuals
acf(summary(lm1)$residuals, ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2')
jarque.bera.test(summary(lm1)$residuals) # Evidence of normality

# AR(1) with no predictors
ar1 <- arima(data[,"TARGET"], order=c(1,0,0))
ar1$aic
Box.test(ar1$residuals, lag=22, type="Ljung-Box") # Reject the null hypothesis of independence of the residuals
acf(ar1$residuals, ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2')
pacf(ar1$residuals, ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2')
jarque.bera.test(ar1$residuals) # Evidence of normality of the residuals

# MA(1) with no predictors
ma1 <- arima(data[,"TARGET"], order=c(0,0,1))
ma1$aic
Box.test(ma1$residuals, lag=22, type="Ljung-Box") # Reject the null hypothesis of independence of the residuals
acf(ma1$residuals, ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2')
pacf(ma1$residuals, ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2')
jarque.bera.test(ma1$residuals) # Evidence of normality of the residuals

# ARMA(1,1) with no predictors
arma11 <- arima(data[,"TARGET"], order=c(1,0,1))
arma11$aic
Box.test(arma11$residuals, lag=22, type="Ljung-Box") # Fail to reject the null hypothesis of independence of the residuals
acf(arma11$residuals, ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2')
pacf(arma11$residuals, ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2')
jarque.bera.test(arma11$residuals) # Evidence of normality of the residuals

# ARMA(2,1) with no predictors
arma21 <- arima(data[,"TARGET"], order=c(2,0,1))
arma21$aic

# ARMA(2,2) with no predictors
arma22 <- arima(data[,"TARGET"], order=c(2,0,2))
arma22$aic



