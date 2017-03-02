library(readr)
library(tseries)
library(lmtest)
library(sandwich)
library(TSA)
data <- as.data.frame(read_csv("~/Desktop/forecast-competition/fe-forecast/forecast-competition-training.csv"))

# Plot TARGET variable
plot(data[,"TIME"],data[,"TARGET"], col="red",type="l")

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
AIC(arma21)
(-2/length(data[,"TARGET"])*(arma21$loglik) + 2/length(data[,"TARGET"])*2)*length(data[,"TARGET"])

# ARMA(2,2) with no predictors
arma22 <- arima(data[,"TARGET"], order=c(2,0,2))
arma22$aic

# ARMA(1,2) with no predictors
arma12 <- arima(data[,"TARGET"], order=c(1,0,2))
arma12$aic

# Study of volatility clustering
# Plot absolute values of the TARGET variable
plot(data[,"TIME"],abs(data[,"TARGET"]), col="red",type="l")

# Sample autocorrelation of obsolute returns
acf(abs(data[,"TARGET"]), ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2') # There seems to be some dependence!

# Sample autocorrelation of obsolute returns
acf(data[,"TARGET"]^2, ylim=c(-0.1,1), lwd=5, xlim=c(0,25), col='darkorange2') # There seems to be some dependence!

# ARCH-LM test (check whether there is statistical significance of volatility clustering)
# For 5 lagged square returns
y <- data[,"TARGET"][6:length(data[,"TARGET"])]**2
n <- length(y)
X <- cbind(data[,"TARGET"][5:(length(data[,"TARGET"])-1)]**2, data[,"TARGET"][4:(length(data[,"TARGET"])-2)]**2,
          data[,"TARGET"][3:(length(data[,"TARGET"])-3)]**2, data[,"TARGET"][2:(length(data[,"TARGET"])-4)]**2,
          data[,"TARGET"][1:(length(data[,"TARGET"])-5)]**2)
archlm <- lm(y ~ X)
archlm.statistic <- n*summary(archlm)$r.squared
1-pchisq(archlm.statistic,5) # Reject null hypothesis of absence of volatility clustering

# For 4 lagged square returns
y <- data[,"TARGET"][5:length(data[,"TARGET"])]**2
n <- length(y)
X <- cbind(data[,"TARGET"][4:(length(data[,"TARGET"])-1)]**2,
           data[,"TARGET"][3:(length(data[,"TARGET"])-2)]**2,
           data[,"TARGET"][2:(length(data[,"TARGET"])-3)]**2,
           data[,"TARGET"][1:(length(data[,"TARGET"])-4)]**2)
archlm <- lm(y ~ X)
archlm.statistic <- n*summary(archlm)$r.squared
1-pchisq(archlm.statistic,4) # Reject null hypothesis of absence of volatility clustering

# For 3 lagged square returns
y <- data[,"TARGET"][4:length(data[,"TARGET"])]**2
n <- length(y)
X <- cbind(data[,"TARGET"][3:(length(data[,"TARGET"])-1)]**2,
           data[,"TARGET"][2:(length(data[,"TARGET"])-2)]**2,
           data[,"TARGET"][1:(length(data[,"TARGET"])-3)]**2)
archlm <- lm(y ~ X)
archlm.statistic <- n*summary(archlm)$r.squared
1-pchisq(archlm.statistic,3) # Reject null hypothesis of absence of volatility clustering

# For 2 lagged square returns
y <- data[,"TARGET"][3:length(data[,"TARGET"])]**2
n <- length(y)
X <- cbind(data[,"TARGET"][2:(length(data[,"TARGET"])-1)]**2,
           data[,"TARGET"][1:(length(data[,"TARGET"])-2)]**2)
archlm <- lm(y ~ X)
archlm.statistic <- n*summary(archlm)$r.squared
1-pchisq(archlm.statistic,2) # Reject null hypothesis of absence of volatility clustering

# For 1 lagged square returns
y <- data[,"TARGET"][2:length(data[,"TARGET"])]**2
n <- length(y)
X <- cbind(data[,"TARGET"][1:(length(data[,"TARGET"])-1)]**2)
archlm <- lm(y ~ X)
archlm.statistic <- n*summary(archlm)$r.squared
1-pchisq(archlm.statistic,1) # Reject null hypothesis of absence of volatility clustering

# ARCH(1) with no predictors
arch11 <- garch(data[,"TARGET"], order = c(0,1))
AIC(arch11)

# GARCH(1,1) with no predictors
garch11 <- garch(data[,"TARGET"], order = c(1,1))
AIC(garch11)

# Compute principal components
x <- prcomp(data[,3:ncol(data)], center=T, scale.=T)
principal_components <- x$x

number_training <- 400
mean_error <- c()
for (j in 1:(nrow(data)-number_training)){
  errors <- c()
  x <- prcomp(data[1:(number_training + j - 1),3:ncol(data)], center=T, scale.=T)
  principal_components <- x$x
  for (i in 1:ncol(principal_components)){
    model <- lm(data[2:(number_training + j - 1),"TARGET"] ~ data[1:(number_training + j - 2),"TARGET"] + principal_components[1:(number_training + j - 2),1:i])
    errors[i] <- (data[number_training + j,"TARGET"] - predict(model, n.ahead=1))^2
  }
  mean_error[j] <- mean(errors)
}










number_training <- 400
mean_error <- c()
for (i in 1:49){
  for (j in 1:(nrow(data)-number_training)){
    errors <- c()
    x <- prcomp(data[1:(number_training + j),3:ncol(data)], center=T, scale.=T)
    principal_components <- x$x
    model <- lm(data[2:(number_training + j - 1),"TARGET"] ~ data[1:(number_training + j - 2),"TARGET"] + principal_components[1:(nrow(principal_components)-2),1:i])
    errors[j] <- (data[(number_training + j),"TARGET"] - predict(model, newdata=as.data.frame(principal_components[(number_training + j),1:i])))^2
  }
  mean_error[i] <- mean(errors)
}
which.min(mean_error)




