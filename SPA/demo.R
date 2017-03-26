rm(list=ls())
library(moments)
source('bsds.R')
D <- read.table('gdp-us-grate.csv')

dates <- as.Date(as.character(D[,1]),'%Y-%m-%d')

H <- 6
N <- nrow(D)-6
y <- D[1:N,2]

# fit different models - random walk to be used as benchmark
rw     <- arima(y, order=c(0,0,0))
ar1    <- arima(y,order=c(1,0,0))
ma1    <- arima(y,order=c(0,0,1))
ma2    <- arima(y,order=c(0,0,2))
arma11 <- arima(y,order=c(1,0,1))

# construct squared residuals to be used as losses
rw_res     <- as.numeric((rw$residuals)^2)
ar1_res    <- as.numeric((ar1$residuals)^2)
ma1_res    <- as.numeric((ma1$residuals)^2)
ma2_res    <- as.numeric((ma2$residuals)^2)
arma11_res <- as.numeric((arma11$residuals)^2)

df <- data.frame(bench = rw_res, ar1 = ar1_res, ma1 = ma1_res, ma2 = ma2_res, arma11 = arma11_res)

write.csv(df, 'data.csv')

df <- read.csv('data.csv')

bench <- as.matrix(df[,2])
models <- as.matrix(df[,3:ncol(df), drop=TRUE])
t <- nrow(bench) # no of observations
k <- ncol(models) # no of models
B <- 1000 # no of bootstrap samples
w <- 8 # size of block

results <- spa(bench, models, B, w, type="STUDENTIZED", boot="BLOCK")
print(results)

# Hansen's SPA p-value is given by c < 0.05 which indicates that the benchmark model is not the best
# model for forecasting the time series.