team.one <- function(y) {
  target <- y[,1]
  mdl <- arima(target, order=c(1,0,1))
  pred <- predict(mdl,n.ahead=1)
  forecast <- pred$pred
  return(forecast)
}

MSE <- 0

for (t in 500:749) {
  f <- team.one(data[1:t, ])
  MSE <- MSE + (data[t+1,1] - f)**2
}



data <- as.data.frame(read.csv("~/projects/fe-forecast/orig_data.csv"))
predictor.zeno(y)
team.one <- function(y) {
  target <- y[,1]
  mdl <- arima(target, order=c(1,0,1))
  pred <- predict(mdl,n.ahead=1)
  forecast <- pred$pred
  return(as.numeric(forecast))
}

MSE <- 0

for (t in c(350:499)) {
  f <- predictor.zeno(data[1:t, ])
  MSE <- MSE + (data[t+1,1] - f)**2
}
MSE <- MSE/(499-350)
MSE
