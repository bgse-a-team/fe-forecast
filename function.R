predictor.one <- function(data){
  library(glmnet)
  T <- nrow(data)
  fit <- glmnet(as.matrix(cbind(data[2:T,2:50], data[1:(T-1),1])), as.matrix(data[2:T,1]), alpha = 1, family = "mgaussian")
  pred<-as.numeric(predict.glmnet(fit, newx =as.matrix(data[T,1:50]), s=0, type="link"))
  return( pred )
}