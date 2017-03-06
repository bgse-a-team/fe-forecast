predictor.one <- function(data){
  x <- prcomp(data[1:nrow(data),2:ncol(data)], center=T, scale.=T)
  principal_components <- x$x[,1:4]
  x1 <- data[1:(nrow(data) - 1),"TARGET"]
  data_new <- as.data.frame(cbind(data[2:nrow(data),"TARGET"],x1,principal_components[1:(nrow(data)-1),, drop=F]))
  colnames(data_new) <- c("y","x1",colnames(principal_components[1:(nrow(data)-1),, drop=F]))
  model <- lm(y ~ ., data=data_new)
  newdata <- cbind(data[nrow(data),"TARGET"],principal_components[nrow(principal_components),, drop=F])
  colnames(newdata)[1] <- "x1"
  newdata <- as.data.frame(newdata)
  pred<-predict(model, newdata=newdata)
  return( pred )
}
