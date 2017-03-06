predictor.zeno <- function(y){
  x <- prcomp(y[1:nrow(y),2:ncol(y)], center=T, scale.=T)
  principal_components <- x$x[,1:4]
  x1 <- y[1:(nrow(y) - 1),1]
  y_new <- as.data.frame(cbind(y[2:nrow(y),1],x1,principal_components[1:(nrow(y)-1),, drop=F]))
  colnames(y_new) <- c("y","x1",colnames(principal_components[1:(nrow(y)-1),, drop=F]))
  model <- lm(y ~ ., data=y_new)
  newy <- cbind(y[nrow(y),1],principal_components[nrow(principal_components),, drop=F])
  colnames(newy)[1] <- "x1"
  newy <- as.data.frame(newy)
  pred<-predict(model, newdata=newy)
  return( as.numeric(pred) )
}