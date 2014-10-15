library("forecast")

infer.arima <- function(predictor, externals){
  # Validate inputs
  size <- length(predictor)
  
  if (!is.matrix(externals)) {
    stop("Invalid argument: 'externals' must be a matrix.")
  }
  
  if (nrow(externals) != size) {
    stop("Invalid argument: The size of the predictor and size of externals does not match.")
  }
  
  # Call auto.arima on predictor
  arima.model = auto.arima(predictor)
  best.p = arima.model$arma[1]
  best.q = arima.model$arma[2]
  best.d = arima.model$arma[length(arima.model$arma) - 1]
  
  # Get ARIMA model for predictor
  fit <- Arima(x, order = c(best.p, best.q, best.d))
  predictor.residuals <- residuals(Arima(x, model=fit))
  
  # Filter on all externals based on the arima model
  
  externals.residuals <- matrix(0, size, ncol(externals))
  
  for (i in 1 : ncol(externals)) {
    externals.residuals[,i] <- residuals(Arima(externals[,i], model=fit))
  }
  
  # Fit multivariable linear regression model
  external.res.df <- data.frame(externals.residuals)
  lin.fit <- lm(predictor.residuals ~ ., external.res.df)
  
  infer.arima.result <- list(arima.fit = fit, residuals.fit = lin.fit)
  class(infer.arima.result) = "infer.arima"
  infer.arima.result
}

predict.infer.arima <- function(object, n.ahead, externals) {
  if (class(df) != "infer.arima") {
    stop("The model is of incorrect type.")
  }
  
  if (!is.matrix(externals)) {
    stop("Invalid argument: 'externals' must be a matrix.")
  }
  
  if (nrow(externals) != n.ahead) {
    stop("Invalid argument: The size of the prediction interval and size of externals does not match.")
  }
  
  arima.prediction <- predict(object$arima.fit, n.ahead)
  residuals.prediction <- predict(object$residuals.fit, data.frame(externals))
  
  return(arima.prediction$pred + residuals.prediction)
}