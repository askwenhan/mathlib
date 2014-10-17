library("forecast")

infer.arima <- function(predictor, externals){
  # Validate inputs
  size <- length(predictor)
  
  if (size <= 0) {
    stop("The length of the predictor must be positive.")
  }
  
  if (!is.matrix(externals)) {
    stop("Invalid argument: 'externals' must be a matrix.")
  }
  
  if (nrow(externals) != size) {
    stop("Invalid argument: The size of the predictor and size of externals does not match.")
  }
  
  # Call auto.arima on predictor
  arima.model = auto.arima(predictor, xreg = externals)
  result <- list(model = arima.model)
  class(result) = "infer.arima"
  result
}

infer.predict <- function(object, n.ahead, externals) {
  if (class(object) != "infer.arima") {
    stop("The model is of incorrect type.")
  }
  
  if (n.ahead <= 0) {
    stop("Invalid argument: 'n.ahead' must be positive.")
  }
  
  if (!is.matrix(externals)) {
    stop("Invalid argument: 'externals' must be a matrix.")
  }
  
  if (nrow(externals) != n.ahead) {
    stop("Invalid argument: The size of the prediction interval and size of externals does not match.")
  }
  
  prediction <- forecast(object$model, h = n.ahead, xreg = externals)
  pred <- prediction$mean
  lower80 <- prediction$lower[,1]
  lower95 <- prediction$lower[,2]
  upper80 <- prediction$upper[,1]
  upper95 <- prediction$upper[,2]
  arima.infer.prediction <- list(pred = pred, lower80 = lower80, lower95 = lower95, upper80 = upper80, upper95 = upper95)
  return(arima.infer.prediction)
}