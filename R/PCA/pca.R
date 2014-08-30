# The R class for principal component analysis
# Constructor takes two arguments:
#  x:         the matrix of data
#  threshold: the threshold for 2-norm of eigenvalues
# Class members:
#  cov:          the covariance matrix of the data
#  eig.vec:      the eigenvalue decomposition matrix
#  eig.vec.inv:  the inverse of the eigenvalue decomposition matrix, should also be the transpose of the eigenvalue decomposition matrix
#  pr.eig.vec:   the principal part of the eigen value decomposition matrix. Its number of columns equals the number of principal components
#  ratio:        the true ratio of the principal components. Should be no less than the threshold.

pca = function(x, threshold) {
  if (!is.matrix(x)) {
    stop("Invalid argument: x must be a matrix.")
  }
  
  if (!is.numeric(threshold)) {
    stop("Invalid argument: threshold must be a numeric.")
  }
  
  if (threshold < 0 || threshold > 1) {
    stop("Invalid argument: threshold must be between 0 and 1.")
  }
  
  cov.matrix = cov(x)
  eig.decomposition = eigen(cov(x),TRUE,FALSE,FALSE)
  eig.values = eig.decomposition$values
  index = 0
  sum = 0
  sum.true = sum(eig.values * eig.values)
  sum.threshold = sum.true * threshold
  while (sum < threshold && index < length(eig.values)) {
    index = index + 1
    sum = sum + eig.values[index] * eig.values[index]
  }
  
  pca.result = list(cov = cov.matrix, eig.vec = eig.decomposition$vectors, eig.vec.inv = t(eig.vec), pr.eig.vec = matrix(eig.decomposition$vectors[,1:index], ncol = index), ratio = sum / sum.true)
  class(pca.result) = "pca"
  pca.result
}