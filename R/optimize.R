# Hello, world!
#
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#'
#' lambda optimization using k-fold cross validation.
#'
#' @name optimize
#' @param X  X from input dataset.
#' @param Y  Y from Input dataset.
#' @param k number of folds for the cross validation
#' @param start start of test lambda sequence
#' @param end end of test lambda sequence
#' @param length number of items in the lambda sequence
#' @export
#' @return The estimated model as a list.

#optimize(X,Y,5,0.01,400,500)
optimize <- function(X,Y,k,start,end,length) {
  n <- end/2
  N <- length
  L=end
  proportion=1/k
  folds <- cut(seq(1,nrow(X)),breaks=k,labels=FALSE)

  #permute::shuffle(folds)
  mse_total=NULL
  lambda_vals <- seq(start, L, length.out = N)
  for(i in 1:k){
    idx <- which(folds==i,arr.ind=TRUE)
    test_X <- X[idx, ]
    train_X <- X[-idx, ]
    test_Y <- Y[idx ]
    train_Y <- Y[-idx]
    lambda_vals <- seq(start, L, length.out = N)

    beta_mat <- sapply(lambda_vals,function(x) {
      ridge_me <- ridge(train_Y ~ ., as.data.frame(cbind(train_X,train_Y)), lambda = x)
      return(matrix(as.double(ridge_me$coefficients[-1])))
    })
    y_hat <- as.matrix(test_X) %*% beta_mat
    #y_hat <- crossprod(X, beta_mat)
    #mse <- apply((y_hat - y_test)^2, 2, mean)
    mse <- as.matrix(apply((y_hat - test_Y)^2, 2, mean))
    mse_total=cbind(mse_total,mse)
  }
  mse_avg=apply(mse_total,1,mean)
  lambda_vals <- seq(start, L, length.out = N)
  final_lambda <- lambda_vals[which.min(mse_avg)]
  return(final_lambda)
}

