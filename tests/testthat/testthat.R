library(testthat)
library(homework2)
#library(glmnet)
library(MASS)
#library(parcor)
library(permute)
#test_check("homework2")
context("Test the output of homework 2, ridge.")

test_that("You ridge() function works in an easy case.", {

  data(iris)
  lamb=0.01
  fit_my_ridge <- ridge(Sepal.Length ~ ., iris, lambda = lamb)

  X <- model.matrix(Sepal.Length ~ .,iris)

  Y <- iris$Sepal.Length
  #fit_glmnet <- glmnet(X,Y,alpha=0,lambda = lamb)
  fit_ridge <- lm.ridge(Sepal.Length ~ ., iris, lambda = lamb)

  #expect_equivalent(as.double(fit_my_ridge$coefficients),as.double(coef(fit_glmnet,s=lamb))[-2],
                   # tolerance = 1)
  expect_equivalent(as.matrix(fit_my_ridge$coefficients),as.matrix(coef(fit_ridge)),
                    tolerance = 0.05)
})


test_that("You optimization() function works in an easy case.", {

  data(iris)
  set.seed(38)
  X <- model.matrix(Sepal.Length ~ .,iris)

  Y <- iris$Sepal.Length
  my_lambda <- optimize(X,Y,5,1e-10,400,500)

  #fit_glmnet <- glmnet(X,Y,alpha=0,lambda = lamb)
  n <- 200
  N <- 500
  L=n*2
  k=5
  # proportion=1/k
  # folds <- cut(seq(1,nrow(X)),breaks=k,labels=FALSE)
  # mse_total=NULL
  # for(i in 1:k){
  #   idx <- which(folds==i,arr.ind=TRUE)
  #   test_X <- X[idx, ]
  #   train_X <- X[-idx, ]
  #   test_Y <- Y[idx ]
  #   train_Y <- Y[-idx]
  #   lambda_vals <- seq(1e-10, L, length.out = N)
  #    beta_mat <- rep(0, n*2)
  #   for (i in 1:n*2){
  #      ridge_me <- ridge(Sepal.Length ~ ., iris, lambda = lambda_vals[i] )
  #      beta_mat[i] <- ridge_me$coefficients  }
  #
  #   beta_mat <- sapply(lambda_vals,function(x) {
  #     ridge_me <- lm.ridge(train_Y ~ ., as.data.frame(cbind(train_X,train_Y)), lambda = x)
  #     return(matrix(as.double(ridge_me$coefficients[-2])))
  #   })
  #   y_hat <- test_X %*% beta_mat
  #   y_hat <- crossprod(X, beta_mat)
  #   #mse <- apply((y_hat - y_test)^2, 2, mean)
  #   mse <- as.matrix(apply((y_hat - test_Y)^2, 2, mean))
  #   mse_total=cbind(mse_total,mse)
  # }
  # mse_avg=apply(mse_total,1,mean)
  # lmridge_lambda <- lambda_vals[which.min(mse_avg)]
  #lmridge_lambda <- ridgereg.cv(Y,X,5 ,lambda_vals)
  lambda_vals <- seq(1e-10, L, length.out = N)
  fit_ridge <- lm.ridge(Sepal.Length ~ ., iris, lambda = lambda_vals)
  #fit_ridge <- lm.ridge(mpg ~ ., mtcars, lambda = lambda_vals)

  lmridge_lambda<- fit_ridge$lambda[which.min(fit_ridge$GCV)]
  #lmridge_lambda<-cv.glmnet(as.matrix(X),as.matrix(Y),nfolds=5,lambda = lambda_vals)
  expect_equivalent(my_lambda,lmridge_lambda ,
                    tolerance = 16)
})
