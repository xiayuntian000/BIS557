# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Ridge regression using SVD.
#' Source: Github called bis557, lecture notes, originally written by Michale Kane
#'
#' @param form The formula to extract X from input dataset.
#' @param data Input dataset.
#' @param lambda Penalization parameter, default=0
#' @return The estimated model as a list.
#' @export
#' @examples
#' ridge(Sepal.Length ~ ., iris, lambda = 0.1)
#'
#'
#'
ridge <- function(form, data, lambda = 0) {
  rownames(data) <- NULL
  X <- model.matrix(form, data)
  # Y <- data[as.numeric(rownames(X)), as.character(form)[2]]
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
  beta <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
  ret<-list()
  ret$coefficients <- beta
  ret$formula <- form
  #class(ret) <- c(class(ret), "ridge_regression")
  ret
}
#library(devtools)
#devtools::document()

#usethis::use_testthat()

#devtools::test()
