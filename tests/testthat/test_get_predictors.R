context("get_predictors")
library(stringi)

set.seed(123)
n <- 1000
p <- 50
Z <- matrix(rnorm(n * (p + 1), 0, 1), nrow = n, ncol = p + 1)
corr <- c(rep(0.2, 6), rep(0, 44))
test_that("get_predictors stops on input errors", {
  expect_error(get_predictors())
  expect_error(get_predictors(), 
               "argument \"p\" is missing, with no default")
  expect_error(get_predictors(corr = 0.2, n, p, Z), 
               "p must be >= 1, n must be >= 2, and corr must be >= 0.0")
  expect_error(get_predictors(corr = corr, n = 1, p, Z), 
               "p must be >= 1, n must be >= 2, and corr must be >= 0.0")
})
