context("get_predictors")
library(stringi)

set.seed(123)
n <- 1000
p <- 50
Z <- matrix(rnorm(n * (p + 1), 0, 1), nrow = n, ncol = p + 1)
corr <- c(rep(0.2, 6), rep(0, 44))
test_that("get_predictors stops on input errors", {
  error_mes <- stri_c("Parameter error:")
  expect_error(get_predictors())
  expect_error(get_predictors(), "argument \"p\" is missing, with no default")
  expect_error(get_predictors(corr = 0.2, n, p, Z), error_mes)
  expect_error(get_predictors(corr = corr, n = 1, p, Z), error_mes)
  expect_error(get_predictors(corr = corr, n = n, p = 0, Z), error_mes)
  expect_error(get_predictors(corr = corr[-1], n = n, p, Z), error_mes)
})
