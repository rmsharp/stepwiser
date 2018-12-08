context("get_predictors")

set.seed(123)
n <- 100
p <- 50
betas <- c(1, 2, 3, -1, -2, -3)
rho <- 0.2
n_betas <- length(betas)
corr <- c(rep(rho, min(p, n_betas)), rep(0, max(p - n_betas, 0)))
y <- rnorm(n, 0, 80)
error_fun_mean <- c(0, 10, 100)
error_fun_sd <- 50:200
error_fun <- function(y) {
  rnorm(n, sample(error_fun_mean, 1), sample(error_fun_sd, 1))
}
test_that("get_predictors stops on input errors", {
  error_mes <- stri_c("Parameter error:")
  expect_error(get_predictors())
  expect_error(get_predictors(), "argument \"y\" is missing, with no default")
  expect_error(get_predictors(NULL, betas, corr, p, error_fun), "Parameter error:")
  expect_error(get_predictors(y = 1, betas, corr, p, error_fun), "Parameter error:")
  
})
test_that("get_predictors creates correct matrix", {
  X <- get_predictors(y, betas, corr, p, error_fun)
  expect_equal(dim(X), c(100, 50))
  expect_equal(X[1, 1], -2.807426, tolerance = .0001, scale = 1)
  expect_equal(X[5, 2], 14.29811, tolerance = .0001, scale = 1)
  expect_equal(X[100, 50], 82.80998, tolerance = .0001, scale = 1)
})

