context("get_targets")
library(stringi)

# set.seed(123)
# n <- 1000
# p <- 50
# Z <- matrix(rnorm(n * (p + 1), 0, 1), nrow = n, ncol = p + 1)
# corr <- c(rep(0.2, 6), rep(0, 44))
# X <- get_predictors(corr = corr, n = n, p = p, Z = Z)
# test_that("get_targets stops on input errors", {
#   error_mes <- stri_c("Parameter error:")
#   expect_error(get_targets())
#   expect_error(get_targets(), "argument \"X\" is missing, with no default")
# })
# test_that("get_targets creates correct matrix", {
#   test <- get_targets(c(1, 2, 3, -1, -2, -3), X, rnorm, nrow(X), 0, 1)
#   
#   expect_equal(length(test), n)               
#   expect_equal(test[1], -0.9100675, tolerance = 0.001, scale = 1)
#   expect_equal(test[1000], -5.96023, tolerance = 0.001, scale = 1)
#   test2 <- get_targets(c(2, 4), matrix(rep(1:3, each=3), nrow = 3), rnorm, 3, 0, 1)
#   
# })                            