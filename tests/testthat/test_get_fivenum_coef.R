context("get_fivenum_coef")
library(stringi)

test_that("get_fivenum_coef fails with bad data", {
  data("results")
  authentic <- paste0("X_", 1:6)
  noise <- paste0("X_", 7:24)
  expect_error(get_fivenum_coef(NULL, authentic, noise))
  expect_error(get_fivenum_coef(NULL, authentic, noise), "'results' argument is NULL")
})
test_that("get_fivenum_coef returns correct values", {
  data("results")
  authentic <- paste0("X_", 1:6)
  noise <- paste0("X_", 7:24)
  expect_equal(length(get_fivenum_coef(results, authentic, noise)), 4)
  expect_equal(get_fivenum_coef(results, authentic, noise)[["coef_net"]][["authentic"]][3], 5.5)
})

