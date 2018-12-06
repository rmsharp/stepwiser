context("get_fivenum_mse")
library(stringi)

test_that("get_fivenum_mse fails with bad data", {
  data("results")
  expect_error(get_fivenum_mse(NULL))
  expect_error(get_fivenum_mse(NULL), "'results' argument is NULL")
})
test_that("get_fivenum_mse returns correct values", {
  data("results")
  expect_equal(length(get_fivenum_mse(results)), 4)
  expect_equal(length(get_fivenum_mse(results)[[1]]), 5)
  expect_equal(get_fivenum_mse(results)[[1]][1], 246.3026, tolerance = 0.01)
})

