#' Get fivenum summary of mean squared error from results list
#' 
#' @param results list of results from regressions
get_fivenum_mse <- function(results) {
  list(
    step = fivenum(sapply(results, function(x) x$mse_step)),
    lasso = fivenum(sapply(results, function(x) x$mse_lasso)),
    ridge = fivenum(sapply(results, function(x) x$mse_ridge)),
    net = fivenum(sapply(results, function(x) x$mse_net))
  )
}
