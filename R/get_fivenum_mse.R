#' Get fivenum summary of mean squared error from results list
#'
#' @param results list of results from regressions
#' @export
get_fivenum_mse <- function(results) {
  if (is.null(results))
    stop("The 'results' argument is NULL and must be defined.")
  mses <- list()
  mse_names <-
    unique(names(results[[1]])[stri_detect_fixed(names(results[[1]]), "mse_")])
  for (mse_name in mse_names) {
    mses[[mse_name]] <-
      fivenum(sapply(results, function(x)
        x[[mse_name]]))
  }
  mses
}
