#' Gets fivenum() summary for coefficients retained by various models
#' 
#' @param results list of results created inside simulation loop
#' @param authentic character vector of authentic predictive variables.
#' @param noise character vector of noise predictive variables.
#' @importFrom stringi stri_detect_fixed
#' @export
get_fivenum_coef <- function(results, authentic, noise) {
  if (is.null(results))
    stop("The 'results' argument is NULL and must be defined.")
  coef_names <- unique(names(results[[1]])[stri_detect_fixed(names(results[[1]]), "coef_")])
  coefs <- list()
  for (coef_name in coef_names) {
    coefs[[coef_name]] <- list(authentic = fivenum(sapply(results, function(x) {
      length(authentic[authentic %in% x[[coef_name]]])
    } )),
    noise = fivenum(sapply(results, function(x) {
      length(noise[noise %in% x[[coef_name]]])
    } )))
  }
  coefs
}
