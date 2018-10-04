#' get_targets returns matrix of target data elements
#' 
#' @return matrix of target variables
#' 
#' @param betas coefficients to use to make the target values
#' @param X a matrix of observed predictor variable values
#' @importFrom stringi stri_c
#' @export
get_targets <- function(betas, X, error_fun, ...) {
    T <- X[ , seq_along(betas), drop = FALSE] * betas
    T <- apply(T, 1, sum) + error_fun(...)
    T
}
