#' get_predictors returns matrix of predictor variable data elements
#' 
#' @return matrix of continuous predictive variables
#' 
#' @param corr correlation parameters that is a vector of length p. The X 
#' columns created with a correlation parameter value greater than 0
#' are authentic predictors and those with 
#' @param n number of observations (subjects)
#' @param p number of predictive variables
#' @param Z a matrix of independent identically distributed pseudorandom 
#'          variates with \code{n} rows and \code{p + 1} columns.
#' @importFrom stringi stri_c
#' @export
get_predictors <- function(corr, n, p, Z) {
  if (p < 1 | n < 2 | any(corr < 0.0) | length(corr) != p |
      dim(Z)[1] != n | dim(Z)[2] != (p + 1))
    stop(stri_c("Parameter error: check that p >= 1, n >= 2, all(corr >= 0.0), ",
                "length(corr) == p, dim(Z)[1] == n and dim(Z)[2] == (p + 1)"))
  X <- matrix(NA, nrow = n, ncol = p)
  for (j in 1:p) {
    X[ , j] <- (1 - corr[j]^2)^(0.5) * Z[ , j] + corr[j] * Z[ , p + 1]
  }
  X
}
