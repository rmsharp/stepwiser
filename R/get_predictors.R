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
get_predictors <- function(y, betas, corr, p, error_fun) {
  if (length(y) < 2 | length(betas) < 0 | p < 1 | any(corr < 0.0) | length(corr) != p)
    stop(stri_c("Parameter error: check that p >= 1, n >= 2, all(corr >= 0.0), ",
                "length(corr) == p, dim(Z)[1] == n and dim(Z)[2] == (p + 1)"))
  
  if (is.null(error_fun))
    error_fun <- function(y) {rnorm(length(y), 0, 0.02)}
  n <- length(y)
  X <- matrix(NA, nrow = n, ncol = p)
  for (j in 1:p) {
    if (j <= length(betas))
      mult <- y * betas[j]
    else
      mult <- rnorm(length(y), sample(1:10, 1), sample(1:10, 1) * 0.1)
    
    X[ , j] <- (1 - corr[j]^2)^(0.5) * mult + corr[j] * y + error_fun(y)
  }
  X
}
