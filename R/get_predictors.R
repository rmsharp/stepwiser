#' get_predictors returns matrix of predictor variable data elements
#' 
#' @return matrix of continuous predictive variables
#' 
#' @param y target value observations
#' @param betas coefficients to use to make the target values
#' @param corr correlation parameters that is a vector of length p. The X 
#' columns created with a correlation parameter value greater than 0
#' are authentic predictors and those with no correlation parameter are noise
#' @param p number of predictive variables
#' @param error_fun error function 
#' @importFrom stringi stri_c
#' @export
get_predictors <- function(y, betas, corr, p, error_fun) {
  if (length(y) < 2 | length(betas) < 0 | p < 1 | any(corr < 0.0) | 
      length(corr) != p)
    stop(stri_c("Parameter error: check that y >= 2, length(betas) >= 0, ",
                "p >= 1, n >= 2, all(corr >= 0.0), ", "length(corr) != p"))
  
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
