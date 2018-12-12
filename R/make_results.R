#' Makes a result list object with all simulation results included.
#'
#' @return The most complete form of the results of the simulation
#' are provided by this object. It has a sublist of results for every
#' simulated sample.
#'
#' @param direction character vector with one of "backward", "forward",
#' or "both".
#' @param n sample size
#' @param alpha threshold value
#' @param rho correlations between real predictors
#' @param p number of predictor variables to include
#' @param models list of fitted models
#' @param data list having the data used for training and testing the 
#' various model types.
#' @param sim number of simulations
#' @export
make_results <- function(direction, n, alpha, rho, p, models, data, sim) {
  family <- get_families(models)
  link <- get_links(models)
  coef <- get_coefs(models)
  mse <- get_mses(models, data)
  
  list(
    direction = direction, n = n, alpha = alpha, rho = rho, p = p,
    family = family, link = link, coef = coef, mse = mse, sim = sim)
}