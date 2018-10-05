#' dk_sim simulation function based on a 1992 paper by Derksen and Keselman
#' 
#' Backward, forward and stepwise automated subset selection algorithms:
#' Frequency of obtaining authentic and noise variables by Shelley Derksen
#' H. J. Keselman, British Journal of Mathematical and Statistical Psychology
#' (1992) 45, 265-282.
#' 
#' @return object with 
#' 
#' @param betas coefficients of regression
#' @param n_values vector of sample sizes
#' @param rho_values vector of correlations between real predictors
#' @param predictors vector of the number of predictor variables to include
#' @param error_fun function providing the error term for each row of 
#' observations
#' @param ... additional arguments need by \code{error_fun}
#' @importFrom MASS stepAIC
#' @export

dk_sim <- function(betas, n_values, rho_values, predictors, direction, 
                   error_fun, ...) {
  rows <- length(alpha_values) *
          length(n_values) *
          length(rho_values) *
          length(predictors)
  results <- list(rows)
  row <- 0
  
  for (alpha in alpha_values) {
    for (n in n_values) {
      for (rho in rho_values) {
        for (p in predictors) {
          corr <- c(rep(rho, min(p, 6)), rep(0, max(p - 6, 0)))
          y <- rnorm(n, 0, 100)
          error_fun <- function(y) {
            rnorm(length(y), sample(1:10, 1), sample(1:10, 1))
          }
          X <- get_predictors(y, betas, corr, p, error_fun)
          X <- as.data.frame(X, drop = FALSE)
          names(X) <- stri_c("X_", 1:ncol(X))
          data <- data.frame(y = y, X, drop = FALSE)
          fit <- glm(y ~ ., data = data)
          #sink("/dev/null")
          step <- invisible(stepAIC(fit, direction = direction))
          #sink()
          estimates <- invisible(summary(step))[[12]]
          row <- row + 1
          results[[row]] <- list(
            betas = betas,
            alpha = alpha,
            n = n,
            rho = rho,
            p = p,
            coefficients = coef(step),
            aic = step$aic,
            family = step$family$family,
            link = step$family$link,
            estimates = estimates)
        }
      }
    }
  }
  results
}

