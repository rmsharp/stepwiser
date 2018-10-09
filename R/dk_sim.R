#' dk_sim simulation function based on a 1992 paper by Derksen and Keselman
#' 
#' Backward, forward and stepwise automated subset selection algorithms:
#' Frequency of obtaining authentic and noise variables by Shelley Derksen
#' H. J. Keselman, British Journal of Mathematical and Statistical Psychology
#' (1992) 45, 265-282.
#' 
#' @return object with 
#' 
#' @param sims number of simulations
#' @param betas coefficients of regression
#' @param n_values vector of sample sizes
#' @param rho_values vector of correlations between real predictors
#' @param predictors vector of the number of predictor variables to include
#' @param error_fun function providing the error term for each row of 
#' observations
#' @param ... additional arguments need by \code{error_fun}
#' @importFrom MASS stepAIC
#' @export

dk_sim <- function(sims, betas, n_values, rho_values, predictors, direction, 
                   error_fun, ...) {
  rows <- length(alpha_values) *
          length(n_values) *
          length(rho_values) *
          length(predictors)
  results <- list(sims)
  sim_results <- list(rows)
  s_row <- 0
  n_betas <- length(betas)
  authentic <- paste0("X_", 1:n_betas)
  
  for (alpha in alpha_values) {
    for (n in n_values) {
      for (rho in rho_values) {
        for (p in predictors) {
          corr <- c(rep(rho, min(p, 6)), rep(0, max(p - 6, 0)))
          r_row <- 0
          for (i in 1:sims) {
            y <- rnorm(n, 0, 100)
            error_fun <- function(y) {
              rnorm(length(y), sample(1:10, 1), sample(1:10, 1))
            }
            X <- get_predictors(y, betas, corr, p, error_fun)
            X <- as.data.frame(X, drop = FALSE)
            names(X) <- paste0("X_", 1:ncol(X))
            data <- data.frame(y = y, X, drop = FALSE)
            fit <- glm(y ~ ., data = data)
            sink("/dev/null")
            step <- invisible(stepAIC(fit, direction = direction))
            sink()
            #estimates <- invisible(summary(step))[[12]] # don't need
            r_row <- r_row + 1
            results[[r_row]] <- list(coef_names = names(step$coefficients))
          }
          s_row <- s_row + 1
          
          mean_authentic <- mean(sapply(results, function(x) {
            length(authentic[x$coef_names %in% authentic])
          } ))
          mean_noise <- mean(sapply(results, function(x) {
            length(x$coef_names[!x$coef_names %in% authentic])
          } ))
          sim_results[[s_row]] <- list(
            betas = betas, alpha = alpha, n = n, rho = rho, p = p,
            family = step$family$family, link = step$family$link,
            mean_authentic = mean_authentic, mean_noise = mean_noise,
            sims = sims)
        }
      }
    }
  }
  sim_results
}

