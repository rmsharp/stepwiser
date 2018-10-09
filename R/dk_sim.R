#' dk_sim simulation function based on a 1992 paper by Derksen and Keselman
#' 
#' Backward, forward and stepwise automated subset selection algorithms:
#' Frequency of obtaining authentic and noise variables by Shelley Derksen
#' H. J. Keselman, British Journal of Mathematical and Statistical Psychology
#' (1992) 45, 265-282.
#' 
#' Need to allow add and remove thresholds instead of a single value.
#' 
#' @return object with 
#' 
#' @param file character containing file argument to cat used to report intermediate results
#' @param directions character vector with one or more of "backward", "forward",
#' or "both".
#' @param betas coefficients of regression
#' @param n_values vector of sample sizes
#' @param alpha_values vector of alpha threshold values
#' @param rho_values vector of correlations between real predictors
#' @param predictors vector of the number of predictor variables to include
#' @param sims number of simulations
#' @param error_fun function providing the error term for each row of 
#' observations
#' @param ... additional arguments need by \code{error_fun}
#' @importFrom MASS stepAIC
#' @importFrom stats fivenum
#' @importFrom stats glm
#' @importFrom stats rnorm
#' @importFrom utils flush.console
#' @export

dk_sim <- function(file = "", directions, betas, n_values, alpha_values, 
                   rho_values, predictors, sims, error_fun, ...) {
  rows <- length(alpha_values) *
          length(n_values) *
          length(rho_values) *
          length(predictors)
  results <- list(sims)
  sim_results <- list(rows)
  s_row <- 0
  n_betas <- length(betas)
  authentic <- paste0("X_", 1:n_betas)
  
  for (direction in directions) {
    for (n in n_values) {
      for (alpha in alpha_values) {
        for (rho in rho_values) {
          for (p in predictors) {
            if (p > n)
              next
            corr <- c(rep(rho, min(p, n_betas)), rep(0, max(p - n_betas, 0)))
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
            
            fivenum_authentic <- fivenum(sapply(results, function(x) {
              length(authentic[x$coef_names %in% authentic])
            } ))
            fivenum_noise <- fivenum(sapply(results, function(x) {
              length(x$coef_names[!x$coef_names %in% authentic])
            } ))
            sim_results[[s_row]] <- list(
              betas = betas, alpha = alpha, n = n, rho = rho, p = p,
              family = step$family$family, link = step$family$link,
              fivenum_authentic = fivenum_authentic, 
              fivenum_noise = fivenum_noise,
              sims = sims)
            cat(file = file, paste0("row ", s_row, " of ", rows, ", alpha = ", alpha, 
                       ", n = ", n, ", rho = ", round(rho, 2), 
                       ", p = ", p, ", family = ", step$family$family, 
                       ", link = ", step$family$link,
                       ", fivenum_authentic[min, median, max] = ", 
                       fivenum_authentic[1], ", ", fivenum_authentic[3], ", ", fivenum_authentic[5],
                       ", fivenum_noise[min, median, max] = ", 
                       fivenum_noise[1], ", ", fivenum_noise[3], ", ", fivenum_noise[5], 
                       ", sims = ", sims, ".\n"), append = TRUE)
            flush.console()
          }
        }
      }
    }
  }
  sim_results
}

