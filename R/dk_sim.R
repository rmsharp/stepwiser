#' dk_sim simulation function based on a 1992 paper by Derksen and Keselman
#' 
#' Backward, forward and both automated subset selection algorithms:
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
#' @param weight observation weights. Can be total counts if responses are 
#' proportion matrices. Default is 1 for each observation 
#' @param nlambda The number of lambda values - default is 100.
#' @param sims number of simulations
#' @param error_fun function providing the error term for each row of 
#' observations
#' @param ... additional arguments need by \code{error_fun}
#' @importFrom MASS stepAIC
#' @importFrom stats fivenum
#' @importFrom stats glm
#' @importFrom stats rnorm
#' @importFrom utils flush.console
#' @import doParallel
#' @import glmnet
#' @importFrom tcltk tkProgressBar setTkProgressBar
#' @export
dk_sim <- function(file = "", directions, betas, n_values, 
                           alpha_values, rho_values, predictors,
                           weight = 1, nlambda = 100, sims, error_fun, ...) {
  registerDoParallel(cores = 4)
  rows <- length(directions) *
          length(alpha_values) *
          length(n_values) *
          length(rho_values) *
          length(predictors)
  total_rows <- sims * rows
  results <- list(total_rows)
  #sim_results <- list(rows)
  s_row <- 0
  row <- 0
  n_betas <- length(betas)
  authentic <- paste0("X_", 1:n_betas)
  pb <- tkProgressBar(title = "simulation progress", min =  0,
                      max = sims * rows, width = 300)
  for (direction in directions) {
    for (n in n_values) {
      for (alpha in alpha_values) {
        for (rho in rho_values) {
          for (p in predictors) {
            if (p > n) {
              next
            }
            noise <- paste0("X_", (n_betas + 1):p)
            corr <- c(rep(rho, min(p, n_betas)), rep(0, max(p - n_betas, 0)))
            #r_row <- 0
            for (sim in 1:sims) {
              data <- make_dataset(n, betas, corr, p)

              fit_models <- get_fit_models(data = data, 
                                           models = c("both", "backward", 
                                                      "ridge", "lasso", 
                                                      "elastic_net"), weight = weight)
              row <- row + 1
              results[[row]] <- 
                make_results(direction = direction, n = n, 
                             alpha = alpha, rho = rho, p = p,
                             models = fit_models, data, sim = sim)
              setTkProgressBar(pb, row, label=paste(round(row / total_rows * 100, 0),
                                                  "% done"))
            }
            s_row <- s_row + 1
            # fivenum_coef <- get_fivenum_coef(results, authentic, noise)
            # fivenum_mse <- get_fivenum_mse(results)
            # sim_results[[s_row]] <- list(
            #   results = results,
            #   direction = direction,
            #   betas = betas, alpha = alpha, n = n, rho = rho, p = p,
            #   family = fit_step$family$family, link = fit_step$family$link,
            #   fivenum_authentic_step = fivenum_coef$coef_step$authentic,
            #   fivenum_noise_step = fivenum_coef$coef_step$noise,
            #   fivenum_authentic_lasso = fivenum_coef$coef_lasso$authentic,
            #   fivenum_noise_lasso = fivenum_coef$coef_lasso$noise,
            #   fivenum_authentic_ridge = fivenum_coef$coef_ridge$authentic,
            #   fivenum_noise_ridge = fivenum_coef$coef_ridge$noise,
            #   fivenum_authentic_net = fivenum_coef$coef_net$authentic,
            #   fivenum_noise_net = fivenum_coef$coef_net$noise,
            #   mse_step = fivenum_mse$step,
            #   mse_lasso = fivenum_mse$lasso,
            #   mse_ridge = fivenum_mse$ridge,
            #   mse_net = fivenum_mse$net,
            #   sims = sims)
            # cat(file = file,
            #     paste0(
            #       "row ", s_row, " of ", rows, ": direction = ",
            #       direction, ", n = ", n, ", alpha = ", signif(alpha, digits = 3),
            #       ", rho = ", round(rho, 2),
            #       ", p = ", p, ", family = ", fit_step$family$family,
            #       ", link = ", fit_step$family$link,
            #       ", fivenum_coef$coef_step$authentic[min, median, max] = ",
            #       fivenum_coef$coef_step$authentic[1], ", ", 
            #       fivenum_coef$coef_step$authentic[3], ", ", 
            #       fivenum_coef$coef_step$authentic[5],
            #       ", fivenum_coef$coef_step$noise[min, median, max] = ",
            #       fivenum_coef$coef_step$noise[1], ", ", 
            #       fivenum_coef$coef_step$noise[3], ", ", 
            #       fivenum_coef$coef_step$noise[5],
            #       ", fivenum_coef$coef_lasso$authentic[min, median, max] = ",
            #       fivenum_coef$coef_lasso$authentic[1], ", ", 
            #       fivenum_coef$coef_lasso$authentic[3], ", ", 
            #       fivenum_coef$coef_lasso$authentic[5],
            #       ", fivenum_coef$coef_lasso$noise[min, median, max] = ",
            #       fivenum_coef$coef_lasso$noise[1], ", ", 
            #       fivenum_coef$coef_lasso$noise[3], ", ", 
            #       fivenum_coef$coef_lasso$noise[5],
            #       ", fivenum_coef$coef_ridge$authentic[min, median, max] = ",
            #       fivenum_coef$coef_ridge$authentic[1], ", ", 
            #       fivenum_coef$coef_ridge$authentic[3], ", ",
            #       fivenum_coef$coef_ridge$authentic[5],
            #       ", fivenum_coef$coef_ridge$noise[min, median, max] = ",
            #       fivenum_coef$coef_ridge$noise[1], ", ", 
            #       fivenum_coef$coef_ridge$noise[3], ", ", 
            #       fivenum_coef$coef_ridge$noise[5],
            #       ", fivenum_coef$coef_net$authentic[min, median, max] = ",
            #       fivenum_coef$coef_net$authentic[1], ", ", 
            #       fivenum_coef$coef_net$authentic[3], ", ", 
            #       fivenum_coef$coef_net$authentic[5],
            #       ", fivenum_coef$coef_net$noise[min, median, max] = ",
            #       fivenum_coef$coef_net$noise[1], ", ", 
            #       fivenum_coef$coef_net$noise[3], ", ", 
            #       fivenum_coef$coef_net$noise[5],
            #       ", fivenum_mse$step[min, median, max] = ", fivenum_mse$step[1], ", ",
            #       fivenum_mse$step[3], ", ", fivenum_mse$step[5],
            #       ", fivenum_mse$lasso[min, median, max] = ", fivenum_mse$lasso[1], ", ",
            #       fivenum_mse$lasso[3], ", ", fivenum_mse$lasso[5],
            #       ", fivenum_mse$ridge[min, median, max] = ", fivenum_mse$ridge[1], ", ",
            #       fivenum_mse$ridge[3], ", ", fivenum_mse$ridge[5],
            #       ", fivenum_mse$net[min, median, max] = ", fivenum_mse$net[1], ", ",
            #       fivenum_mse$net[3], ", ", fivenum_mse$net[5],
            #       ", sims = ", sims, ".\n"), append = TRUE)
            flush.console()
          }
        }
      }
    }
  }
  results
}

