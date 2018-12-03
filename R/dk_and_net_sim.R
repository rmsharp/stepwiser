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
#' @import glmnet
#' @export
dk_and_net_sim <- function(file = "", directions, betas, n_values, 
                           alpha_values, rho_values, predictors,
                           weight = 1, nlambda = 100, sims, error_fun, ...) {
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
            noise <- paste0("X_", (n_betas + 1):p)
            if (p > n) {
              next
            }
            corr <- c(rep(rho, min(p, n_betas)), rep(0, max(p - n_betas, 0)))
            r_row <- 0
            for (i in 1:sims) {
              data <- make_dataset(n, betas, corr, p)

              cv_lasso <- get_cv_glmnet(data$train_X, data$train_y, cv_alpha = 1)
              cv_ridge <- get_cv_glmnet(data$train_X, data$train_y, cv_alpha = 0)
              cv_net <- get_cv_glmnet(data$train_X, data$train_y, 
                                      cv_alpha = seq(0.1, 0.9, 0.05))

              fit <- glm(y ~ ., data = data$train)
              sink("/dev/null")
              fit_step <- invisible(stepAIC(fit, direction = direction))
              sink()
              fit_lasso <- glmnet(data$train_X, data$train_y, family = "gaussian",
                                weights = rep(weight, nrow(data$train_X)),
                                lambda = cv_lasso$lambda.1se,
                                alpha = 1)
              fit_ridge <- glmnet(data$train_X, data$train_y, family = "gaussian",
                                weights = rep(weight, nrow(data$train_X)),
                                lambda = cv_ridge$lambda.1se,
                                alpha = 0)
              fit_net <- glmnet(data$train_X, data$train_y, family = "gaussian",
                                weights = rep(weight, nrow(data$train_X)),
                                lambda = cv_net$lambda.1se,
                                alpha = cv_net$alpha)
              r_row <- r_row + 1
              results[[r_row]] <- list(
                coef_step = names(fit_step$coefficients),
                coef_lasso = get_glmnet_coef(fit_lasso),
                coef_ridge = get_glmnet_coef(fit_ridge),
                coef_net = get_glmnet_coef(fit_net),
                mse_step = mean((data$test_y - as.numeric(
                  predict(fit_step, data$test, type = "response")))^2),
                mse_lasso = mean((data$test_y - as.numeric(
                  predict(fit_lasso, s = cv_lasso$lambda.1se,
                          newx = data$test_X)))^2),
                mse_ridge = mean((data$test_y - as.numeric(
                  predict(fit_ridge, s = cv_ridge$lambda.1se,
                          newx = data$test_X)))^2),
                mse_net =  mean((data$test_y -
                                   predict(fit_net, s = cv_net$lambda.1se,
                                           newx = data$test_X))^2)
                )
            }
            s_row <- s_row + 1
            fivenum_coef <- get_fivenum_coef(results, authentic, noise)
            fivenum_mse <- get_fivenum_mse(results)
            sim_results[[s_row]] <- list(
              betas = betas, alpha = alpha, n = n, rho = rho, p = p,
              family = fit_step$family$family, link = fit_step$family$link,
              fivenum_authentic_step = fivenum_coef$authentic_step,
              fivenum_noise_step = fivenum_coef$noise_step,
              fivenum_authentic_lasso = fivenum_coef$authentic_lasso,
              fivenum_noise_lasso = fivenum_coef$noise_lasso,
              fivenum_authentic_ridge = fivenum_coef$authentic_ridge,
              fivenum_noise_ridge = fivenum_coef$noise_ridge,
              fivenum_authentic_net = fivenum_coef$authentic_net,
              fivenum_noise_net = fivenum_coef$noise_net,
              mse_step = fivenum_mse$step,
              mse_lasso = fivenum_mse$lasso,
              mse_ridge = fivenum_mse$ridge,
              mse_net = fivenum_mse$net,
              sims = sims)
            cat(file = file,
                paste0(
                  "row ", s_row, " of ", rows, ": direction = ",
                  direction, ", n = ", n, ", alpha = ", signif(alpha, digits = 3),
                  ", rho = ", round(rho, 2),
                  ", p = ", p, ", family = ", fit_step$family$family,
                  ", link = ", fit_step$family$link,
                  ", fivenum_coef$authentic_step[min, median, max] = ",
                  fivenum_coef$authentic_step[1], ", ", fivenum_coef$authentic_step[3], ", ", fivenum_coef$authentic_step[5],
                  ", fivenum_coef$noise_step[min, median, max] = ",
                  fivenum_coef$noise_step[1], ", ", fivenum_coef$noise_step[3], ", ", fivenum_coef$noise_step[5],
                  ", fivenum_coef$authentic_lasso[min, median, max] = ",
                  fivenum_coef$authentic_lasso[1], ", ", fivenum_coef$authentic_lasso[3], ", ", fivenum_coef$authentic_lasso[5],
                  ", fivenum_coef$noise_lasso[min, median, max] = ",
                  fivenum_coef$noise_lasso[1], ", ", fivenum_coef$noise_lasso[3], ", ", fivenum_coef$noise_lasso[5],
                  ", fivenum_coef$authentic_ridge[min, median, max] = ",
                  fivenum_coef$authentic_ridge[1], ", ", fivenum_coef$authentic_ridge[3], ", ", fivenum_coef$authentic_ridge[5],
                  ", fivenum_coef$noise_ridge[min, median, max] = ",
                  fivenum_coef$noise_ridge[1], ", ", fivenum_coef$noise_ridge[3], ", ", fivenum_coef$noise_ridge[5],
                  ", fivenum_coef$authentic_net[min, median, max] = ",
                  fivenum_coef$authentic_net[1], ", ", fivenum_coef$authentic_net[3], ", ", fivenum_coef$authentic_net[5],
                  ", fivenum_coef$noise_net[min, median, max] = ",
                  fivenum_coef$noise_net[1], ", ", fivenum_coef$noise_net[3], ", ", fivenum_coef$noise_net[5],
                  ", fivenum_mse$step[min, median, max] = ", fivenum_mse$step[1], ", ",
                  fivenum_mse$step[3], ", ", fivenum_mse$step[5],
                  ", fivenum_mse$lasso[min, median, max] = ", fivenum_mse$lasso[1], ", ",
                  fivenum_mse$lasso[3], ", ", fivenum_mse$lasso[5],
                  ", fivenum_mse$ridge[min, median, max] = ", fivenum_mse$ridge[1], ", ",
                  fivenum_mse$ridge[3], ", ", fivenum_mse$ridge[5],
                  ", fivenum_mse$net[min, median, max] = ", fivenum_mse$net[1], ", ",
                  fivenum_mse$net[3], ", ", fivenum_mse$net[5],
                  ", sims = ", sims, ".\n"), append = TRUE)
            flush.console()
          }
        }
      }
    }
  }
  sim_results
}

