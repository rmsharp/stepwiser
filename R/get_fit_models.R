#' Fits list of models to provided data
#' 
#' @return List of fitted model objects
#' @param data data set made by \code{make_dataset}
#' @param models character vector of model types to fit. Can be
#' \code{"backward"}, \code{"forward"}, \code{"both"}, \code{"ridge"}
#' \code{"lasso"}, or \code{"elastic_net"}.
#' @param ... additional named parameters needed to fit the models.
#' @importFrom MASS stepAIC
#' @importFrom glmnet glmnet
#' @export
get_fit_models <- function(data, models, ...) {
  fits <- list()
  if (any(models %in% c("forward", "backward", "both")))
    fit <- glm(y ~ ., data = data$train)
  
  for (model in models) {
    if (model == "forward") {
      #sink("/dev/null")
      fits[["forward"]] <- list(model = invisible(stepAIC(fit, direction = "forward")))
      #sink()
    } else if (model == "backward") {
      sink("/dev/null")
      fits[["backward"]] <- list(model = invisible(stepAIC(fit, direction = "backward")))
      sink()
    } else if (model == "both") {
      sink("/dev/null")
      fits[["both"]] <- list(model = invisible(stepAIC(fit, direction = "both")))
      sink()
    } else if (model == "ridge") {
      cv_ridge <- get_cv_glmnet(data$train_X, data$train_y, cv_alpha = 0)
      fits[["ridge"]] <- 
        list(model = glmnet(data$train_X, data$train_y, family = "gaussian",
                                  weights = rep(weight, nrow(data$train_X)),
                                  lambda = cv_ridge$lambda.1se,
                                  alpha = 0),
             cv = cv_ridge)
    } else if (model == "lasso") {
      cv_lasso <- get_cv_glmnet(data$train_X, data$train_y, cv_alpha = 1)
      fits[["lasso"]] <- 
        list(model = glmnet(data$train_X, data$train_y, family = "gaussian",
                                  weights = rep(weight, nrow(data$train_X)),
                                  lambda = cv_lasso$lambda.1se,
                                  alpha = 1),
             cv = cv_lasso)
    } else if (model == "elastic_net") {
      cv_net <- get_cv_glmnet(data$train_X, data$train_y, 
                              cv_alpha = seq(0.1, 0.9, 0.05))
      fits[["elastic_net"]] <-  
        list(model = glmnet(data$train_X, data$train_y, family = "gaussian",
                                        weights = rep(weight, nrow(data$train_X)),
                                        lambda = cv_net$lambda.1se,
                                        alpha = cv_net$alpha),
             cv = cv_net)
    }
  }
  fits
}
