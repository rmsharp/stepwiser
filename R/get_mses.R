#' Get Mean Squared Errors for a list of models using test data.
#' 
#' @return list object with Mean Squared Errors named with the name
#' of the variable selection (reduction) technique used.
#' 
#' @param models list of model objects made by \code{get_fit_models}.
#' @param data list having the data used for training and testing the 
#' various model types.
#' @importFrom stats predict
#' @export
get_mses <- function(models, data) {
  mses <- list()
  for (model_name in names(models)) {
    fit <- models[[model_name]]$model
    if (any(model_name %in% c("forward", "backward", "both")))
      mses[[model_name]] <- mean((data$test_y - as.numeric(
        stats::predict(fit, data$test, type = "response")))^2)
    else 
      mses[[model_name]] <-  mean((data$test_y - as.numeric(
        stats::predict(fit, s = models[[model_name]]$cv$lambda.1se,
                       newx = data$test_X)))^2)
  }
  mses
}
