#' Get coeficient names from a list of fitted models.
#' 
#' @return list character vectors with coeficient names named with the name
#' of the variable selection (reduction) technique used.
#' 
#' @param models list of model objects made by \code{get_fit_models}.
#' @param data list having the data used for training and testing the 
#' various model types.
#' @export
get_coefs <- function(models) {
  coefs <- list()
  for (model_name in names(models)) {
    fit <- models[[model_name]]
    if (any(model_name %in% c("forward", "backward", "both")))
      coefs[[model_name]] <- names(fit$model$coefficients)
    else 
      coefs[[model_name]] <- get_glmnet_coef(fit)
  }
  coefs
}
