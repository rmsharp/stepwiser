#' Get a list of the link functions used from a list of fitted model objects
#'
#' This function does not really work for glmnet. There is not link function
#' information directly stored within the fitted model. 
#' 
#' @return character vector of the link functions used to fit the models
#' @param models list of fitted models
#' @importFrom stringi stri_c stri_detect_fixed stri_split_fixed
#' @export
get_links <- function(models) {
  links <- list()
  for (model_name in names(models)) {
    fit <- models[[model_name]]
    if (any(model_name %in% c("both", "forward", "backward"))) {
      links[[model_name]] <- fit$family$link
    } else if (any(model_name %in% c("ridge", "lasso", "elastic_net"))) {
      if (stri_detect_fixed(stri_split_fixed(deparse(fit[[1]]$call), 
                                             pattern = ",")[[1]][3], "gaussian")) {
        links[[model_name]] <- "identity"
      } else {
        stop(stri_c("Could not detect link function in ", model_name, "."))
      } 
    } else {
      links[[model_name]] <- ""
    }
  }
  links
}
