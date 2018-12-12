#' Get a list of the distribution families used from a list of fitted model objects
#'
#' @return character vector of the distribution families used to fit the model
#' @param models list of fitted models
#' @importFrom stringi stri_c stri_detect_fixed stri_split_fixed
#' @export
get_families <- function(models) {
  families <- list()
  for (model_name in names(models)) {
    fit <- models[[model_name]]
    if (any(model_name %in% c("both", "forward", "backward"))) {
      families[[model_name]] <- fit$family$family
    } else if (any(model_name %in% c("ridge", "lasso", "elastic_net"))) {
      if (stri_detect_fixed(stri_split_fixed(deparse(fit[[1]]$call), 
                                             pattern = ",")[[1]][3], "gaussian")) {
        families[[model_name]] <- "gaussian"
      } else {
        stop(stri_c("Could not detect family in ", model_name, "."))
      } 
    } else {
      families[[model_name]] <- ""
    }
  }
  families
}
