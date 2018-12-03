#' Get coeficients without intercept from glmnet model 
#' 
#' @param fit glmnet model
#' @importFrom stats coef
#' @export
get_glmnet_coef <- function(fit) {
  coef_fit <- coef(fit)
  coef_fit@Dimnames[[1]][-1][coef_fit@i[-1]]
}
