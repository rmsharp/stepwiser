#' Gets fivenum() summary for coefficients retained by various models
#' 
#' @param results list of results created inside simulation loop
#' @param authentic character vector of authentic predictive variables.
#' @param noise character vector of noise predictive variables.
#' @export
get_fivenum_coef <- function(results, authentic, noise) {
  coef_names <- unique(names(results[[1]])[stri_detect_fixed(names(results[[1]]), "coef_")])
  coefs <- list()
  for (coef_name in coef_names) {
    coefs[[coef_name]] <- list(authentic = fivenum(sapply(results, function(x) {
      length(authentic[authentic %in% x[[coef_name]]])
    } )),
    noise = fivenum(sapply(results, function(x) {
      length(noise[noise %in% x[[coef_name]]])
    } )))
  }
  coefs
  # list(
  #   authentic_step = fivenum(sapply(results, function(x) {
  #     length(authentic[authentic %in% x$coef_step])
  #   } )),
  #   noise_step = fivenum(sapply(results, function(x) {
  #     length(noise[noise %in% x$coef_step])
  #   } )),
  #   authentic_lasso = fivenum(sapply(results, function(x) {
  #     length(authentic[authentic %in% x$coef_lasso])
  #   } )),
  #   noise_lasso = fivenum(sapply(results, function(x) {
  #     length(noise[noise %in% x$coef_lasso])
  #   } )),
  #   authentic_ridge = fivenum(sapply(results, function(x) {
  #     length(authentic[authentic %in% x$coef_ridge])
  #   } )),
  #   noise_ridge = fivenum(sapply(results, function(x) {
  #     length(noise[noise %in% x$coef_ridge])
  #   } )),
  #   authentic_net = fivenum(sapply(results, function(x) {
  #     length(authentic[authentic %in% x$coef_net])
  #   } )),
  #   noise_net = fivenum(sapply(results, function(x) {
  #     length(noise[noise %in% x$coef_net])
  #   } ))
  # )
}
