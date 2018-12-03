get_fivenum_coef <- function(results, authentic, noise) {
  list(
    authentic_step = fivenum(sapply(results, function(x) {
      length(authentic[authentic %in% x$coef_step])
    } )),
    noise_step = fivenum(sapply(results, function(x) {
      length(noise[noise %in% x$coef_step])
    } )),
    authentic_lasso = fivenum(sapply(results, function(x) {
      length(authentic[authentic %in% x$coef_lasso])
    } )),
    noise_lasso = fivenum(sapply(results, function(x) {
      length(noise[noise %in% x$coef_lasso])
    } )),
    authentic_ridge = fivenum(sapply(results, function(x) {
      length(authentic[authentic %in% x$coef_ridge])
    } )),
    noise_ridge = fivenum(sapply(results, function(x) {
      length(noise[noise %in% x$coef_ridge])
    } )),
    authentic_net = fivenum(sapply(results, function(x) {
      length(authentic[authentic %in% x$coef_net])
    } )),
    noise_net = fivenum(sapply(results, function(x) {
      length(noise[noise %in% x$coef_net])
    } ))
  )
}
