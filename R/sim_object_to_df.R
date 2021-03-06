#' sim_object_to_df creates a dataframe from the simulation object created by 
#' \code{dk_sim}
#' 
#' @return dataframe from the simulation object created by \code{dk_sim}
#' function
#' 
#' @param sim_object list object made by \code{dk_sim} function
#' @export
sim_object_to_df <- function(sim_object) {
  data.frame(
    direction = sapply(sim_object, function(x) {x$direction}),
    n = sapply(sim_object, function(x) {x$n}),
    alpha = sapply(sim_object, function(x) {x$alpha}),
    rho = sapply(sim_object, function(x) {x$rho}),
    p = sapply(sim_object, function(x) {x$p}),
    family = sapply(sim_object, function(x) {x$family}),
    link = sapply(sim_object, function(x) {x$link}),
    authentic_min_step = sapply(sim_object, function(x) {x$fivenum_authentic_step[1]}),
    authentic_1st_step = sapply(sim_object, function(x) {x$fivenum_authentic_step[2]}),
    authentic_median_step = sapply(sim_object, function(x) {x$fivenum_authentic_step[3]}),
    authentic_3rd_step = sapply(sim_object, function(x) {x$fivenum_authentic_step[4]}),
    authentic_max_step = sapply(sim_object, function(x) {x$fivenum_authentic_step[5]}),
    noise_min_step = sapply(sim_object, function(x) {x$fivenum_noise_step[1]}),
    noise_1st_step = sapply(sim_object, function(x) {x$fivenum_noise_step[2]}),
    noise_median_step = sapply(sim_object, function(x) {x$fivenum_noise_step[3]}),
    noise_3rd_step = sapply(sim_object, function(x) {x$fivenum_noise_step[4]}),
    noise_max_step = sapply(sim_object, function(x) {x$fivenum_noise_step[5]}),
    authentic_min_lasso = sapply(sim_object, function(x) {x$fivenum_authentic_lasso[1]}),
    authentic_1st_lasso = sapply(sim_object, function(x) {x$fivenum_authentic_lasso[2]}),
    authentic_median_lasso = sapply(sim_object, function(x) {x$fivenum_authentic_lasso[3]}),
    authentic_3rd_lasso = sapply(sim_object, function(x) {x$fivenum_authentic_lasso[4]}),
    authentic_max_lasso = sapply(sim_object, function(x) {x$fivenum_authentic_lasso[5]}),
    noise_min_lasso = sapply(sim_object, function(x) {x$fivenum_noise_lasso[1]}),
    noise_1st_lasso = sapply(sim_object, function(x) {x$fivenum_noise_lasso[2]}),
    noise_median_lasso = sapply(sim_object, function(x) {x$fivenum_noise_lasso[3]}),
    noise_3rd_lasso = sapply(sim_object, function(x) {x$fivenum_noise_lasso[4]}),
    noise_max_lasso = sapply(sim_object, function(x) {x$fivenum_noise_lasso[5]}),
    authentic_min_ridge = sapply(sim_object, function(x) {x$fivenum_authentic_ridge[1]}),
    authentic_1st_ridge = sapply(sim_object, function(x) {x$fivenum_authentic_ridge[2]}),
    authentic_median_ridge = sapply(sim_object, function(x) {x$fivenum_authentic_ridge[3]}),
    authentic_3rd_ridge = sapply(sim_object, function(x) {x$fivenum_authentic_ridge[4]}),
    authentic_max_ridge = sapply(sim_object, function(x) {x$fivenum_authentic_ridge[5]}),
    noise_min_ridge = sapply(sim_object, function(x) {x$fivenum_noise_ridge[1]}),
    noise_1st_ridge = sapply(sim_object, function(x) {x$fivenum_noise_ridge[2]}),
    noise_median_ridge = sapply(sim_object, function(x) {x$fivenum_noise_ridge[3]}),
    noise_3rd_ridge = sapply(sim_object, function(x) {x$fivenum_noise_ridge[4]}),
    noise_max_ridge = sapply(sim_object, function(x) {x$fivenum_noise_ridge[5]}),
    authentic_min_net = sapply(sim_object, function(x) {x$fivenum_authentic_net[1]}),
    authentic_1st_net = sapply(sim_object, function(x) {x$fivenum_authentic_net[2]}),
    authentic_median_net = sapply(sim_object, function(x) {x$fivenum_authentic_net[3]}),
    authentic_3rd_net = sapply(sim_object, function(x) {x$fivenum_authentic_net[4]}),
    authentic_max_net = sapply(sim_object, function(x) {x$fivenum_authentic_net[5]}),
    noise_min_net = sapply(sim_object, function(x) {x$fivenum_noise_net[1]}),
    noise_1st_net = sapply(sim_object, function(x) {x$fivenum_noise_net[2]}),
    noise_median_net = sapply(sim_object, function(x) {x$fivenum_noise_net[3]}),
    noise_3rd_net = sapply(sim_object, function(x) {x$fivenum_noise_net[4]}),
    noise_max_net = sapply(sim_object, function(x) {x$fivenum_noise_net[5]}),
    mse_min_step = sapply(sim_object, function(x) {x$mse_step[1]}),
    mse_1st_step = sapply(sim_object, function(x) {x$mse_step[2]}),
    mse_median_step = sapply(sim_object, function(x) {x$mse_step[3]}),
    mse_3rd_step = sapply(sim_object, function(x) {x$mse_step[4]}),
    mse_max_step = sapply(sim_object, function(x) {x$mse_step[5]}),
    mse_min_lasso = sapply(sim_object, function(x) {x$mse_lasso[1]}),
    mse_1st_lasso = sapply(sim_object, function(x) {x$mse_lasso[2]}),
    mse_median_lasso = sapply(sim_object, function(x) {x$mse_lasso[3]}),
    mse_3rd_lasso = sapply(sim_object, function(x) {x$mse_lasso[4]}),
    mse_max_lasso = sapply(sim_object, function(x) {x$mse_lasso[5]}),
    mse_min_ridge = sapply(sim_object, function(x) {x$mse_ridge[1]}),
    mse_1st_ridge = sapply(sim_object, function(x) {x$mse_ridge[2]}),
    mse_median_ridge = sapply(sim_object, function(x) {x$mse_ridge[3]}),
    mse_3rd_ridge = sapply(sim_object, function(x) {x$mse_ridge[4]}),
    mse_max_ridge = sapply(sim_object, function(x) {x$mse_ridge[5]}),
    mse_min_net = sapply(sim_object, function(x) {x$mse_net[1]}),
    mse_1st_net = sapply(sim_object, function(x) {x$mse_net[2]}),
    mse_median_net = sapply(sim_object, function(x) {x$mse_net[3]}),
    mse_3rd_net = sapply(sim_object, function(x) {x$mse_net[4]}),
    mse_max_net = sapply(sim_object, function(x) {x$mse_net[5]}),
    sims = sapply(sim_object, function(x) {x$sims}), stringsAsFactors = FALSE)
}
