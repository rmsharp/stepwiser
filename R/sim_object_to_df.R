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
    alpha = sapply(sim_object, function(x) {x$alpha}),
    n = sapply(sim_object, function(x) {x$n}),
    rho = sapply(sim_object, function(x) {x$rho}),
    p = sapply(sim_object, function(x) {x$p}),
    family = sapply(sim_object, function(x) {x$family}),
    link = sapply(sim_object, function(x) {x$link}),
    authentic_min = sapply(sim_object, function(x) {x$fivenum_authentic[1]}),
    authentic_1st = sapply(sim_object, function(x) {x$fivenum_authentic[2]}),
    authentic_median = sapply(sim_object, function(x) {x$fivenum_authentic[3]}),
    authentic_3rd = sapply(sim_object, function(x) {x$fivenum_authentic[4]}),
    authentic_max = sapply(sim_object, function(x) {x$fivenum_authentic[5]}),
    noise_min = sapply(sim_object, function(x) {x$fivenum_noise[1]}),
    noise_1st = sapply(sim_object, function(x) {x$fivenum_noise[2]}),
    noise_median = sapply(sim_object, function(x) {x$fivenum_noise[3]}),
    noise_3rd = sapply(sim_object, function(x) {x$fivenum_noise[4]}),
    noise_max = sapply(sim_object, function(x) {x$fivenum_noise[5]}),
    sims = sapply(sim_object, function(x) {x$sims}), stringsAsFactors = FALSE)
}
