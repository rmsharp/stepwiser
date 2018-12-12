#' Simulation software loosely based on [@derksen_keselman1992]
#' 
#' @description Primary Data Structures --- Simulation Object
#'
#' The simulation object is the output from \code{dk_sim()} and contains the
#' results of the simulation.
#'
#' A Pedigree is a data.frame with the following columns:
#' The possible columns are as follows:
#' \itemize{
#' \item{result} {-- list with variable selections and mean squared error 
#' values from each simulated data set}
#' }
#'
#' Simulation function
#'
#' \itemize{
#' \item{\link{dk_sim}} {--- Used to simulate GLM analyses with various 
#' stepwise, lasso, ridge, and elastic net variable selection techniques.}
#' }
#' 
#'
#' @docType package
#' @name stepwiser
NULL
