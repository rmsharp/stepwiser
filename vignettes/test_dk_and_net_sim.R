library(glmnet)
library(stringi)
library(rmsutilityr)
library(stepwiser)
file <- get_dated_filename("dk_and_net_sim.csv")
sims <- 1
directions <- "forward"
betas <- c(1, 2, 3, -1, -2, -3)
n_values <- c(30, 60, 90)
n_values <- 30
rho_values <- c(0, sqrt(0.1), sqrt(0.2), sqrt(0.4))
rho_values <- 0
predictors <- c(12, 18, 24)
predictors <- 12
MFWER <- 0.15
alpha_values <- c(0.5, 0.15, 1 - (1 - MFWER)^(1 / predictors), 0.05)
alpha_values <- 0.05
sim_object <- dk_and_net_sim(file = file, directions = directions, betas, 
                             n_values, alpha_values, rho_values, predictors, 
                             net_alpha = 0.5, weight = 1, nlambda = 100,
                             sims = sims, rnorm, 0, 0.2)
