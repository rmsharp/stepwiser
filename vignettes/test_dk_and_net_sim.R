pkgs <- list("stringi", "rmsutilityr", "glmnet", "doParallel", "foreach", 
             "MASS", "pROC", "stepwiser")
lapply(pkgs, library, character.only = TRUE)
registerDoParallel(cores = 4)
file <- get_dated_filename("dk_and_net_sim.csv")
sims <- 20
directions <- "forward"
betas <- c(1, 2, 3, -1, -2, -3)
n_values <- c(300, 900)
#n_values <- 300
rho_values <- c(0, sqrt(0.1), sqrt(0.2), sqrt(0.4))
#rho_values <- 0
predictors <- c(12, 24)
#predictors <- 24
MFWER <- 0.15
alpha_values <- c(0.5, 0.15, 1 - (1 - MFWER)^(1 / predictors), 0.05)
#alpha_values <- 0.15
#alpha_values <- 0.05
weight <- 1
nlambda <- 100
sim_object <- dk_and_net_sim(file = file, directions = directions, betas, 
                             n_values, alpha_values, rho_values, predictors, 
                             weight = weight, nlambda = nlambda,
                             sims = sims, rnorm, 0, 0.2)
