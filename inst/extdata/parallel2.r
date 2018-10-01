################################################################################
# Monte Carlo Simulation and Resampling Methods in Social Science              #
# Thomas M. Carsey and Jeffrey J. Harden                                       #
# Chapter 6 File (Parallel Processing 2 of 2)                                  #
# Last update: 1/31/13                                                         #
################################################################################

# Negative Binomial vs. Zero-Inflated Negative Binomial (parallel processing)
library(pscl)
library(snow) 
library(doSNOW) 
library(foreach)

cl.tmp <- makeCluster(4) 
registerDoSNOW(cl.tmp) 

# Zero-inflated negative binomial random number generator
rzinbinom <- function(n, mu, size, zprob){ 
ifelse(runif(n) < zprob, 0, rnbinom(n, size = size, mu = mu))
}

# Simulation function
zinb.sim <- function(n = 1000){
require(pscl)
par.est.zinb <- matrix(NA, nrow = 1, ncol = 4) # Empty matrix to store the
                                               # estimates 
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X

# Generate data with a zero-inflation component
Y.zi <- rzinbinom(n, mu = exp(b0 + b1*X), size = .5,
zprob = exp(b0 + b1*X)/(1 + exp(b0 + b1*X)))
# Generate data with no zero-inflation component
Y.nozi <- rzinbinom(n, mu = exp(b0 + b1*X), size = .5, zprob = 0) 
model.nb1 <- glm.nb(Y.zi ~ X) # Standard negative binomial
model.nb2 <- glm.nb(Y.nozi ~ X)
model.zinb1 <- zeroinfl(Y.zi ~ X | X, dist = "negbin") # Zero-inflated model
model.zinb2 <- zeroinfl(Y.nozi ~ X | X, dist = "negbin") 
# Store the estimates of the coefficient on X (count equation)
par.est.zinb[ , 1] <- model.nb1$coef[2] # Standard NB, with ZI
par.est.zinb[ , 2] <- model.nb2$coef[2] # Standard NB, no ZI
par.est.zinb[ , 3] <- as.numeric(model.zinb1$coef$count[2]) # ZI NB, with ZI
par.est.zinb[ , 4] <- as.numeric(model.zinb2$coef$count[2]) # ZI NB, no ZI
return(par.est.zinb)
}

# Parallel Processing
set.seed(2837) # Set the seed for reproducible results
reps <- 1000 # Set the number of repetitions

start.time <- Sys.time()
results <- foreach(i = 1:reps, .combine = rbind) %dopar% {
zinb.sim(n = 1000)
}

end.time <- Sys.time()
end.time - start.time