################################################################################
# Monte Carlo Simulation and Resampling Methods for Social Science             #
# Thomas M. Carsey and Jeffrey J. Harden                                       #
# Chapter 6 File                                                               #
# Last update: 2/28/13                                                         #
################################################################################
# Set the working directory
setwd("C:/Users/jjharden/Dropbox/Research/Sim Book")

# OLS as a Probability Model
set.seed(123456) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store the
                                             # estimates 
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X

for(i in 1:reps){ # Start the loop
Y <- rnorm(n, b0 + b1*X, 1) # The true DGP, Y ~ N(mu, sigma)
model <- lm(Y ~ X) # Estimate OLS model
vcv <- vcov(model) # Variance-covariance matrix
par.est[i, 1] <- model$coef[1] # Put the estimate for the intercept
                               # in the first column
par.est[i, 2] <- model$coef[2] # Put the estimate for the coefficient on
                               # X in the second column
par.est[i, 3] <- sqrt(diag(vcv)[1]) # SE of the intercept
par.est[i, 4] <- sqrt(diag(vcv)[2]) # SE of the coefficient on X
} # End the loop

# Logit
# Inverse Logit Function
inv.logit <- function(p){
return(exp(p)/(1 + exp(p)))
}

# CP Function
coverage <- function(b, se, true, level = .95, df = Inf){ # Estimate, 
                                                          # standard error,
                                                          # true parameter, 
                                                          # confidence level, 
                                                          # and df  
qtile <- level + (1 - level)/2 # Compute the proper quantile
lower.bound <- b - qt(qtile, df = df)*se # Lower bound
upper.bound <- b + qt(qtile, df = df)*se # Upper bound 
# Is the true parameter in the confidence interval? (yes = 1)
true.in.ci <- ifelse(true >= lower.bound & true <= upper.bound, 1, 0)
cp <- mean(true.in.ci) # The coverage probability
mc.lower.bound <- cp - 1.96*sqrt((cp*(1 - cp))/length(b)) # Monte Carlo error  
mc.upper.bound <- cp + 1.96*sqrt((cp*(1 - cp))/length(b))  
return(list(coverage.probability = cp, # Return results
            true.in.ci = true.in.ci,
            ci = cbind(lower.bound, upper.bound),
            mc.eb = c(mc.lower.bound, mc.upper.bound)))
}

set.seed(32945) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.logit <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store
                                                   # the estimates 
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X

for(i in 1:reps){ # Start the loop
Y <- rbinom(n, 1, inv.logit(b0 + b1*X)) # The true DGP, Bernoulli trials
model <- glm(Y ~ X, family = binomial (link = logit)) # Estimate logit model
vcv <- vcov(model) # Variance-covariance matrix
par.est.logit[i, 1] <- model$coef[1] # Put the estimate for the 
                                     # intercept in the first column
par.est.logit[i, 2] <- model$coef[2] # Put the estimate for the coefficient 
                                     # on X in the second column
par.est.logit[i, 3] <- sqrt(diag(vcv)[1]) # SE of the intercept
par.est.logit[i, 4] <- sqrt(diag(vcv)[2]) # SE of the coefficient on X
} # End the loop

cp.beta0.logit <- coverage(par.est.logit[ , 1], par.est.logit[ , 3], b0,
 df = n - model$rank)
cp.beta1.logit <- coverage(par.est.logit[ , 2], par.est.logit[ , 4], b1,
 df = n - model$rank)

pdf("logit-hist1.pdf")

par(mar = c(5, 5.25, .5, .5))
hist(par.est.logit[ , 1], breaks = 25, col = "gray50", ylim = c(0, 150),
 xlab = "", ylab = "", main = "", axes = FALSE)
axis(1, cex.axis = 1.25)
axis(2, cex.axis = 1.25, las = 2)
title(xlab = expression(hat(beta[0])), cex.lab = 1.5)
title(ylab = expression("Frequency"), line = 3.75, cex.lab = 1.5)
abline(v = b0, lwd = 4)
text(.05, 70, expression("True"~beta[0]~"= 0.20"), cex = 1.5)
box()

dev.off()

pdf("logit-hist2.pdf")

par(mar = c(5, 5.25, .5, .5))
hist(par.est.logit[ , 2], breaks = 25, xlim = c(.1, .9), col = "gray50",
 ylim = c(0, 200), xlab = "", ylab = "", main = "", axes = FALSE)
axis(1, at = seq(.1, .9, .1), cex.axis = 1.25)
axis(2, cex.axis = 1.25, las = 2)
title(xlab = expression(hat(beta)[1]), cex.lab = 1.5)
title(ylab = expression("Frequency"), line = 3.75, cex.lab = 1.5)
abline(v = b1, lwd = 4)
text(.75, 125, expression("True"~beta[1]~"= 0.50"), cex = 1.5)
box()

dev.off()

# Probit
set.seed(3295255) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.probit <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store
                                                   # the estimates 
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X

for(i in 1:reps){ # Start the loop
Y <- rbinom(n, 1, pnorm(b0 + b1*X)) # The true DGP, Bernoulli trials
model <- glm(Y ~ X, family = binomial (link = probit)) # Estimate probit model
vcv <- vcov(model) # Variance-covariance matrix
par.est.probit[i, 1] <- model$coef[1] # Put the estimate for the 
                                      # intercept in the first column
par.est.probit[i, 2] <- model$coef[2] # Put the estimate for the coefficient 
                                      # on X in the second column
par.est.probit[i, 3] <- sqrt(diag(vcv)[1]) # SE of the intercept
par.est.probit[i, 4] <- sqrt(diag(vcv)[2]) # SE of the coefficient on X
} # End the loop

mean(par.est.probit[ , 1]) # Mean of intercept estimates
mean(par.est.probit[ , 2]) # Mean of coefficient on X estimates
# Coverage probability for the intercept
coverage(par.est.probit[ , 1], par.est.probit[ , 3], b0,
 df = n - model$rank)$coverage.probability
# Coverage probability for the coefficient on X
coverage(par.est.probit[ , 2], par.est.probit[ , 4], b1,
 df = n - model$rank)$coverage.probability

# Ordered Models
library(MASS)
set.seed(8732) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.oprobit <- matrix(NA, nrow = reps, ncol = 2) # Empty matrices to store
taus.oprobit <- matrix(NA, nrow = reps, ncol = 3)    # the estimates 
b0 <- 0 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
X <- rnorm(n, 0, 1) # Create a sample of n observations on the 
                    # independent variable X

XB <- b0 + b1*X # Systematic component
sd.error <- 1 # SD of the error of the unobserved Y*
# Define the true cutpoints
tau1 <- qnorm(.1, mean = mean(XB), sd = sqrt(var(XB) + sd.error^2)) 
tau2 <- qnorm(.5, mean = mean(XB), sd = sqrt(var(XB) + sd.error^2)) 
tau3 <- qnorm(.9, mean = mean(XB), sd = sqrt(var(XB) + sd.error^2)) 

for(i in 1:reps){ # Start the loop
Y.star <- rnorm(n, XB, sd.error) # The unobserved Y*
Y <- rep(NA, n) # Define Y as a vector of NAs with length n
Y[Y.star < tau1] <- 1 # Set Y equal to a value according to Y.star
Y[Y.star >= tau1 & Y.star < tau2] <- 2
Y[Y.star >= tau2 & Y.star < tau3] <- 3
Y[Y.star >= tau3] <- 4
# Estimate ordered model
model <- polr(as.ordered(Y) ~ X, method = "probit", Hess = TRUE) 
vcv <- vcov(model) # Variance-covariance matrix
par.est.oprobit[i, 1] <- model$coef[1] # Put the estimate for the coefficient
                                       # on X in the second column
par.est.oprobit[i, 2] <- sqrt(diag(vcv)[1]) # SE of the coefficient on X
taus.oprobit[i, ] <- model$zeta
cat("Just completed iteration", i, "\n")
} # End the loop

pdf("ystar.pdf")

par(mar = c(5, 5.25, .5, .5))
plot(Y.star, Y, xlim = c(-3, 3), xlab = "", ylab = "", main = "",
 axes = FALSE, pch = 19)
axis(1, at = seq(-3, 3, 1), cex.axis = 1.25)
axis(2, at = 1:4, cex.axis = 1.25, las = 2)
title(xlab = expression("Y* (Unobserved/Continuous)"), cex.lab = 1.5)
title(ylab = expression("Y (Observed/Categorical)"), line = 3.75,
 cex.lab = 1.5)
abline(v = c(tau1, tau2, tau3), lwd = 2, lty = 3)
text(tau1 - .25, 3.5, expression(tau[1]), cex = 2)
text(tau2 - .25, 3.5, expression(tau[2]), cex = 2)
text(tau3 - .25, 3.5, expression(tau[3]), cex = 2)
box()

dev.off()

mean(par.est.oprobit[ , 1]) # Mean of coefficient on X estimates
# Compare the actual taus to the means of the tau estimates
data.frame(True = c(tau1, tau2, tau3), Estimated = apply(taus.oprobit, 2, mean))
# Coverage probability for the coefficient on X
coverage(par.est.oprobit[ , 1], par.est.oprobit[ , 2], b1,
 df = n - length(c(coef(model), model$zeta)))$coverage.probability

# Unordered Models
library(Zelig)
set.seed(45262) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.mnl <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store
                                                 # the estimates 
b0A <- .2 # True values for the intercepts
b0B <- -.2
b1A <- .5 # True values for the slopes
b1B <- .75
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X
# Compute the probabilities of each outcome based on the DGP
pA <- exp(b0A + b1A*X)/(1 + exp(b0A + b1A*X) + exp(b0B + b1B*X))
pB <- exp(b0B + b1B*X)/(1 + exp(b0A + b1A*X) + exp(b0B + b1B*X))
pC <- 1 - pA - pB

for(i in 1:reps){ # Start the loop
Y <- rep(NA, n) # Define Y as a vector of NAs with length n
for(j in 1:n){ # Create the dependent variable in another loop
Y[j] <- sample(c("A", "B", "C"), 1, replace = TRUE,
 prob = c(pA[j], pB[j], pC[j]))
}

# Estimate a MNL model
model <- zelig(as.factor(Y) ~ X, model = "mlogit",
 data = data.frame(Y, X), cite = FALSE)
vcv <- vcov(model) # Variance-covariance matrix
par.est.mnl[i, 1] <- coefficients(model)[3] # Coefficient on X, outcome 1 
par.est.mnl[i, 2] <- coefficients(model)[4] # Coefficient on X, outcome 2
par.est.mnl[i, 3] <- sqrt(diag(vcv)[3]) # SE of coefficient on X, outcome 1
par.est.mnl[i, 4] <- sqrt(diag(vcv)[4]) # SE of coefficient on X, outcome 2
cat("Just completed iteration", i, "\n")
} # End the loop

# Mean of coefficients on X estimates
mean(par.est.mnl[ , 1]) # Outcome 1
mean(par.est.mnl[ , 2]) # Outcome 2

# Coverage probabilities for the coefficients on X
# Outcome 1
coverage(par.est.mnl[ , 1], par.est.mnl[ , 3], b1A,
 df = n - length(coef(model)))$coverage.probability
# Outcome 2
coverage(par.est.mnl[ , 2], par.est.mnl[ , 4], b1B,
 df = n - length(coef(model)))$coverage.probability

# Ordered vs. MNL
library(Zelig)
set.seed(99999)

reps <- 1000 # Set the number of repetitions at the top of the script
d.pp <- array(NA, c(4, 3, reps)) # Empty array to store 
                                 # simulated change in probabilities

# Ordered logit model DGP
b0 <- 0 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X

# MNL model DGP
b0A <- .2 # True values for the intercepts
b0B <- -.2
b1A <- .5 # True values for the slopes
b1B <- .75
n <- 1000 # Sample size

# Compute the probabilities of each outcome based on the DGP
pA <- exp(b0A + b1A*X)/(1 + exp(b0A + b1A*X) + exp(b0B + b1B*X))
pB <- exp(b0B + b1B*X)/(1 + exp(b0A + b1A*X) + exp(b0B + b1B*X))
pC <- 1 - pA - pB
 
for(i in 1:reps){
# Ordered dependent variable
Y.star <- rlogis(n, b0 + b1*X, 1) # The unobserved Y*
# Define the true cutpoints
tau1 <- quantile(Y.star, .25) 
tau2 <- quantile(Y.star, .75) 
Y.o <- rep(NA, n) # Define Y as a vector of NAs with length n
Y.o[Y.star < tau1] <- 1 # Set Y equal to a value according to Y.star
Y.o[Y.star >= tau1 & Y.star < tau2] <- 2
Y.o[Y.star >= tau2] <- 3
# Ordered data
o.data <- data.frame(Y.o, X) # Put the data in a data frame

# Unordered dependent variable
Y.m <- rep(NA, n) # Define Y as a vector of NAs with length n
for(j in 1:n){ # Create the dependent variable in another loop
Y.m[j] <- sample(1:3, 1, replace = TRUE, prob = c(pA[j], pB[j], pC[j]))
}
# Unordered data
m.data <- data.frame(Y.m, X) # Put the data in a data frame

# Estimate the models with the ordered dependent variable
o.correct <- zelig(as.ordered(Y.o) ~ X, model = "ologit",
 data = o.data, cite = FALSE)
m.incorrect <- zelig(as.factor(Y.o) ~ X, model = "mlogit",
 data = o.data, cite = FALSE)

# Estimate the models with the multinomial dependent variable
m.correct <- zelig(as.factor(Y.m) ~ X, model = "mlogit",
 data = m.data, cite = FALSE)
o.incorrect <- zelig(as.ordered(Y.m) ~ X, model = "ologit",
 data = m.data, cite = FALSE)

# Set X to its minimum and maximum for each model
x.oc <- setx(o.correct, X = min(X)) # For o.correct
x.oc1 <- setx(o.correct, X = max(X)) 

x.mi <- setx(m.incorrect, X = min(X)) # For m.incorrect
x.mi1 <- setx(m.incorrect, X = max(X)) 

x.mc <- setx(m.correct, X = min(X)) # For m.correct
x.mc1 <- setx(m.correct, X = max(X))
 
x.oi <- setx(o.incorrect, X = min(X)) # For o.incorrect
x.oi1 <- setx(o.incorrect, X = max(X)) 

# Compute the change in expected probabilities of falling in each category
# when moving from the minimum to the maximum of X
sim.oc <- sim(o.correct, x = x.oc, x1 = x.oc1)$qi$fd
sim.mi <- sim(m.incorrect, x = x.mi, x1 = x.mi1)$qi$fd
sim.mc <- sim(m.correct, x = x.mc, x1 = x.mc1)$qi$fd
sim.oi <- sim(o.incorrect, x = x.oi, x1 = x.oi1)$qi$fd
d.pp[1, , i] <- apply(sim.oc, 2, mean)
d.pp[2, , i] <- apply(sim.mi, 2, mean)
d.pp[3, , i] <- apply(sim.mc, 2, mean)
d.pp[4, , i] <- apply(sim.oi, 2, mean)
cat("Just completed iteration", i, "of", reps, "\n")
}

# Compute the average change in probability for each of the four models
dpp.means <- rbind(apply(d.pp[1, , ], 1, mean), apply(d.pp[2, , ], 1, mean), apply(d.pp[3, , ], 1, mean), apply(d.pp[4, , ], 1, mean))

# Compute the SD of the change in probability for each of the four models
dpp.sds <- rbind(apply(d.pp[1, , ], 1, sd), apply(d.pp[2, , ], 1, sd), apply(d.pp[3, , ], 1, sd), apply(d.pp[4, , ], 1, sd))

# LaTeX table
library(xtable)
results <- rbind(dpp.means, dpp.sds)

xtable(results, digits = 4, caption = "Means and Standard Deviations of the Change in expected probability for Each Category", label = "ord-unord-results", align = "lrrr")

# Count Models
# Poisson 
set.seed(3759) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.pois <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store the
                                                  # estimates 
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X

for(i in 1:reps){ # Start the loop
Y <- rpois(n, exp(b0 + b1*X)) # The true DGP
model <- glm(Y ~ X, family = "poisson") # Estimate Poisson model
vcv <- vcov(model) # Variance-covariance matrix
par.est.pois[i, 1] <- model$coef[1] # Put the estimate for the intercept
                                    # in the first column
par.est.pois[i, 2] <- model$coef[2] # Put the estimate for the coefficient on
                                    # X in the second column
par.est.pois[i, 3] <- sqrt(diag(vcv)[1]) # SE of the intercept
par.est.pois[i, 4] <- sqrt(diag(vcv)[2]) # SE of the coefficient on X
} # End the loop

# Means of the coefficient estimates
mean(par.est.pois[ , 1]) # Intercept
mean(par.est.pois[ , 2]) # Coefficient on X

# Coverage probabilities 
# Intercept
coverage(par.est.pois[ , 1], par.est.pois[ , 3], b0,
 df = n - model$rank)$coverage.probability
# Coefficient on X
coverage(par.est.pois[ , 2], par.est.pois[ , 4], b1,
 df = n - model$rank)$coverage.probability

pdf("od1.pdf")

par(mar = c(5, 5.25, .5, .5)) 
plot(1:n, Y, ylim = c(0, 20), col = "gray50", xlab = "", ylab = "",
 main = "", axes = FALSE)
axis(1, cex.axis = 1.25)
axis(2, at = 0:20, cex.axis = 1.25, las = 2)
title(xlab = expression("Observations"), cex.lab = 1.5)
title(ylab = expression("Y"), line = 3.75, cex.lab = 1.5)
abline(h = mean(Y), lwd = 2)
abline(h = var(Y), lwd = 2, lty = 2)
box()
legend("topleft", bty = "n", c(expression("Mean of Y"),
 expression("Variance of Y")), lty = c(1, 2), lwd = 2, cex = 1.5)

dev.off()

# Poisson vs. Negative Binomial
library(MASS)
set.seed(763759) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.pnb <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store the
                                                 # estimates 
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X
                            
for(i in 1:reps){
Y <- rnbinom(n, size = .5, mu = exp(b0 + b1*X)) # Generate data with
                                                # overdispersion
model.p <- glm(Y ~ X, family = "poisson") # Estimate Poisson model
model.nb <- glm.nb(Y ~ X) # Estimate NB model
vcv.p <- vcov(model.p) # Variance-covariance matrices
vcv.nb <- vcov(model.nb)
par.est.pnb[i, 1] <- model.p$coef[2]  # Store the results
par.est.pnb[i, 2] <- model.nb$coef[2] 
par.est.pnb[i, 3] <- sqrt(diag(vcv.p)[2])    
par.est.pnb[i, 4] <- sqrt(diag(vcv.nb)[2])
cat("Completed", i, "of", reps, "\n")
}

# Means of the coefficient on X estimates
mean(par.est.pnb[ , 1]) # Poisson estimates
mean(par.est.pnb[ , 2]) # NB estimates

# MSE
mean((par.est.pnb[ , 1])^2) # Poisson MSE
mean((par.est.pnb[ , 2])^2) # NB MSE

# Coverage probabilities 
# Poisson SEs
coverage(par.est.pnb[ , 1], par.est.pnb[ , 3], b1,
 df = n - model.p$rank)$coverage.probability
# NB SEs
coverage(par.est.pnb[ , 2], par.est.pnb[ , 4], b1,
 df = n - model.nb$rank)$coverage.probability

pdf("od2.pdf")

par(mar = c(5, 5.25, .5, .5)) 
plot(1:n, Y, ylim = c(0, 20), col = "gray50", xlab = "", ylab = "",
 main = "", axes = FALSE)
axis(1, cex.axis = 1.25)
axis(2, at = 0:20, cex.axis = 1.25, las = 2)
title(xlab = expression("Observations"), cex.lab = 1.5)
title(ylab = expression("Y"), line = 3.75, cex.lab = 1.5)
abline(h = mean(Y), lwd = 2)
abline(h = var(Y), lwd = 2, lty = 2)
box()
legend("topleft", bty = "n", c(expression("Mean of Y"),
 expression("Variance of Y")), lty = c(1, 2), lwd = 2, cex = 1.5)

dev.off()

# Negative Binomial vs. Zero-Inflated Negative Binomial
library(pscl)
# Zero-inflated negative binomial random number generator
rzinbinom <- function(n, mu, size, zprob){ 
ifelse(rbinom(n, 1, zprob) == 1, 0, rnbinom(n, size = size, mu = mu))
}

set.seed(2837) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.zinb <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store
                                                  # the estimates 
b0z <- -.8 # True value for the inflation intercept
b1z <- .3 # True value for the inflation slope                                                  
b0c <- .2 # True value for the count intercept
b1c <- .5 # True value for the count slope
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X
Z <- rnorm(n, X, 1) # Inflation independent variable

for(i in 1:reps){
# Generate data with a zero-inflation component
Y.zi <- rzinbinom(n, mu = exp(b0c + b1c*X), size = .5,
zprob = exp(b0z + b1z*Z)/(1 + exp(b0z + b1z*Z)))

# Generate data with no zero-inflation component
Y.nozi <- rzinbinom(n, mu = exp(b0c + b1c*X), size = .5, zprob = 0) 
model.nb1 <- glm.nb(Y.zi ~ X) # Standard negative binomial
model.nb2 <- glm.nb(Y.nozi ~ X)
model.zinb1 <- zeroinfl(Y.zi ~ X | Z, dist = "negbin") # Zero-inflated model
model.zinb2 <- zeroinfl(Y.nozi ~ X | Z, dist = "negbin") 

# Store the estimates of the coefficient on X (count equation)
par.est.zinb[i, 1] <- model.nb1$coef[2] # Standard NB, with ZI
par.est.zinb[i, 2] <- model.nb2$coef[2] # Standard NB, no ZI
par.est.zinb[i, 3] <- as.numeric(model.zinb1$coef$count[2]) # ZI NB, with ZI
par.est.zinb[i, 4] <- as.numeric(model.zinb2$coef$count[2]) # ZI NB, no ZI
cat("Completed", i, "of", reps, "\n")
}

pdf("zinb-zi.pdf")

par(mar = c(5, 5.25, .5, .5)) 
plot(density(par.est.zinb[ , 1]), xlim = c(-.5, 1.5), ylim = c(0, 3.5),
 lwd = 3, xlab = "", ylab = "", main = "", axes = FALSE)
lines(density(par.est.zinb[ , 3]), lwd = 3, lty = 2)
axis(1, cex.axis = 1.25)
axis(2, cex.axis = 1.25, las = 2)
title(xlab = expression(hat(beta[1])), cex.lab = 1.5)
title(ylab = expression("Density"), line = 3.75, cex.lab = 1.5)
abline(v = b1, lwd = 2)
text(1, 2, expression("True"~beta[1]~"= 0.50"), cex = 1.5)
box()
legend("topright", bty = "n", c(expression("Standard NB"),
 expression("ZINB")), lty = c(1, 2), lwd = 3, cex = 1.5)

dev.off()

pdf("zinb-nozi.pdf")

par(mar = c(5, 5.25, .5, .5)) 
plot(density(par.est.zinb[ , 2]), xlim = c(-.5, 1.5), ylim = c(0, 5),
 lwd = 3, xlab = "", ylab = "", main = "", axes = FALSE)
lines(density(par.est.zinb[ , 4]), lwd = 3, lty = 2)
axis(1, cex.axis = 1.25)
axis(2, cex.axis = 1.25, las = 2)
title(xlab = expression(hat(beta[1])), cex.lab = 1.5)
title(ylab = expression("Density"), line = 3.75, cex.lab = 1.5)
abline(v = b1, lwd = 2)
text(1, 2, expression("True"~beta[1]~"= 0.50"), cex = 1.5)
box()
legend("topright", bty = "n", c(expression("Standard NB"),
 expression("ZINB")), lty = c(1, 2), lwd = 3, cex = 1.5)

dev.off()

# Duration Models (Cox PH)
library(survival)
library(coxrobust)

set.seed(4679) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.cox <- matrix(NA, nrow = reps, ncol = 3) # Empty matrix to store
                                                 # the estimates 
b1 <- .5 # True value for the slope
n <- 1000 # Sample size
X <- runif(n, -1, 1) # Create a sample of n observations on the 
                     # independent variable X
Z <- runif(n, -1, 1) # Create an omitted variable

for(i in 1:reps){ # Start the loop
Y <- rexp(n, exp(b1*X + Z)) # Generate survival times
Ys <- Surv(Y, event = rep(1, n))
model.plm <- coxph(Ys ~ X, method = "breslow") # Standard PLM estimator
model.irr <- coxr(Ys ~ X, trunc = .95) # IRR, downweighting outliers (5%)
model.irr2 <- coxr(Ys ~ X, trunc = .8) # IRR, even more downweighting (20%)
par.est.cox[i, 1] <- model.plm$coef[1] # Store the estimates
par.est.cox[i, 2] <- model.irr$coef[1]
par.est.cox[i, 3] <- model.irr2$coef[1]
cat("Completed", i, "of", reps, "\n")
}

apply(par.est.cox, 2, mean)
apply(par.est.cox, 2, sd)

mean((par.est.cox[ , 1] - b1)^2)
mean((par.est.cox[ , 2] - b1)^2)
mean((par.est.cox[ , 3] - b1)^2)

pdf("cox.pdf")

par(mar = c(5, 5.25, .5, .5)) 
plot(density(par.est.cox[ , 1]), xlim = c(.15, .65), lwd = 3, xlab = "",
 ylab = "", main = "", axes = FALSE)
lines(density(par.est.cox[ , 2]), lwd = 3, lty = 2)
lines(density(par.est.cox[ , 3]), lwd = 3, lty = 3)
axis(1, cex.axis = 1.25)
axis(2, cex.axis = 1.25, las = 2)
title(xlab = expression(hat(beta[1])), cex.lab = 1.5)
title(ylab = expression("Density"), line = 3.75, cex.lab = 1.5)
abline(v = b1, lwd = 2)
text(.58, 6, expression("True"~beta[1]~"= 0.50"), cex = 1.5)
box()
legend("topleft", bty = "n", c(expression("PLM"), expression("IRR 5%"),
 expression("IRR 20%")), lty = c(1, 2, 3), lwd = 3, cex = 1.5)

dev.off()

# Computational Issues
source("parallel1.r")
source("parallel2.r")
