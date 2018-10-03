library(stringi)
# OLS as a Probability Model
set.seed(123456) # Set the seed for reproducible results
## junk
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
  qtile <- level + (1 - level) / 2 # Compute the proper quantile
  lower.bound <- b - qt(qtile, df = df) * se # Lower bound
  upper.bound <- b + qt(qtile, df = df) * se # Upper bound 
  # Is the true parameter in the confidence interval? (yes = 1)
  true.in.ci <- ifelse(true >= lower.bound & true <= upper.bound, 1, 0)
  cp <- mean(true.in.ci) # The coverage probability
  mc.lower.bound <- cp - 1.96 * sqrt((cp * (1 - cp)) / length(b)) # Monte Carlo error  
  mc.upper.bound <- cp + 1.96 * sqrt((cp * (1 - cp)) / length(b))  
  return(list(coverage.probability = cp, # Return results
              true.in.ci = true.in.ci,
              ci = cbind(lower.bound, upper.bound),
              mc.eb = c(mc.lower.bound, mc.upper.bound)))
}

set.seed(32945) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.logit <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store
# the estimates 
b0 <- 0.2 # True value for the intercept
b1 <- 0.01 # True value for the slope
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

pdf("vignettes/images/logit-hist1.pdf")

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

pdf("vignettes/images/logit-hist2.pdf")

par(mar = c(5, 5.25, .5, .5))
hist(par.est.logit[ , 2], breaks = 25, xlim = c(.1, .9), col = "gray50",
     ylim = c(0, 200), xlab = "", ylab = "", main = "", axes = FALSE)
axis(1, at = seq(.1, .9, .1), cex.axis = 1.25)
axis(2, cex.axis = 1.25, las = 2)
title(xlab = expression(hat(beta)[1]), cex.lab = 1.5)
title(ylab = expression("Frequency"), line = 3.75, cex.lab = 1.5)
abline(v = b1, lwd = 4)
text(.75, 125, bquote("True "~beta[1]==.(b1)), cex = 1.5)
box()

dev.off()
x <- rnorm(100, 0, 1)
x <- matrix(x, nrow = 10)
y <- rnorm(10, 1, 1)
mod3 <- glm(y ~ x)
s3 <- simulate(mod3, nsim = 3)
