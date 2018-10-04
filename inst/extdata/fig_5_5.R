## Under the NULL
plot(function(x) dnorm(x, mean=0, sd = 1, log = F), -4, 4, 
     xlab="Estimated effect", ylab = "Density")
lines(x=c(0,0), y=c(0,.5), lty=1)
x95 <- qnorm(p=(1-0.025), mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
x995 <- qnorm(p=(1-0.0025), mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

# Polygon for p<.05
xpol <- seq(x95,4,by=0.01)
ypol <- dnorm(x=xpol, mean=0, sd = 1, log = F)
xpol <- c(x95,xpol,4)
ypol <- c(0,ypol,0)
polygon(xpol,ypol, col = "gray")
# lower tail
xpol <- seq(-x95,-4,by=-0.01)
ypol <- dnorm(x=xpol, mean=0, sd = 1, log = F)
xpol <- c(-x95,xpol,-4)
ypol <- c(0,ypol,0)
polygon(xpol,ypol, col = "gray")

##############
## mean = 1
## range -3, 5
plot(function(x) dnorm(x, mean=1, sd = 1, log = F), -3, 5, 
     xlab="Estimated effect", ylab = "Density")
lines(x=c(1,1), y=c(0,.5), lty=2)

x95 <- qnorm(p=(1-0.025), mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
x995 <- qnorm(p=(1-0.0025), mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

# Polygon for p<.05
xpol <- seq(x95,5,by=0.01)
ypol <- dnorm(x=xpol, mean=1, sd = 1, log = F)
xpol <- c(x95,xpol,5)
ypol <- c(0,ypol,0)
polygon(xpol,ypol, col = "gray")
# lower tail
xpol <- seq(-x95,-3,by=-0.01)
ypol <- dnorm(x=xpol, mean=1, sd = 1, log = F)
xpol <- c(-x95,xpol,-3)
ypol <- c(0,ypol,0)
polygon(xpol,ypol, col = "gray")