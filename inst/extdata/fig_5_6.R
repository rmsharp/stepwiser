# Illustrate model performance development / test / external: Fig 5.6
library(rms)

# create x vars
n <- 100000
x1  <- rnorm(n,sd=1)
x2  <- rnorm(n,sd=.9)
x3  <- rnorm(n,sd=.8)
x4  <- rnorm(n,sd=.7)
x5  <- rnorm(n,sd=.6)
x6  <- rnorm(n,sd=.5)
x7  <- rnorm(n,sd=.4)
x8  <- rnorm(n,sd=.3)
x9  <- rnorm(n,sd=.2)
x10  <- rnorm(n,sd=.1)
y <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +rnorm(n,sd=3) # y in developement data
y2 <- .5*x1 + 1.5*x2 + .5*x3 + 1.5*x4 + 0.5*x5 + 1.5*x6 + 0.5*x7 + 1.5*x8 + 0.5*x9 + 1.5*x10 +rnorm(n,sd=3)

datax <- as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,y,y2))
datax[1:5,]
describe(y)
describe(y2)

colnames(datax) <- Cs(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,Y,Y2)
# gold standard fits
ols(y~x1)
ols(y~x1 + x2 + x3 + x4 + x5 + x6 + x6 + x7 + x8 + x9 + x9 + x10)
ols(y2~x1 + x2 + x3 + x4 + x5 + x6 + x6 + x7 + x8 + x9 + x9 + x10)

# function for MSE
MSE <- function(y, fit,newdata)   {mean((y-predict(fit,newdata=newdata))^2)}

#######################
## Start simulations ##
#######################
sim <- 1000
nstudy  <- 500
resultsmat  <- matrix(nrow=sim,ncol=75)

for (i in 1:sim) {
  cat("\nSimulation #", i)
  
  # create x vars
  n <- 10000
  x1  <- rnorm(n,sd=1)
  x2  <- rnorm(n,sd=.9)
  x3  <- rnorm(n,sd=.8)
  x4  <- rnorm(n,sd=.7)
  x5  <- rnorm(n,sd=.6)
  x6  <- rnorm(n,sd=.5)
  x7  <- rnorm(n,sd=.4)
  x8  <- rnorm(n,sd=.3)
  x9  <- rnorm(n,sd=.2)
  x10  <- rnorm(n,sd=.1)
  x11  <- rnorm(n,sd=1)
  x12  <- rnorm(n,sd=.9)
  x13  <- rnorm(n,sd=.8)
  x14  <- rnorm(n,sd=.7)
  x15  <- rnorm(n,sd=.6)
  x16  <- rnorm(n,sd=.5)
  x17  <- rnorm(n,sd=.4)
  x18  <- rnorm(n,sd=.3)
  x19  <- rnorm(n,sd=.2)
  x20  <- rnorm(n,sd=.1)
  y <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
    x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + rnorm(n,sd=3)
  y2 <- .5*x1 + 1.5*x2 + .5*x3 + 1.5*x4 + 0.5*x5 + 1.5*x6 + 0.5*x7 + 1.5*x8 + 0.5*x9 + 1.5*x10 +
    .5*x11 + 1.5*x12 + .5*x13 + 1.5*x14 + 0.5*x15 + 1.5*x16 + 0.5*x17 + 1.5*x18 + 0.5*x19 + 1.5*x20 + rnorm(n,sd=3)
  
  datax <- as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,y,y2))
  colnames(datax) <- Cs(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,Y,Y2)
  
  j <- sample(x=1:n, size=nstudy)
  dataj <- datax[j,]
  datamj <- datax[-j,]
  Yj <- datax$Y[j]
  Ymj <- datax$Y[-j]
  Y2mj <- datax$Y2[-j]
  
  fit1  <- ols(Y~X1, data=dataj)
  fit2  <- ols(Y~X1 + X2, data=dataj)
  fit3  <- ols(Y~X1 + X2 + X3, data=dataj)
  fit4  <- ols(Y~X1 + X2 + X3 + X4, data=dataj)
  fit5  <- ols(Y~X1 + X2 + X3 + X4 + X5, data=dataj)
  fit6  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6, data=dataj)
  fit7  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7, data=dataj)
  fit8  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=dataj)
  fit9  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 , data=dataj)
  fit10  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data=dataj)
  fit11  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11, data=dataj)
  fit12  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12, data=dataj)
  fit13  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13, data=dataj)
  fit14  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14, data=dataj)
  fit15  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15, data=dataj)
  fit16  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16, data=dataj)
  fit17  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17, data=dataj)
  fit18  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X8, data=dataj)
  fit19  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 , data=dataj)
  fit20  <- ols(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20, data=dataj)
  
  # coefs in matrix
  resultsmat[i,1:15]  <- c(coef(fit5)[2:6],coef(fit10)[2:11])
  # test MSE
  resultsmat[i,16:35]  <-
    c(MSE(y=Ymj, fit=fit1, newdata=datamj),
      MSE(y=Ymj, fit=fit2, newdata=datamj),
      MSE(y=Ymj, fit=fit3, newdata=datamj),
      MSE(y=Ymj, fit=fit4, newdata=datamj),
      MSE(y=Ymj, fit=fit5, newdata=datamj),
      MSE(y=Ymj, fit=fit6, newdata=datamj),
      MSE(y=Ymj, fit=fit7, newdata=datamj),
      MSE(y=Ymj, fit=fit8, newdata=datamj),
      MSE(y=Ymj, fit=fit9, newdata=datamj),
      MSE(y=Ymj, fit=fit10, newdata=datamj),
      MSE(y=Ymj, fit=fit11, newdata=datamj),
      MSE(y=Ymj, fit=fit12, newdata=datamj),
      MSE(y=Ymj, fit=fit13, newdata=datamj),
      MSE(y=Ymj, fit=fit14, newdata=datamj),
      MSE(y=Ymj, fit=fit15, newdata=datamj),
      MSE(y=Ymj, fit=fit16, newdata=datamj),
      MSE(y=Ymj, fit=fit17, newdata=datamj),
      MSE(y=Ymj, fit=fit18, newdata=datamj),
      MSE(y=Ymj, fit=fit19, newdata=datamj),
      MSE(y=Ymj, fit=fit20, newdata=datamj))
  
  # external validation
  resultsmat[i,36:55]  <-
    c(MSE(y=Y2mj, fit=fit1, newdata=datamj),
      MSE(y=Y2mj, fit=fit2, newdata=datamj),
      MSE(y=Y2mj, fit=fit3, newdata=datamj),
      MSE(y=Y2mj, fit=fit4, newdata=datamj),
      MSE(y=Y2mj, fit=fit5, newdata=datamj),
      MSE(y=Y2mj, fit=fit6, newdata=datamj),
      MSE(y=Y2mj, fit=fit7, newdata=datamj),
      MSE(y=Y2mj, fit=fit8, newdata=datamj),
      MSE(y=Y2mj, fit=fit9, newdata=datamj),
      MSE(y=Y2mj, fit=fit10, newdata=datamj),
      MSE(y=Y2mj, fit=fit11, newdata=datamj),
      MSE(y=Y2mj, fit=fit12, newdata=datamj),
      MSE(y=Y2mj, fit=fit13, newdata=datamj),
      MSE(y=Y2mj, fit=fit14, newdata=datamj),
      MSE(y=Y2mj, fit=fit15, newdata=datamj),
      MSE(y=Y2mj, fit=fit16, newdata=datamj),
      MSE(y=Y2mj, fit=fit17, newdata=datamj),
      MSE(y=Y2mj, fit=fit18, newdata=datamj),
      MSE(y=Y2mj, fit=fit19, newdata=datamj),
      MSE(y=Y2mj, fit=fit20, newdata=datamj))
  
  # apparent validation
  resultsmat[i,56:75]  <-
    c(MSE(y=Yj, fit=fit1, newdata=dataj),
      MSE(y=Yj, fit=fit2, newdata=dataj),
      MSE(y=Yj, fit=fit3, newdata=dataj),
      MSE(y=Yj, fit=fit4, newdata=dataj),
      MSE(y=Yj, fit=fit5, newdata=dataj),
      MSE(y=Yj, fit=fit6, newdata=dataj),
      MSE(y=Yj, fit=fit7, newdata=dataj),
      MSE(y=Yj, fit=fit8, newdata=dataj),
      MSE(y=Yj, fit=fit9, newdata=dataj),
      MSE(y=Yj, fit=fit10, newdata=dataj),
      MSE(y=Yj, fit=fit11, newdata=dataj),
      MSE(y=Yj, fit=fit12, newdata=dataj),
      MSE(y=Yj, fit=fit13, newdata=dataj),
      MSE(y=Yj, fit=fit14, newdata=dataj),
      MSE(y=Yj, fit=fit15, newdata=dataj),
      MSE(y=Yj, fit=fit16, newdata=dataj),
      MSE(y=Yj, fit=fit17, newdata=dataj),
      MSE(y=Yj, fit=fit18, newdata=dataj),
      MSE(y=Yj, fit=fit19, newdata=dataj),
      MSE(y=Yj, fit=fit20, newdata=dataj))
  
  cat(".. end")
} # end simulations sim

## With n=50
resultsmatn50 <- resultsmat
resultsn50 <- apply(resultsmatn50,2,mean)
perfmat <- matrix(resultsn50[16:75],nrow=20,ncol=3)
plot(spline(x=1:20,y=perfmat[,3]),lty=1,lwd=1, xlim=c(0.5,20.5),ylim=c(8,18), type="l",
     xlab="Number of predictors", ylab="Mean squared error",cex.lab=1.2) # Apparent
lines(spline(x=1:20,y=perfmat[,1]),lty=2,lwd=2)                   # Internal
lines(smooth.spline(x=1:20,y=perfmat[,2]),lty=3,lwd=3)                   # External
text(x=c(7,7,7),y=c(11.7,13.8,15.4),label=Cs(Apparent, Internal, External))
