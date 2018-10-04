# Plot 4 situations: no heterogeneity to much heterogeneity
# Fig 5.3 first
par(mfrow = c(2,2), pty='s', mar=c(3,2,1,1))
# Make data set with 100 centers, each 20 patients, 10% mortality, variability sd 0 to 0.03
for (sdtau in c(0,.01,.02,.03)) { 
  truemort  <- rnorm(n=100,mean=0.1,sd=sdtau)
  mortmat <- as.matrix(cbind(1:100, sapply(truemort,FUN=function(x)rbinom(n=1,20,x))/20,truemort))
  
  plot(x=0, y=0, pch='', xlim=c(-.2,1.2), ylim=c(-.03,.35),axes=F, xlab="", ylab='mortality')
  axis(side=2,at=c(0,.1,.2,.3),labels=c("0%","10%","20%","30%"))
  axis(side=1,at=c(0,1),labels=c("Observed","True mortality"))
  for (i in (1:100)) {
    lines(x=c(0,1), y=mortmat[i,c(2,3)])
    points(x=c(0+runif(1,min=-.07,max=.07),1), y=mortmat[i,c(2,3)], pch="+") } }

## Same, for n=200 per center Fig 5.4
# Plot 4 situations: no heterogeneity to much heterogeneity
par(mfrow = c(2,2), pty='s', mar=c(3,2,1,1))
# Make data set with 100 centers, each 200 patients, 10% mortality, variability sd 0 to 0.03
for (sdtau in c(0,.01,.02,.03)) { 
  truemort  <- rnorm(n=100,mean=0.1,sd=sdtau)
  mortmat <- as.matrix(cbind(1:100, sapply(truemort,FUN=function(x)rbinom(n=1,200,x))/200,truemort))
  plot(x=0, y=0, pch='', xlim=c(-.2,1.2), ylim=c(-.03,.35),axes=F, xlab="", ylab='mortality')
  axis(side=2,at=c(0,.1,.2,.3),labels=c("0%","10%","20%","30%"))
  axis(side=1,at=c(0,1),labels=c("Observed","True mortality"))
  for (i in (1:100)) {
    lines(x=c(0,1), y=mortmat[i,c(2,3)])
    points(x=c(0+runif(1,min=-.07,max=.07),1), y=mortmat[i,c(2,3)], pch="+") } }
