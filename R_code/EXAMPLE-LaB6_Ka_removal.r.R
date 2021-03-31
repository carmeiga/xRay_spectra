

##################################################################
## LaB6: Ka2 removal using the PCLM
##################################################################
##################################################################

library("R.utils")
library("spam")

source('read_raw.r')
source('read_uxd.r')
source('write_raw.r')
source('sparse_bases.r')
source('kastrip.r')
source('doublet.r')


##################################################################
## READ DATA
##################################################################

# read uxd file 
fname <- "sal"
#D <- read_xrd(fname)$D
#x <- D$phi
#y <- D$counts

x=theta

# plot result
plot(x,y, type='l', main=fname)

# calculate doublet distance
delta <- doublet(x, anode="copper")$delta


##################################################################
## KA REMOVAL
##################################################################

## series of lambda
nn <- 40
lambda = seq(1,8, length.out=nn)
lambda <- 10^lambda

## estimate
Mk <- kastrip(x, y, nseg=5000, delta, lambda, tau=0.5)


## Plot results
###########################

## data, Ka1, Ka2
plot(x, Mk$y, type="l", main="Observed data and estimated components",
     xlab="2theta", ylab="Counts")
lines(x, Mk$yhat, type="l", col="red")
lines(x, Mk$yhat2, type="l", col="blue")


## AIC
plot(log(lambda), Mk$aic, type="l", main="AIC curve and optimal lambda")
abline(v=log(lambda[which.min(Mk$aic)]), col="red")

## L-curve
par(mfrow=c(2,1))
plot(Mk$psi,Mk$phi, asp=1, main="L-curve")

## step size
dlts <- sqrt(diff(Mk$psi)^2 + diff(Mk$phi)^2)
mulamb <- rep(0,length(lambda)-1)
for(i in 1:(length(lambda)-1))
{
  mulamb[i] <- (mean(c(lambda[i], lambda[i+1])))
}
plot(log(mulamb), dlts, pch=19, "Optimal lambda according the L-curve")
lines(log(mulamb), dlts)
wm <- which.min(sqrt(diff(Mk$psi)^2 + diff(Mk$phi)^2))
abline(v=log(mulamb[wm]), col="red", lwd=2)


###############################################################
