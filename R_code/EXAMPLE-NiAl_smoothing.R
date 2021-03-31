

##################################################################
## NiAl: Smoothing using penalized likelihood smoother
##################################################################
##################################################################

library("R.utils")
library("spam")

source('read_raw.r')
source('read_uxd.r')
source('write_raw.r')
source('wh_smoother.r')
source('pl_smoother.r')


##################################################################
## READ DATA
##################################################################

# Read raw file 
fname <- "Ni-Alumina_22feb13V3.raw"
D <- read_raw(fname)
x <- D$x
y <- D$y

# Plot result
plot(x,y, type='l', main=fname)


###############################################################
## SMOOTHING
###############################################################

#-- Smoothing using Whitaker
Swh <- wh_smoother(y, lambda=50)
plot(x, y, type="l", lwd=2, main="Observed and smoothed data (wh-smoother)")
lines(x, Swh, col="red")


#-- Smoothing using penalized likelihood
nn <- 40
lambda = seq(1,8, length.out=nn)
lambda <- 10^lambda
Spl <- pl_smoother(y, 3, lambda=lambda)
Spl <- pl_smoother(y, 3, lambda=Spl$oplamb, SE=T)
plot(x, y, type="l", lwd=2, main="Observed and smoothed data (pl-smoother)")
lines(x, Spl$mu, col="red3", lwd=1)


##############################################
