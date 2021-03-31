

## Smoothing counts using penalized likelihood
##############################################

# y: vector with observed counts
# d: order of the differences
# lambda: tuning parameter values
# range: analyse part of the data, if 0 complete scan is analyzed
# SE: calculate standard erros, default is False

pl_smoother <- function(y, d=2, lambda, range=0, AIC=T, SE=F)
  {
  yorg <- y
  if(range[1]>0)
    {y <- y[range]}

  nl <- length(lambda)  
  m <- length(y)
  ss <- 1:nl
  
  aic <- rep(0,nl)
  Mu <- matrix(0,m,nl)
  Se <- matrix(0,m,nl)
  R <- matrix(0,m,nl)
  
  # progress bar
  cat("Estimation...", '\n')
  pb <- pb <- txtProgressBar(min = 0, max = nl, style=3)
  
  for (i in 1:nl)
    {
    M <- pl_smoother.int(y, d=d, lambda[i], AIC=AIC, SE=SE)
    aic[i] <- M$aic
    Mu[,i] <- M$mu
    Se[,i] <- M$se
    R[,i] <- M$r
    #print(ss[i])
    setTxtProgressBar(pb, i)
    }
  
  minm <- which.min(aic)
  mu <- Mu[,minm]
  se <- Se[,minm]
  oplamb <- lambda[minm]
  op_aic <- aic[minm]
  r <- R[,minm]
  
  if(range[1]>0)
    {M <- sm_counts(yorg, 2, lambda[minm])
     mu <- M$mu
     op_aic <- M$aic
     r <- M$r
     }
  
  list(mu=mu, aic=aic, op_aic=op_aic, oplamb=oplamb, r=r, se=se)
  } 



## Internal function
###########################

pl_smoother.int <- function(y, d, lambda, AIC, SE){
  
  m <- length(y)
  z <- log(y + 1)
  E <- diag.spam(m)
  D <- diff(E, differences=d)
  P <- lambda * t(D) %*% D
  mu <- exp(z)
  
  for (it in 1:10)
  {
    W <- diag.spam(mu)
    z = solve(W + P, y - mu + mu * z)
    munew = exp(z)
    dmu = max(abs(munew - mu))
    mu = munew    
    if(dmu < 0.0001){break}
  }
  
  ## AIC
  if(AIC==TRUE)
  {
  H <- solve(W + P)%*%W
  ed <- sum(diag(H))
  dev <- 2 * sum(y * log((y + 1e-10) / mu))
  aic <- dev + 2* ed*m/(m-ed)
  }else{aic=0}
  
  ## standardized residuals for counts
  r <- (y - mu) / sqrt(mu)

  se <- rep(0, m)
  if(SE==TRUE)
  {
    HH<-H%*%W%*%t(H)
    se <- sqrt(diag(HH))
  }
  
  ## report  
  list(mu=mu, aic=aic, r=r, se=se)
}



