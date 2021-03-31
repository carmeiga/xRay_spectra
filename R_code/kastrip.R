
#################################
## Ka removal, user level function
#################################


kastrip <- function(x, y, nseg=1, delta, lambda, tau=0.5)
{
  if(nseg==1)
  {nseg <- signif(length(y)*0.50,1)}
  
  nl <- length(lambda)  
  ml <- length(y)
  ss <- 1:nl
  
  # prepare matrices for storage
  aic <- rep(0,nl)
  Yhat <- matrix(0,ml,nl)
  Yhat2 <- matrix(0,ml,nl)
  mu <- matrix(0,ml,nl)
  SR <- matrix(0,ml,nl)
  phi <- rep(0, nl)
  psi <- rep(0, nl)
  
  # Prepare model matrices
  xr <- max(x)
  xl <- min(x) -1
  xd <- x-delta
  B1 <- bbase.spam(x, xl, xr, ndx = nseg)
  B2 <- bbase.spam(xd, xl=xl, xr=xr, ndx = nseg)
  n <- ncol(B1)
  B <- rbind(B1, B2)
  m <- length(x)
  Cb <- diag.spam(m)
  C <- cbind(diag.spam(m), tau * Cb)
  
  # Prepare penalty
  E <- diag.spam(n)
  D <- diff(E, diff = 2)
  
  # progress bar
  cat("Estimation...", '\n')
  pb <- pb <- txtProgressBar(min = 0, max = nl, style=3)
  
  # Initialize
  bst = solve(t(B1) %*% B1 + 1e-4 * E, t(B1) %*% log((y + 1) / 2))
  
  # estimate for all lambda
  for (i in 1:nl)
    {
    M <- kastrip.int(y, B, C, D, bst, lambda[i])
    aic[i] <- M$aic
    Yhat[,i] <- M$yhat
    Yhat2[,i] <- M$yhat2
    SR[,i] <- M$sr
    mu[,i] <- M$mu
    phi[i] <- M$phi
    psi[i] <- M$psi
    #print(ss[i])
    setTxtProgressBar(pb, i)
    }
  
  ## find optimal model and report
  minm <- which.min(aic)
  op_aic <- aic[minm]
  yhat <- Yhat[,minm]
  yhat2 <- Yhat2[,minm]
  mu <- mu[,minm]
  oplamb <- lambda[minm]
  sr <- SR[,minm]
  
  list(yhat=yhat, aic=aic, op_aic=op_aic, oplamb=oplamb, sr=sr, 
       yhat2=yhat2, mu=mu, y=y, phi=phi, psi=psi, x=x)
} 


#################################
## Ka removal, for internal use
#################################


kastrip.int <- function(y, B, C, D, bst, lambda)
{
  # penalty
  P = lambda * t(D) %*% D
  beta <- bst
  
  # Fit iteratively
  for (it in 1:30) {
    eta = B %*% beta
    gam = exp(eta)
    mu = C %*% gam
    M = diag.spam(1 / c(mu))
    X = (M %*% C %*% diag.spam(c(gam))) %*% B
    W = diag.spam(c(mu))
    G = t(X) %*% W %*% X
    bnew = solve(G + P, t(X) %*% (y - mu) + G %*% beta)
    db = max(abs(bnew - beta))
    beta = bnew
    #cat(it, db, '\n')
    if (db < 1e-4) break
  }
  
  ## AIC
  dev = 2 *  sum(y * log((y + (y == 0)) / mu))
  H = solve(G + P, G)
  ed = sum(diag(H))
  aic = dev + 2 * ed
  sr = (y - mu) / sqrt(mu)
  m = length(y)
  sdr = sqrt(sum(sr ^2) / (m - ed))
  #cat(lambda, aic, ed, sdr, '\n')
  
  ## L-curve
  phi <- log(sum((D%*%beta)^2))
  psi <- log(sum(sr^2))
  
  ## estimated signals
  yhat <- gam[1:m]
  yhat2 <- gam[(m+1):length(gam)]*0.5
  
  ## return values
  list(yhat = yhat, sr=sr, aic=aic, yhat2 = yhat2, beta=beta, 
       mu=mu, phi=phi, psi=psi)
}


###################################

