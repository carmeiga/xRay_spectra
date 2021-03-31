

#### The Whitaker smoother function
###########################################

# y: vector with observed counts
# d: order of the differences
# lambda: tuning parameter values

wh_smoother = function(y, d = 2, lambda = 10)
  {
  w <- 0 * y + 1
  m <- length(y)
  E <- diag.spam(m)
  D <- diff(E, diff = d)
  W <- diag.spam(w)
  z <- solve(W + lambda * t(D) %*% D, w * y)
  return(z)
}
