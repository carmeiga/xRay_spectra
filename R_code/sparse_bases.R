
# Functions for TPF and B-spline bases

tpower <- function(x, t, p)
  # Function for truncated p-th power function
  (x - t) ^ p * (x > t)

bbase <- function(x, xl = min(x), xr = max(x), ndx = 10, deg = 3){
  # Function for B-spline basis
  dx <- (xr - xl) / ndx
  knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
  P <- outer(x, knots, tpower, deg)
  n <- dim(P)[2]
  D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg)
  B <- (-1) ^ (deg + 1) * P %*% t(D)
  B
}


bbase.spam = function(x, xl = min(x), xr = max(x), ndx = 10, deg = 3) {
  # Sparse B-spline matrix in spam format
  require(spam)
  
  m = length(x)
  
  # Reduce x to first interval
  dx = (xr - xl) / ndx
  ix <- floor((x-xl) / dx)
  xr = (x-xl) - ix * dx
  
  # Full basis for reduced x
  Br = bbase(xr, xl = 0, xr = 0 + dx, ndx = 1, deg = deg)
  
  # Compute proper rows, columns
  nr = ncol(Br)
  rw = rep(1:m, each = nr)
  cl = rep(1:nr, m) + rep(ix, each = nr)
  
  # Make the sparse matrix
  b = as.vector(t(Br))
  Bs = spam(list(i = rw, j = cl, b), nrow=m, ncol=(ndx+deg))
  
  #list(Bs=Bs, ix=ix,cl=cl,b=b, xr=xr, dx=dx)
  return(Bs)
  }
