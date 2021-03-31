

doublet <- function(x, anode)
{
  # characteristic wavelength of anode material
  Co <- c(1.78897, 1.79285)
  Cu <- c(1.54060, 1.54443)
  
  wl <- switch(anode, "cobalt"=Co, "copper"=Cu)
  
  wl1 <- wl[1]
  wl2 <- wl[2]
  wr <- wl2 / wl1
  d2r <- pi / 180
  u <- sin(d2r * x / 2) / wl1
  delta <- 2 * asin(wr * sin(d2r * x / 2)) / d2r - x
  
  list(delta=delta)
}
