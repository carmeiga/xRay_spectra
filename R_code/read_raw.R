

read_raw <- function(fname)
  {
  
  ##-- read header
  cn <- file(fname, "rb")
  raw <- readChar(cn, 8)
  fstat <- readBin(cn, integer(), size=4)
  rng <- readBin(cn, integer(), size=4)
  date <- readChar(cn, 10)
  time <- readChar(cn, 10)
  usr <- readChar(cn, 72)
  site <- readChar(cn, 218)
  sample <- readChar(cn, 60)
  cmnt <- readChar(cn, 160)
  close(cn)

  ##-- Read XRD profile properties
  cn <- file(fname, "rb") 
  block1 <- readBin(cn, integer(), n=712, size=1)
  headr <- readBin(cn, integer(), size=4)
  nrec <- readBin(cn, integer(), size=4)
  gonio <- readBin(cn, numeric(), size=8, n=2)
  strt <- gonio[2]
  nix <- readBin(cn, integer(), size=1, n=176-24)
  step <- readBin(cn, numeric(), size=8)
  close(cn)

  ##-- Read file headers
  cn <- file(fname, "rb") 
  block1 <- readBin(cn, integer(), n=386, size=1)
  cmnt <- readChar(cn, 160)
  block2 <- readBin(cn, integer(), n=422, size=1)
  # check length of supp. header
  size_sup <- readBin(cn, integer(), size=4)
  block3 <- readBin(cn, integer(), n=44, size=1)
  block4 <- readBin(cn, integer(), n=size_sup, size=1)
  close(cn)

  ##-- Read profile
  cn <- file(fname, "rb")
  blocks <- readBin(cn, integer(), n=712 + 304 + size_sup, size=1)
  y <- readBin(cn, numeric(), n=nrec, size=4)
  x <- (strt + (1:nrec) * step) - step
  close(cn)
  
  list(x=x, y=y)  
  }

################################################
