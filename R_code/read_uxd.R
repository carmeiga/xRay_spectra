
############################################
## READ XRD
############################################

read_xrd <- function(fname)
{TF <- grepl(".uxd", tolower(fname))
 if(TF==TRUE)
 {
 C <-read_uxd(fname)
 D <- C$D; Cont <- C$Cont
 }
 else{L <-read_raw(fname)
 C <- cbind(L$x,L$y)
 }
 names(D) = c('phi', 'counts')
 list(D=D, Cont=Cont)
}
 
############################################
## READ UXD
############################################
 
read_uxd <- function(fname)
  {
  ## READING HEADER AND DATA
  ############################
  DD <- readLines(fname)
  Ds <- gsub(" ", "", DD)
  Gd <-grep("^[1,2,3,4,5,6,7,8,9,0]", Ds)
  strt <- min(Gd)
  H <- readLines(fname ,n=strt-1)
  D <- read.table(fname, skip=strt-1, header=F)
  
  ## PREPARATIONS FOR WRITING UXD
  ###############################
  # search white spaces
  Ds <- DD[strt]
  L <- gregexpr(" ", Ds[1])[[1]]
  # determine locations
  l <- length(L)
  sl <- L[1:l]
  s <- 1:max(sl)
  TF <- s %in% sl
  # create columns with N whitespaces
  TFF <- ifelse(TF==TRUE, " ", "N")
  TFFc <- paste(TFF, collapse="")
  cnt <- sum(ifelse(TF==TRUE, 0,1))
  N <- paste(rep("N", cnt), collapse="")
  clms <- strsplit(TFFc, N)
  
  # create dataframe
  C1 <- rep(clms[[1]][1], nrow(D))
  C2 <- rep(clms[[1]][2], nrow(D))
  numb <- 1:nrow(D)
  Dw <- data.frame(C1, D[,1], C2, numb)
  
  Cont <- list(H=H, Dw=Dw)
  
  ## RETURN
  list(D=D, Cont=Cont)
  }
 

############################################
## WRITE UXD
############################################

write_uxd <- function(fname, Cont, cdat)
  {
  # create data matrix
  D <- Cont$Dw
  D[,4] <- round(cdat,0)
  
  # reproduce header
  write.table(Cont$H, paste(fname, ".uxd", sep=""), row.names=FALSE, 
             col.names=F, quote=FALSE)
  write.table(format(D, digits=10), paste(fname, ".uxd", sep=""), row.names=FALSE, 
             col.names=F, append=T, sep="", quote=FALSE)
}

