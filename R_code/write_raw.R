
write_raw <- function(oname, nname, y, comment)
    {
    
    cn <- file(oname, "rb")  
    block1 <- readBin(cn, integer(), n=386, size=1)
    cmnt <- readChar(cn, 160)
    block2 <- readBin(cn, integer(), n=422, size=1)
    size_sup <- readBin(cn, integer(), size=4)
    block3 <- readBin(cn, integer(), n=44, size=1)
    block4 <- readBin(cn, integer(), n=size_sup, size=1)
    close(cn)

    ##-- add comments
    cmb <- strsplit(comment,split=NULL)[[1]]
    cmnt <- rep("a",160)
    #cmnt[1:length(cmb)] <- cmb
    
    ##-- write to file
    nname <- paste(nname, ".raw", sep="")
    cn2 <- file(nname, "wb")
    writeBin(block1, cn2, size=1)
    writeChar(cmnt, cn2, rep(1,160), eos=NULL)
    writeBin(block2, cn2, size=1)
    writeBin(size_sup, cn2, size=4)
    writeBin(block3, cn2, size=1)
    writeBin(block4, cn2, size=1)
    # write smoothed y
    writeBin(y, cn2, size=4)
    close(cn2)
    }


