require(Rmpfr)
# sqrt test on integers
# This test shows that some numbers that are not squares of integers still
#   give  x - (sqrt(x)*sqrt(x))  as zero.
iseq <- 0L:49L
print(str(iseq)) # verify that they are integers
sisq <- sqrt(iseq)
dvn <- (iseq - sisq*sisq)
cat("Deviations:\n")
print(dvn) ## not readable
dispdvn <- data.frame(row.names=iseq, dvn, Dec=format(dvn, scientific=FALSE, digits=2), Hex=as.character(formatHex(dvn)))
print(dispdvn)
cat("Values giving Non-zero deviation:\n")
# Note that we subtract 1 so we print the VALUE in the sequence where we match or not
print(which(dvn != 0)-1)
cat("Values giving zero deviation:\n")
izdvn <- which(dvn == 0) # These are indices the zero deviation results
print(izdvn-1)
iexsq <- which((sisq %% 1L) == 0) 
inzd <- izdvn[which(iexsq != izdvn)]
cat("The following members of the set have zero deviation but are NOT squared integers:\n")
print(inzd-1)
cat("There are ",length(iseq)," integers in the test set\n")
cat("Of these ",length(izdvn)," satisfy x == (sqrt(x)*sqrt(x))\n")
cat("But ",length(inzd)," are NOT squares of integers\n")
cat("\n\n")
cat("Check with ^ operator\n")
sesq <- iseq^(0.5)
ssesq <- sesq^2
edvn <- iseq - ssesq
cat("E Deviations:\n")
print(edvn)
dispedvn <- data.frame(row.names=iseq, edvn, Dec=format(dvn, scientific=FALSE, digits=2), Hex=as.character(formatHex(dvn)))
print(dispedvn)
cat("Values giving Non-zero deviation:\n")
# Note that we subtract 1 so we print the VALUE in the sequence where we match or not
print(which(edvn != 0)-1)
cat("Values giving zero deviation:\n")
iezdvn <- which(edvn == 0) # These are indices the zero deviation results
print(iezdvn-1)
ieexsq <- which((sesq %% 1L) == 0) 
ienzd <- izdvn[which(ieexsq != iezdvn)]
cat("The following members of the set have zero deviation but are NOT squared integers:\n")
print(ienzd-1)
cat("There are ",length(iseq)," integers in the test set\n")
cat("Of these ",length(iezdvn)," satisfy x == (sqrt(x)*sqrt(x))\n")
cat("But ",length(ienzd)," are NOT squares of integers\n")

