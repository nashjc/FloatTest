# pairwise variance calculation script
# This program is didactic rather than high-performance
# arrays: P contains counts
#         T contains sum
#         S contains squares

# ?? seems stack is not being passed around

  pairvar <- function(datasource){
     maxdepth <- 8 # maximum depth of stack -- handles more than 2^maxdepth observations
     datapointer <<- 0 # global. Points to next data pointer. Set negative at end.
     # Note: R does not have long integers, so we could have trouble with very large datasets.
     # Using double can get to 4e15 points, however.
     # actualy sum(i=0:(maxdepth-1)){2^i}
     P <- T <- S <- rep(0,maxdepth) # set up stacks
     k <- 0 # stack depth

     showstack <- function(k){
        cat("showstack k=",k,"\n")
        print(P)
        cat("   P             T            S\n")
        for (i in 1:k){
            cat("i =", i,"\n")
            cat(P[i]," ",T[i]," ",S[i],"\n")
        }
        cat("============================================\n")
        return(1)
     }     
     
     getdatapoint <- function(datasource){
     # get next data value -- do we need a global??
       datapointer <<- datapointer + 1
       if (datapointer > length(datasource)) {
          datapointer <<- -1 # do we want to do this?
          x <- NA
       } else {
          x <- datasource[datapointer]
       } 
       cat("getdatapoint x=",x,"\n")
       x
     }

     combform <- function(k){
       cat("in combform, k=",k,"\n")
       print(P)
       nn <- P[k]
       mm <- P[k-1]
       P[k-1] <- nn + mm   
       T1 <- nn * T[k-1]/mm - T[k]
       T2 <- mm*T1*T1/(nn*(mm + nn))
       S[k-1] <- S[k-1] + S[k] + T2
       T[k-1] <- T[k-1] + T[k]
       # Note could zero the level k entries
       P[k] <- T[k] <- S[k] <- 0 # not absolutely necessary
       k <- k - 1
       k
     }
     
     stackcombine <- function(k) { # assume k is OK from above
       cat("in stackcombine, k=",k,"\n")
       print(P)
       while ( (k > 1) && (P[k] == P[k-1])) {
          cat("k=",k," combform called\n")
          k <- combform(k)
        }
        k
     }
    
    cat("start of work with datasource\n") 
    cat("datapointer = ",datapointer,"\n")
    x  <- getdatapoint(datasource) 
    if (is.na(x)) {
       stop("No data")
    } else { 
       k <- 1 
       P[1] <- 1
       T[1] <- x
    }
    x  <- getdatapoint(datasource) 
    if (is.na(x)) {
      stop("Only 1 data point")
    } else { 
      k <- 2 
      P[2] <- 1
      T[2] <- x
    }
    cat("after first 2 points\n")
    print(P)
    showstack(k)
    k <- stackcombine(k)
    cat("datapointer = ",datapointer,"  stackpointer=",k,"\n")
    showstack(k)
    repeat {
       x <- getdatapoint(datasource)
       cat("datapointer = ",datapointer,"\n")
       print(P)
       if (is.na(x)) { break }
       k <- k+1
       cat("k=",k," after new data\n")
       
       P[k] <- 1
       T[k] <- x 
       S[k] <- 0 # in case left non-zero
       # combine if equal
       k <- stackcombine(k)
       showstack(k)
       tmp <- readline("continue?")
    }
    # Force collapse of stack now data finished
    while (k > 1) {
       stackcombine(k)
    }
    mean <- T[1]/P[1]
    variance <- S[1]/(P[1] - 1)
    res <- list(mean=mean, variance=variance)
    res
}    

  ds <- 1000:1010
  pairvar(ds)
  cat("done\n")
  