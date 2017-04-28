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
     S <- rep(0,maxdepth) # set up stacks
     T <- rep(0,maxdepth) # set up stacks
     P <- rep(0,maxdepth) # set up stacks
     k<-maxdepth
     showstack <- function(k, P, T, S){
       #  cat("showstack k=",k,"\n")
       #  print(P)
       cat(" P  \t T    \t      S    for k=",k,"\n")
       for (i in 1:k){
         #   cat("i =", i,"\n")
         cat(P[i],"\t ",T[i],"   \t ",S[i],"\n")
       }
       cat("============================================\n")
       return(1)
     }     

     showstack(k, P, T, S)
     
     k <- 0 # stack depth

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
    showstack(k, P, T, S)
    nn <- P[k]
    mm <- P[k-1]
    P[k-1] <- nn + mm   
    T1 <- nn * T[k-1]/mm - T[k]
    T2 <- mm*T1*T1/(nn*(mm + nn))
    cat("nn, mm, T1, T2:", nn, mm, T1, T2,"\n")
    S[k-1] <- S[k-1] + S[k] + T2
    T[k-1] <- T[k-1] + T[k]
    cat("T, S for k-1: ",T[k-1],S[k-1],"\n")
    # Note could zero the level k entries
    P[k] <- T[k] <- S[k] <- 0 # not absolutely necessary
    k <- k - 1
    showstack(k, P, T, S)
    tmp <- readline("above after first 2 points\n")
    repeat {
       x <- getdatapoint(datasource)
       cat("datapointer = ",datapointer,"\n")
     #  print(P)
       if (is.na(x)) { break }
       k <- k+1
       cat("k=",k," after new data\n")
       if (k <= maxdepth) {
         P[k] <- 1
         T[k] <- x 
         S[k] <- 0 # in case left non-zero
         # combine if equal
         while ( (k>1) && (P[k-1]==P[k]) ) {
           nn <- P[k]
           mm <- P[k-1]
           P[k-1] <- nn + mm   
           T1 <- nn * T[k-1]/mm - T[k]
           T2 <- mm*T1*T1/(nn*(mm + nn))
           cat("nn, mm, T1, T2:", nn, mm, T1, T2,"\n")
           S[k-1] <- S[k-1] + S[k] + T2
           T[k-1] <- T[k-1] + T[k]
           cat("T, S for k-1: ",T[k-1],S[k-1],"\n")
           # Note could zero the level k entries
           P[k] <- T[k] <- S[k] <- 0 # not absolutely necessary
           k <- k - 1
           showstack(k, P, T, S)
          tmp <- readline("continue?")
         }
       } else { stop("Stack overflow")}
    }
    # Force collapse of stack now data finished
    while (k > 1) {
        nn <- P[k]
        mm <- P[k-1]
        P[k-1] <- nn + mm   
        T1 <- nn * T[k-1]/mm - T[k]
        T2 <- mm*T1*T1/(nn*(mm + nn))
        cat("nn, mm, T1, T2:", nn, mm, T1, T2,"\n")
        S[k-1] <- S[k-1] + S[k] + T2
        T[k-1] <- T[k-1] + T[k]
        cat("T, S for k-1: ",T[k-1],S[k-1],"\n")
        # Note could zero the level k entries
        P[k] <- T[k] <- S[k] <- 0 # not absolutely necessary
        k <- k - 1
        showstack(k, P, T, S)
        tmp <- readline("cont. collapse")
    }
    mean <- T[1]/P[1]
    variance <- S[1]/(P[1] - 1)
    res <- list(mean=mean, variance=variance)
    res
}    

  ds <- 1000:1010
  sol<-pairvar(ds)
  print(sol)
  mean(ds)
  var(ds)
  cat("done\n")
  