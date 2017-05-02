# pairwise variance calculation script
# This program is didactic rather than high-performance
# arrays: P contains counts
#         T contains sum
#         S contains squares

# ?? seems stack is not being passed around


  ds <- 1000:1010
  # sol<-pairvar(ds)
  # print(sol)
  print(mean(ds))
  print(var(ds))
  n <- length(ds)
  fmla <- n*(n+1)/12
  
  cat("formula for seq =",fmla,"\n")
  
  cat("done\n")
  
  n <- 1000
  fmla <- n*(n+1)/12
  
  
  cat("formula for seq =",fmla,"\n")
  ds0 <- 1:n
  shift <- 1
  ds <- ds0 + shift 
  
  while (var(ds) == fmla) {
      cat ("shift =", shift," is OK\n")
      shift <- 10 * shift
      ds <- ds0 + shift
      cat(fmla," ",var(ds),"\n")
  #    readline("cont.")
  }
  cat("failed for shift =",shift,"\n")
  
  mgp <- function(a, r, n) {
     mean <- a*(r^n - 1)/(n *(r - 1))       
  }
  vgp <- function(a, r, n){
    # Need to check for best method for calculation
#    val1 <- a*a*(1-r^n)*((1+r^n)/(1+r)-(1-r^n)/(n*(1-r)))/((n-1)*(1-r))
    val <- (a^2)*(r^(2*n)*((n-1)*r-(n+1))+r^n*2*(r+1)-(n+1)*r+(n-1))/(n*(n-1)*(r-1)*(r^2 -1))
#    cat("diff val1-val:", val1-val,"\n")
    val
  }
  
  trygp <- function(a, r, n, forward = TRUE){ ## split out mean and var ??
      sq <- 0:(n-1) 
      gp <- a * r^sq
      if (! forward) gp <- rev(gp)
      cat("Geometric progression: a=",a,"   r=", r, "   ",n,"elements\n")
      print(gp)
      mf <- mgp(a, r, n)
      mr <- mean(gp)
      cat("Mean: R", mr,"   formula=",mf,"  diff = ", (mr-mf),"\n" )
      vf <- vgp(a, r, n)
      vr <- var(gp)
      cat("Variance: R", vr,"   formula=",vf,"  diff = ", (vr-vf),"\n" )
      # return some flags, but for now just 1
      return (1)
  }
  
  
  cat("geometric progressions\n")
  a <- 1
  r <- 2
  for (nn in 10:50) {
    cat("Forward ")
    trygp(a, r, nn)
    cat("Backward ")
    trygp(a, r, nn)
    cat("\n\n")
    
  }
  # ?? need a better summary and diagnostics
  
