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
  
  cat("geometric progressions\n")
  a <- 1
  r <- 1.1
  n <- 100
  sq <- 0:(n-1)
  gp <- a*(r^sq)
  gp
  sumgp <- a*(1 - r^n)/(1-r)
  sumgp
  sum(gp)
  gp2 <- a*a*(r^(2*sq))
  gp2
  sum(gp2)
  sumgp2 <- a*a*(1 - r^(2*n))/(1 - r*r)
  sumgp2
  var(gp) 
mean(gp)
sumgp/n
sum(gp^2)  
# a*a*(1 - (r^2)^n)/(1-r^2)

vgp <- 2*(r^n-2*r)*a*a*n*(1-r^n)/((n-1)*(1-r*r)*(1+r))
vgp
var(gp)

# (r^n + 1)/(r+1) - (r^n-1)/(r-1)

vgp2 <- a*a*(1-r^n)*((1+r^n)/(1+r)-(1-r^n)/(n*(1-r)))/((n-1)*(1-r))
vgp2
var(gp)
# vgp2a <- (a*a*(1-r)/())
