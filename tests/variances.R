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
      readline("cont.")
  }
  cat("failed for shift =",shift,"\n")
  