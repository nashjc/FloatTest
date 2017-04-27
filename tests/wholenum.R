is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
is.wholenumber(1) # is TRUE
x0 <- seq(1, 5, by = 0.5) 
offset <- 2
idx <- 1
repeat {
  x <- x0 + offset  
  cat(idx," ",sum(as.integer(is.wholenumber(x))),"\n")
  if (all(is.wholenumber(x) == TRUE)) break
  idx <- idx + 1
  offset <- 2 * offset
}
cat("\n")
cat("Stopped with idx =",idx,"  offset=",offset,"\n")


