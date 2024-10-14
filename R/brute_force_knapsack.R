brute_force_knapsack<- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W)&&(W>0))
  stopifnot(all(colnames(x)==c("w","v")))
  stopifnot(all(sapply(x,function(x) x>0))&&all(sapply(x,function(x) is.numeric(x))))
  
  n<-nrow(x)
  value <- 0
  elements <- NULL
  
  for (i in 0:(2^n - 1)) {
    combination <- as.logical(intToBits(i)[1:n])
    totalWeight <- sum(x$w[combination])
    totalValue <- sum(x$v[combination])
    
    if (totalWeight <= W && totalValue > value) {
      value <- totalValue
      elements <- which(combination)
    }
  }
  return(list(value=value, elements=elements))
}

# ptm <- proc.time()
# a<-brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000)
# proc.time() - ptm
# # it took 0.404 milliseconds