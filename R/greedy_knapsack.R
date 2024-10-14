greedy_knapsack<- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W)&&(W>0))
  stopifnot(all(colnames(x)==c("w","v")))
  stopifnot(all(sapply(x,function(x) x>0))&&all(sapply(x,function(x) is.numeric(x))))
  
  x$ratio<-x$v/x$w
  y<-x[order(-x$ratio), ]
  
  value <- 0
  totalWeight <- 0
  elements<-integer(0)
  
  for(i in 1:nrow(y)) {
    if(totalWeight+y$w[i]<=W) {
      value<- value+y$v[i]
      totalWeight<- totalWeight+y$w[i]
      elements<- c(elements,which((x$w==y$w[i])&(x$v==y$v[i])))
    }
  }
  
  return(list(value=value,elements=elements))
}
# ptm <- proc.time()
# a<-greedy_knapsack(x = knapsack_objects[1:1000000,], W = 2000)
# proc.time() - ptm
# # it took 6.923 milliseconds