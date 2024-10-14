knapsack_dynamic<- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W)&&(W>0))
  stopifnot(all(colnames(x)==c("w","v")))
  stopifnot(all(sapply(x,function(x) x>0))&&all(sapply(x,function(x) is.numeric(x))))
  n<-nrow(x)
  m<-matrix(0, nrow=n+1, ncol=W+1)
  for(i in 1:n) {
    for(j in 1:W) {
      if(x$w[i]>j) {
        m[i+1,j+1]<-m[i,j+1]
      } else{
        m[i+1,j+1]<-max(m[i,j+1],m[i,j+1-x$w[i]]+x$v[i])
      }
    }
  }
  value<-m[n+1,W+1]
  elements<-integer(0)
  j<-W
  for(i in n:1) {
    if(m[i+1,j+1]!=m[i,j+1]){
      elements<-c(elements,i)
      j<-j-x$w[i]
    }
  }
  return(list(value = value, elements = sort(elements)))
}

# ptm <- proc.time()
# a<-knapsack_dynamic(x = knapsack_objects[1:500,], W = 2000)
# proc.time() - ptm
# # it took 1.808 milliseconds