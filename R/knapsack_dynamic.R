#' @name knapsack_dynamic
#' @title Solution to knapsack problem using dynamic programming
#' @description The function implements solution to knapsack problem using dynamic programming. [wiki](https://en.wikipedia.org/wiki/Knapsack).
#' @param x Data.frame Contains 2 columns "w" and "v" containing the weight of sacks and value of sacks respectively.
#' @param W Numeric. Specifies the capacity of knapsack.
#' @return Named List
#' @export
#' 
#' @examples
#' #Prepare Data:
#' suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' knapsack_objects<-data.frame(w=sample(1:4000,size=1000,replace=TRUE),v=runif(n=1000,0,10000))
#' 
#' ptm <- proc.time()
#' a<-knapsack_dynamic(x = knapsack_objects[1:500,], W = 2000)
#' print(proc.time() - ptm) # 1.808 milliseconds
#' print(a)
#' 
#' @seealso
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}

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