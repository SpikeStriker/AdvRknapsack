#' @name greedy_knapsack
#' @title Solution to knapsack problem using Greedy Algorithm
#' @description The function implements solution to knapsack problem using Greedy Algorithm [wiki](https://en.wikipedia.org/wiki/Knapsack).
#' @param x Data.frame Contains 2 columns "w" and "v" containing the weight of sacks and value of sacks respectively.
#' @param W Numeric. Specifies the capacity of knapsack.
#' @param ... Optional logical parameter optimised with default=FALSE. If set to TRUE, an optimised version (developed using profvis analysis) of algorithm is used.
#' @return Named List
#' @export
#' 
#' @examples
#' #Prepare Data:
#' suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' knapsack_objects<-data.frame(w=sample(1:4000,size=1000000,replace=TRUE),v=runif(n=1000000,0,10000))
#' 
#' ptm <- proc.time()
#' a<-greedy_knapsack(x = knapsack_objects[1:1000000,], W = 2000)
#' print(proc.time() - ptm) # 6.923 milliseconds
#' print(a)
#' 
#' ptm <- proc.time()
#' b<-greedy_knapsack(x = knapsack_objects[1:1000000,], W = 2000, optimised=TRUE)
#' print(proc.time() - ptm) # 1.451 milliseconds
#' print(b)
#' 
#' @seealso
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}

greedy_knapsack<- function(x, W,...){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W)&&(W>0))
  stopifnot(all(colnames(x)==c("w","v")))
  stopifnot(all(sapply(x,function(x) x>0))&&all(sapply(x,function(x) is.numeric(x))))
  z<-list(...)
  useNonOptimised=TRUE
  
  if(!is.null(z$optimised)){
    if(z$optimised==TRUE){
      useNonOptimised=FALSE
      x$ratio<-x$v/x$w
      x<-x[order(-x$ratio), ]
      value <- 0
      totalWeight <- 0
      elements<-integer(0)
      for(i in 1:nrow(x)) {
        if(totalWeight+x$w[i]<=W) {
          value<- value+x$v[i]
          totalWeight<- totalWeight+x$w[i]
          elements<- c(elements,rownames(x[i,]))
        }
      }
      
    }
  }
  if(useNonOptimised){
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
  }
  return(list(value=value,elements=elements))
}