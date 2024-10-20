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
#' print(proc.time() - ptm) # 5.807 milliseconds
#' print(a)
#' 
#' ptm <- proc.time()
#' b<-greedy_knapsack(x = knapsack_objects[1:1000000,], W = 2000, optimised=TRUE)
#' print(proc.time() - ptm) # 0.227 milliseconds
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
      x<-x[x$w<=W,]
      x<-x[order(x$ratio,decreasing=TRUE), ]
      value <- x$v[1]
      totalWeight <- x$w[1]
      elements<-integer(0)
      i<-1
      while ((totalWeight+x$w[i+1]<=W)&(i<nrow(x))) {
        value<- value+x$v[i+1]
        totalWeight<- totalWeight+x$w[i+1]
        elements<- c(elements,as.numeric(rownames(x[i+1,])))
        i<-i+1
      }
    }
  }
  if(useNonOptimised){
    x$ratio<-x$v/x$w
    y<-x[which(x$w<=W),]
    y<-y[order(y$ratio,decreasing=TRUE), ]
    value <- y$v[1]
    totalWeight <- y$w[1]
    elements<-integer(0)
    i<-1
    while ((totalWeight+y$w[i+1]<=W)&(i<nrow(y))) {
      value<- value+y$v[i+1]
      totalWeight<- totalWeight+y$w[i+1]
      elements<- c(elements,which((x$w==y$w[i+1])&(x$v==y$v[i+1])))
      i<-i+1
    }
  }
  return(list(value=value,elements=elements))
}
