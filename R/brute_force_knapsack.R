#' @name brute_force_knapsack
#' @title Solution to knapsack problem using Brute-Force Algorithm
#' @description The function implements solution to knapsack problem using Brute-Force Algorithm [wiki](https://en.wikipedia.org/wiki/Knapsack).
#' @param x Data.frame Contains 2 columns "w" and "v" containing the weight of sacks and value of sacks respectively.
#' @param W Numeric. Specifies the capacity of knapsack.
#' @param ... Optional logical parameter parallel that defaults to FALSE. When set to true performs parallelised execution of brute force algorithm.
#' @return Named List
#' @import parallel
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @export
#' 
#' @examples
#' #Prepare Data:
#' suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' knapsack_objects<-data.frame(w=sample(1:4000,size=2000,replace=TRUE),v=runif(n=2000,0,10000))
#' 
#' ptm <- proc.time()
#' a<-brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000)
#' print(proc.time() - ptm) # 0.404 milliseconds
#' print(a)
#' 
#' ptm <- proc.time()
#' library(parallel)
#' b<-brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000,parallel=TRUE)
#' print(proc.time() - ptm) # 0.104 milliseconds
#' print(b)
#' 
#' @seealso
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}

brute_force_knapsack<- function(x, W,...){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W)&&(W>0))
  stopifnot(all(colnames(x)==c("w","v")))
  stopifnot(all(sapply(x,function(x) x>0))&&all(sapply(x,function(x) is.numeric(x))))
  z<-list(...)
  n<-nrow(x)
  value <- 0
  elements <- NULL
  
  seriesExecution=TRUE
  if(!is.null(z$parallel)){
    if(z$parallel==TRUE){
      seriesExecution=FALSE
      
      chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      if (nzchar(chk) && chk == "TRUE") {
        num_workers <- 2L
      } else {
        num_workers <- parallel::detectCores()
      }
      
      cl <- makeCluster(num_workers,type="PSOCK")
      allCombinations<-function(i,n,cx,W){
        combination <- as.logical(intToBits(i)[1:n])
        totalWeight <- sum(cx$w[combination])
        totalValue <- sum(cx$v[combination])
        if(totalWeight<=W) {
          return(list(value = totalValue, elements = which(combination)))
        }else{return(NULL)}
      }
      allResults<-parLapply(cl,0:(2^n-1),allCombinations,cx=x,W=W,n=n)
      stopCluster(cl)
      
      value<-0
      elements<-NULL
      for (result in allResults) {
        if (!is.null(result) && result$value > value) {
          value <- result$value
          elements <- result$elements
        }
      }
    }
  }
  if(seriesExecution){
    for(i in 0:(2^n-1)) {
      combination <- as.logical(intToBits(i)[1:n])
      totalWeight <- sum(x$w[combination])
      totalValue <- sum(x$v[combination])
      if(totalWeight<=W && totalValue>value) {
        value <- totalValue
        elements <- which(combination)
      }
    }
  }
  return(list(value=value, elements=elements))
}