#' myclt
#'
#' @param n number
#' @param iter number of iterations
#'
#' @return none
#' @export
#'
myclt=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nrow=n, byrow=TRUE) # B
  sm=apply(data,2,sum) # C
  hist(sm)
  sm
}
