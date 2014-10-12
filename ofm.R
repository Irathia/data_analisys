#one factor model
library(svd)

ofm <- function(data){
  r <- c()
  for(i in 1:length(data)){
    r <- c(r,data[[i]]$rate)
  }
  dim(r) <- c(length(r)/30,30)
  
  dsvd <- svd(r)
  d <- dsvd$d
  u <- dsvd$u
  v <- dsvd$v
  
  u1 <- u[,1]
  v1 <- v[,1]
  d1 <- d[1]
  
  err <- sum(d[-1]^2)/sum(d^2)
  
  w <- v1/sqrt(d1)
  coef <- c()
  rdim <- dim(r)
  coef <- a%*%w
  
  return(list(Factor = u1, Coef = coef, Err = err))
}