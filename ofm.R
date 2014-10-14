#one factor model
library(svd)

readrealIndex <- function(){
  dataI <- read.csv("table.csv", sep = ';')
  priceI <- dataI$Close
  volumeI <- dataI$Volume
  
  rateI <- vector(mode = "numeric", length = length(priceI)-1)
  for (i in 2:length(priceI)) {
    rateI[i-1] <- log((priceI[i])/priceI[i-1])
  }
  
  return(list(rateI = rateI, volumeI = volumeI))
}


ofmrate <- function(data){
  r <- c()
  index <- readrealIndex()
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
  coef <- r%*%w
  newr <- u1%*%t(v1)*d1
  nr <- sqrt(sum(r^2))
  nnr <- sqrt(sum((r-newr)^2))
  print(nnr/nr)
  coefcorr <- cor(coef,index$rateI)
  print(coefcorr)
  return(list(Factor = u1, Coef = coef, Err = err))
}

ofmvolume <- function(data){
  r <- c()
  index <- readrealIndex()
  for(i in 1:length(data)){
    r <- c(r,data[[i]]$volume)
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
  coef <- r%*%w
  newr <- u1%*%t(v1)*d1
  nr <- sqrt(sum(r^2))
  nnr <- sqrt(sum((r-newr)^2))
  print(nnr/nr)
  coefcorr <- cor(coef,index$volumeI)
  print(coefcorr)
  return(list(Factor = u1, Coef = coef, Err = err))
}

