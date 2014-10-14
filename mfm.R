#many factors model
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


mfmrate <- function(data){
  r <- c()
  ind <- c()
  index <- readrealIndex()
  for(i in 1:length(data)){
    r <- c(r,data[[i]]$rate)
    ind <- c(ind,data[[i]]$industry)
  }
  dim(r) <- c(length(r)/30,30)
  
  dsvd <- svd(r)
  d <- dsvd$d
  u <- dsvd$u
  v <- dsvd$v
  
  u1 <- u[,1]
  v1 <- v[,1]
  d1 <- d[1]
  u2 <- u[,2]
  v2 <- v[,2]
  d2 <- d[2]
  u3 <- u[,3]
  v3 <- v[,3]
  d3 <- d[3]
  u4 <- u[,4]
  v4 <- v[,4]
  d4 <- d[4]
  
  err <- sum(d[-c(1,2,3,4)]^2)/sum(d^2)
  
  newr <- u1%*%t(v1)*d1+u2%*%t(v2)*d2+u3%*%t(v3)*d3+u4%*%t(v4)*d4
  nr <- sqrt(sum(r^2))
  nnr <- sqrt(sum((r-newr)^2))
  print(nnr/nr)
  
  #coefcorr <- cor(coef,index$rateI)
  #print(coefcorr)
  return(list(Factors = list(u1 = u1,u2 = u2, u3 = u3, u4 = u4), Err = err, V = v[,1:4],ind = ind))
}

mfmvolume <- function(data){
  r <- c()
  ind <- c()
  index <- readrealIndex()
  for(i in 1:length(data)){
    r <- c(r,data[[i]]$volume)
    ind <- c(ind,data[[i]]$industry)
  }
  dim(r) <- c(length(r)/30,30)
  
  dsvd <- svd(r)
  d <- dsvd$d
  u <- dsvd$u
  v <- dsvd$v
  
  u1 <- u[,1]
  v1 <- v[,1]
  d1 <- d[1]
  u2 <- u[,2]
  v2 <- v[,2]
  d2 <- d[2]
  u3 <- u[,3]
  v3 <- v[,3]
  d3 <- d[3]
  u4 <- u[,4]
  v4 <- v[,4]
  d4 <- d[4]
  
  err <- sum(d[-c(1,2,3,4)]^2)/sum(d^2)
  
  newr <- u1%*%t(v1)*d1+u2%*%t(v2)*d2+u3%*%t(v3)*d3+u4%*%t(v4)*d4
  nr <- sqrt(sum(r^2))
  nnr <- sqrt(sum((r-newr)^2))
  print(nnr/nr)
  
  #coefcorr <- cor(coef,index$volumeI)
  #print(coefcorr)
  return(list(Factors = list(u1 = u1,u2 = u2, u3 = u3, u4 = u4), Err = err, V = v[,1:4],ind = ind))
}
