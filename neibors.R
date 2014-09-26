#Knn
library(class)

neibors <- function(data,k){
  a <- c()
  for (i in 1:10){
    a <- c(a,list(data$rate[,i],data$value[,i]))
  }
  dim(a) = c(2,10)
  b <- c()
  for (i in 11:30){
    b <- c(b,list(data$rate[,i],data$value[,i]))
  }
  dim(b) = c(2,20)
  f <- knn(a,b,data$industry[1:10],k = k)
  return(f)
}