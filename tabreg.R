#task 1

tabregrate <- function(data){
  
  ind1 <- data$rate[,which(data$industry == "Money")]
  ind2 <- data$rate[,which(data$industry == "Other")]
  ind3 <- data$rate[,which(data$industry == "Conglomerat")]
  ind4 <- data$rate[,which(data$industry == "Realestate")]
  
  mean1 <- c()
  sd1 <- c()
  mean2 <- c()
  sd2 <- c()
  mean3 <- c()
  sd3 <- c()
  mean4 <- c()
  sd4 <- c()
  
  for (i in 1:dim(ind1)[2]){
    mean1 <- c(mean1, mean(ind1[,i]))
    sd1 <- c(sd1,sd(ind1[,i]))
  }
  
  for (i in 1:dim(ind2)[2]){
    mean2 <- c(mean2, mean(ind2[,i]))
    sd2 <- c(sd2,sd(ind2[,i]))
  }
  
  for (i in 1:dim(ind3)[2]){
    mean3 <- c(mean3, mean(ind3[,i]))
    sd3 <- c(sd3,sd(ind3[,i]))
  }
  
  for (i in 1:dim(ind4)[2]){
    mean4 <- c(mean4, mean(ind4[,i]))
    sd4 <- c(sd4,sd(ind4[,i]))
  }
  
  mean1 = mean(mean1)
  sd1 = mean(sd1)
  mean2 = mean(mean2)
  sd2 = mean(sd2)
  mean3 = mean(mean3)
  sd3 = mean(sd3)
  mean4 = mean(mean4)
  sd4 = mean(sd4)
  meanall <- c(mean1,mean2,mean3,mean4)
  sdall <-c(sd1,sd2,sd3,sd4)
  print(meanall - sdall)
  print(meanall+sdall)
  y <- c(1,2,3,4)
  plot(meanall~y,cex=1.5,xaxt='n',col='blue',pch=16,ylim=c(-0.05,0.05))
  arrows(y,(meanall-sdall),y,(meanall+sdall),code=3,length=0.2,angle=90,col='red')
}

tabregvalue <- function(data){
  
  ind1 <- data$value[,which(data$industry == "Money")]
  ind2 <- data$value[,which(data$industry == "Other")]
  ind3 <- data$value[,which(data$industry == "Conglomerat")]
  ind4 <- data$value[,which(data$industry == "Realestate")]
  
  mean1 <- c()
  sd1 <- c()
  mean2 <- c()
  sd2 <- c()
  mean3 <- c()
  sd3 <- c()
  mean4 <- c()
  sd4 <- c()
  
  for (i in 1:dim(ind1)[2]){
    mean1 <- c(mean1, mean(ind1[,i]))
    sd1 <- c(sd1,sd(ind1[,i]))
  }
  
  for (i in 1:dim(ind2)[2]){
    mean2 <- c(mean2, mean(ind2[,i]))
    sd2 <- c(sd2,sd(ind2[,i]))
  }
  
  for (i in 1:dim(ind3)[2]){
    mean3 <- c(mean3, mean(ind3[,i]))
    sd3 <- c(sd3,sd(ind3[,i]))
  }
  
  for (i in 1:dim(ind4)[2]){
    mean4 <- c(mean4, mean(ind4[,i]))
    sd4 <- c(sd4,sd(ind4[,i]))
  }
  
  mean1 = mean(mean1)
  sd1 = mean(sd1)
  mean2 = mean(mean2)
  sd2 = mean(sd2)
  mean3 = mean(mean3)
  sd3 = mean(sd3)
  mean4 = mean(mean4)
  sd4 = mean(sd4)
  meanall <- c(mean1,mean2,mean3,mean4)
  sdall <-c(sd1,sd2,sd3,sd4)
  print(meanall - sdall)
  print(meanall+sdall)
  y <- c(1,2,3,4)
  plot(meanall~y,cex=1.5,xaxt='n',col='blue',pch=16,ylim=c(10000,45000000))
  arrows(y,(meanall-sdall),y,(meanall+sdall),code=3,length=0.2,angle=90,col='red')
}
