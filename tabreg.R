#task 1

tabreg <- function(data){
  
  ind1 <- list()#Money
  ind2 <- list()#Other
  ind3 <- list()#Conglomerate
  ind4 <- list()#Realestate
  name <- names(data)
  
  for (i in 1:length(data)){
    if (data[[i]]$industry == "Money"){ 
      ind1[[name[i]]] <- list(rate=data[[i]]$rate,volume=data[[i]]$volume)
    }
    if (data[[i]]$industry == "Other"){
      ind2[[name[i]]] <- list(rate=data[[i]]$rate,volume=data[[i]]$volume)
    }
    if (data[[i]]$industry == "Conglomerat"){
      ind3[[name[i]]] <- list(rate=data[[i]]$rate,volume=data[[i]]$volume)
    }
    if(data[[i]]$industry == "Realestate"){
      ind4[[name[i]]] <- list(rate=data[[i]]$rate,volume=data[[i]]$volume)
    }
  }
  
  
  
  mean1 <- c()
  sd1 <- c()
  mean2 <- c()
  sd2 <- c()
  mean3 <- c()
  sd3 <- c()
  mean4 <- c()
  sd4 <- c()
  
  for (i in 1:length(ind1)){
    mean1 <- c(mean1, mean(ind1[[i]]$rate))
    sd1 <- c(sd1,sd(ind1[[i]]$rate))
  }
  
  for (i in 1:length(ind2)){
    mean2 <- c(mean2, mean(ind2[[i]]$rate))
    sd2 <- c(sd2,sd(ind2[[i]]$rate))
  }
  
  for (i in 1:length(ind3)){
    mean3 <- c(mean3, mean(ind3[[i]]$rate))
    sd3 <- c(sd3,sd(ind3[[i]]$rate))
  }
  
  for (i in 1:length(ind4)){
    mean4 <- c(mean4, mean(ind4[[i]]$rate))
    sd4 <- c(sd4,sd(ind4[[i]]$rate))
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
  #print(meanall - sdall)
  #print(meanall+sdall)
  y <- c(1,2,3,4)
  plot(meanall~y,cex=1.5,xaxt='n',col='blue',pch=16,ylim=c(-0.05,0.05))
  arrows(y,(meanall-sdall),y,(meanall+sdall),code=3,length=0.2,angle=90,col='red')

  #volume
# mean1 <- c()
# sd1 <- c()
# mean2 <- c()
# sd2 <- c()
# mean3 <- c()
# sd3 <- c()
# mean4 <- c()
# sd4 <- c()
# 
# for (i in 1:length(ind1)){
#   mean1 <- c(mean1, mean(ind1[[i]]$volume))
#   sd1 <- c(sd1,sd(ind1[[i]]$volume))
# }
# 
# for (i in 1:length(ind2)){
#   mean2 <- c(mean2, mean(ind2[[i]]$volume))
#   sd2 <- c(sd2,sd(ind2[[i]]$volume))
# }
# 
# for (i in 1:length(ind3)){
#   mean3 <- c(mean3, mean(ind3[[i]]$volume))
#   sd3 <- c(sd3,sd(ind3[[i]]$volume))
# }
# 
# for (i in 1:length(ind4)){
#   mean4 <- c(mean4, mean(ind4[[i]]$volume))
#   sd4 <- c(sd4,sd(ind4[[i]]$volume))
# }
# 
# mean1 = mean(mean1)
# sd1 = mean(sd1)
# mean2 = mean(mean2)
# sd2 = mean(sd2)
# mean3 = mean(mean3)
# sd3 = mean(sd3)
# mean4 = mean(mean4)
# sd4 = mean(sd4)
# meanall <- c(mean1,mean2,mean3,mean4)
# sdall <-c(sd1,sd2,sd3,sd4)
# print(meanall - sdall)
# print(meanall+sdall)
# y <- c(1,2,3,4)
# plot(meanall~y,cex=1.5,xaxt='n',col='blue',pch=16,ylim=c(10000,45000000))
# arrows(y,(meanall-sdall),y,(meanall+sdall),code=3,length=0.2,angle=90,col='red')
}

tabreg(data)