library(tools)
readdataTennis <- function(dir){
  files <- list.files(dir)
  t <- getwd()
  setwd(dir)
  
  data <- list()
  for(i in 1:length(files)){
    data[[i]] <- read.csv(files[i], header = T)
  }
  names(data) <-  file_path_sans_ext(files)
  setwd(t)
  return(data)
}

dataTennis <- readdataTennis("Tennis-Major-Tournaments-Match-Statistics")

for(k in 1:length(dataTennis)){
  data <- as.matrix(dataTennis[[k]])
  l <- dim(data)[1]
  ll <- dim(data)[2]
  d1 <- matrix(0,l,l)#evklidovo
  d2 <- matrix(0,l,l)#gorodsk kv
  d3 <- matrix(0,l,l)#minkovskogo p = 5
  d4 <- matrix(0,l,l)#chebsheva
  p <- 5
  
  for(i in 1:l){
    for(j in 1:l){
      index <- which(is.na((data[i,])) != T & is.na((data[j,])) != T)
      index <- index[-c(1,2)]
      d1[i,j] <- sqrt(t(as.numeric(data[i,index])-as.numeric(data[j,index]))%*%(as.numeric(data[i,index])-as.numeric(data[j,index])))
      d2[i,j] <- sum(abs((as.numeric(data[i,index])-as.numeric(data[j,index]))))
      d3[i,j] <- (sum(abs(as.numeric(data[i,index])-as.numeric(data[j,index]))^p))^(1/p)
      d4[i,j] <- max(abs(as.numeric(data[i,index])-as.numeric(data[j,index])))
    }
  }
  
  dir.create(paste("InputTennis",names(dataTennis)[k], sep = '/'))
  write.csv(d1,file = paste("InputTennis",names(dataTennis)[k],"test1.csv", sep = '/'))
  write.csv(d2,file = paste("InputTennis",names(dataTennis)[k],"test2.csv", sep = '/'))
  write.csv(d3,file = paste("InputTennis",names(dataTennis)[k],"test3.csv", sep = '/'))
  write.csv(d4,file = paste("InputTennis",names(dataTennis)[k],"test4.csv", sep = '/'))
  
  cl1 <- hclust(as.dist(d1),method = "ward.D")
  tr1 <- cutree(cl1, k = 4)
  
  cl2 <- hclust(as.dist(d2),method = "ward.D")
  tr2 <- cutree(cl1, k = 4)
  
  cl3 <- hclust(as.dist(d3),method = "ward.D")
  tr3 <- cutree(cl3, k = 4)
  
  cl4 <- hclust(as.dist(d4),method = "ward.D")
  tr4 <- cutree(cl4, k = 4)
  
  write.csv(tr1,file = paste("InputTennis",names(dataTennis)[k],"source1.csv", sep = '/'))
  write.csv(tr2,file = paste("InputTennis",names(dataTennis)[k],"source2.csv", sep = '/'))
  write.csv(tr3,file = paste("InputTennis",names(dataTennis)[k],"source3.csv", sep = '/'))
  write.csv(tr4,file = paste("InputTennis",names(dataTennis)[k],"source4.csv", sep = '/'))
}