#clasters

hcls <- function(data){
  #евклидово расстояние
  d1 <- matrix(0,length(data),length(data))
  #расстояние городских кварталов
  d2 <- matrix(0,length(data),length(data))
  #расстояние Чебышева
  d3 <- matrix(0,length(data),length(data))
  #Расстояние Минковского p = 5
  d4 <- matrix(0,length(data),length(data))
  p <- 5
  for(i in 1:length(data)){
    for(j in 1:length(data)){
      x <- data[[i]]$rate
      y <- data[[j]]$rate
      d1[i,j] <- sqrt(t(x-y)%*%(x-y))
      d2[i,j] <- sum(abs(x-y))
      d3[i,j] <- max(abs(x-y))
      d4[i,j] <- (sum(abs(x-y)^p))^(1/p)
    }
  }
  
  cl1 <- hclust(as.dist(d1),method = "ward.D")
  cl2 <- hclust(as.dist(d2),method = "ward.D")
  cl3 <- hclust(as.dist(d3),method = "ward.D")
  cl4 <- hclust(as.dist(d4),method = "ward.D")
  
  tr1_2 <- cutree(cl1,k = 2)
  tr2_2 <- cutree(cl2,k = 2)
  tr3_2 <- cutree(cl3,k = 2)
  tr4_2 <- cutree(cl4,k = 2)
  
  tr1_3 <- cutree(cl1,k = 3)
  tr2_3 <- cutree(cl2,k = 3)
  tr3_3 <- cutree(cl3,k = 3)
  tr4_3 <- cutree(cl4,k = 3)
  
  tr1_4 <- cutree(cl1,k = 4)
  tr2_4 <- cutree(cl2,k = 4)
  tr3_4 <- cutree(cl3,k = 4)
  tr4_4 <- cutree(cl4,k = 4)
  
  tr1_5 <- cutree(cl1,k = 5)
  tr2_5 <- cutree(cl2,k = 5)
  tr3_5 <- cutree(cl3,k = 5)
  tr4_5 <- cutree(cl4,k = 5)
  
  

#расстояние между кластерами
  #type 1 - cl1, type2 - cl2, type3 - cl3, type4 - cl4
  type12d1 <- matrix(0,2,2)#метод ближайшего соседа
  type12d2 <- matrix(0,2,2)#метод дальнего соседа
  type12d3 <- matrix(0,2,2)#метод медиан
  type12d4 <- matrix(0,2,2)#метод Варда

  type13d1 <- matrix(0,3,3)#метод ближайшего соседа
  type13d2 <- matrix(0,3,3)#метод дальнего соседа
  type13d3 <- matrix(0,3,3)#метод медиан
  type13d4 <- matrix(0,3,3)#метод Варда

  type14d1 <- matrix(0,4,4)#метод ближайшего соседа
  type14d2 <- matrix(0,4,4)#метод дальнего соседа
  type14d3 <- matrix(0,4,4)#метод медиан
  type14d4 <- matrix(0,4,4)#метод Варда
  
  type15d1 <- matrix(0,5,5)#метод ближайшего соседа
  type15d2 <- matrix(0,5,5)#метод дальнего соседа
  type15d3 <- matrix(0,5,5)#метод медиан
  type15d4 <- matrix(0,5,5)#метод Варда

  type12 <- list(d1 = type12d1, d2 = type12d2, d3 = type12d3, d4 = type12d4)
  type13 <- list(d1 = type13d1, d2 = type13d2, d3 = type13d3, d4 = type13d4)
  type14 <- list(d1 = type14d1, d2 = type14d2, d3 = type14d3, d4 = type14d4)
  type15 <- list(d1 = type15d1, d2 = type15d2, d3 = type15d3, d4 = type15d4)
  type1 <- list(cl2 = type12, cl3 = type13, cl4 = type14, cl5 = type15)
  
  type22d1 <- matrix(0,2,2)#метод ближайшего соседа
  type22d2 <- matrix(0,2,2)#метод дальнего соседа
  type22d3 <- matrix(0,2,2)#метод медиан
  type22d4 <- matrix(0,2,2)#метод Варда
  
  type23d1 <- matrix(0,3,3)#метод ближайшего соседа
  type23d2 <- matrix(0,3,3)#метод дальнего соседа
  type23d3 <- matrix(0,3,3)#метод медиан
  type23d4 <- matrix(0,3,3)#метод Варда
  
  type24d1 <- matrix(0,4,4)#метод ближайшего соседа
  type24d2 <- matrix(0,4,4)#метод дальнего соседа
  type24d3 <- matrix(0,4,4)#метод медиан
  type24d4 <- matrix(0,4,4)#метод Варда
  
  type25d1 <- matrix(0,5,5)#метод ближайшего соседа
  type25d2 <- matrix(0,5,5)#метод дальнего соседа
  type25d3 <- matrix(0,5,5)#метод медиан
  type25d4 <- matrix(0,5,5)#метод Варда
  
  type22 <- list(d1 = type22d1, d2 = type22d2, d3 = type22d3, d4 = type22d4)
  type23 <- list(d1 = type23d1, d2 = type23d2, d3 = type23d3, d4 = type23d4)
  type24 <- list(d1 = type24d1, d2 = type24d2, d3 = type24d3, d4 = type24d4)
  type25 <- list(d1 = type25d1, d2 = type25d2, d3 = type25d3, d4 = type25d4)
  type2 <- list(cl2 = type22, cl3 = type23, cl4 = type24, cl5 = type25)

  type32d1 <- matrix(0,2,2)#метод ближайшего соседа
  type32d2 <- matrix(0,2,2)#метод дальнего соседа
  type32d3 <- matrix(0,2,2)#метод медиан
  type32d4 <- matrix(0,2,2)#метод Варда
  
  type33d1 <- matrix(0,3,3)#метод ближайшего соседа
  type33d2 <- matrix(0,3,3)#метод дальнего соседа
  type33d3 <- matrix(0,3,3)#метод медиан
  type33d4 <- matrix(0,3,3)#метод Варда
  
  type34d1 <- matrix(0,4,4)#метод ближайшего соседа
  type34d2 <- matrix(0,4,4)#метод дальнего соседа
  type34d3 <- matrix(0,4,4)#метод медиан
  type34d4 <- matrix(0,4,4)#метод Варда
  
  type35d1 <- matrix(0,5,5)#метод ближайшего соседа
  type35d2 <- matrix(0,5,5)#метод дальнего соседа
  type35d3 <- matrix(0,5,5)#метод медиан
  type35d4 <- matrix(0,5,5)#метод Варда
  
  type32 <- list(d1 = type32d1, d2 = type32d2, d3 = type32d3, d4 = type32d4)
  type33 <- list(d1 = type33d1, d2 = type33d2, d3 = type33d3, d4 = type33d4)
  type34 <- list(d1 = type34d1, d2 = type34d2, d3 = type34d3, d4 = type34d4)
  type35 <- list(d1 = type35d1, d2 = type35d2, d3 = type35d3, d4 = type35d4)
  type3 <- list(cl22 = type32, cl3 = type33, cl4 = type34, cl5 = type35)
  
  type42d1 <- matrix(0,2,2)#метод ближайшего соседа
  type42d2 <- matrix(0,2,2)#метод дальнего соседа
  type42d3 <- matrix(0,2,2)#метод медиан
  type42d4 <- matrix(0,2,2)#метод Варда
  
  type43d1 <- matrix(0,3,3)#метод ближайшего соседа
  type43d2 <- matrix(0,3,3)#метод дальнего соседа
  type43d3 <- matrix(0,3,3)#метод медиан
  type43d4 <- matrix(0,3,3)#метод Варда
  
  type44d1 <- matrix(0,4,4)#метод ближайшего соседа
  type44d2 <- matrix(0,4,4)#метод дальнего соседа
  type44d3 <- matrix(0,4,4)#метод медиан
  type44d4 <- matrix(0,4,4)#метод Варда
  
  type45d1 <- matrix(0,5,5)#метод ближайшего соседа
  type45d2 <- matrix(0,5,5)#метод дальнего соседа
  type45d3 <- matrix(0,5,5)#метод медиан
  type45d4 <- matrix(0,5,5)#метод Варда

  type42 <- list(d1 = type42d1, d2 = type42d2, d3 = type42d3, d4 = type42d4)
  type43 <- list(d1 = type43d1, d2 = type43d2, d3 = type43d3, d4 = type43d4)
  type44 <- list(d1 = type44d1, d2 = type44d2, d3 = type44d3, d4 = type44d4)
  type45 <- list(d1 = type45d1, d2 = type45d2, d3 = type45d3, d4 = type45d4)
  type4 <- list(cl2 = type42, cl3 = type43, cl4 = type44, cl5 = type45)

  distance <- list(type1 = type1, type2 = type2, type3 = type3, type4 = type4)
  for(k in 1:length(cl)){
    for(t in 1:length(cl[[k]])){
      n <- max(cl[[k]][[t]])
      for(i in 1:n){
        for(j in 1:n){
          x <- data[which(cl[[k]][[t]] == i)]
          y <- data[which(cl[[k]][[t]] == j)]
          
          
        
          if (t == 1){
            xy <- c()
            xs <- 0
            ys <- 0
            for(i1 in 1:length(x)){
              xs <- (xs+x[[i1]]$rate)/length(x)
              ys <- 0
              for(i2 in 1:length(y)){
                ys <- (ys+y[[i2]]$rate)/length(y)
                xy <- c(xy, sqrt(t(x[[i1]]$rate-y[[i2]]$rate)%*%(x[[i1]]$rate-y[[i2]]$rate)))
              }
            }
            distance[[t]][[k]]$d1[i,j] <- min(xy)
            distance[[t]][[k]]$d2[i,j] <- max(xy)
            distance[[t]][[k]]$d3[i,j] <- sqrt(t(xs-ys)%*%(xs-ys))
            distance[[t]][[k]]$d4[i,j] <- (length(x)*length(y))/(length(x)+length(y))*(sqrt(t(xs-ys)%*%(xs-ys)))
          }
          if (t == 2){
            xs <- 0
            ys <- 0
            xy <- c()
            for(i1 in 1:length(x)){
              xs <- (xs+x[[i1]]$rate)/length(x)
              ys <- 0
              for(i2 in 1:length(y)){
                ys <- (ys+y[[i2]]$rate)/length(y)
                xy <- c(xy, sum(abs(x[[i1]]$rate-y[[i2]]$rate)))
              }
            }
            distance[[t]][[k]]$d1[i,j] <- min(xy)
            distance[[t]][[k]]$d2[i,j] <- max(xy)
            distance[[t]][[k]]$d3[i,j] <- sum(abs(xs-ys))
            distance[[t]][[k]]$d4[i,j] <- (length(x)*length(y))/(length(x)+length(y))*sum(abs(xs-ys))
          }
          if (t == 3){
            xs <- 0
            ys <- 0
            xy <- c()
            for(i1 in 1:length(x)){
              xs <- (xs+x[[i1]]$rate)/length(x)
              ys <- 0
              for(i2 in 1:length(y)){
                ys <- (ys+y[[i2]]$rate)/length(y)
                xy <- c(xy, max(abs(x[[i1]]$rate-y[[i2]]$rate)))
              }
            }
            distance[[t]][[k]]$d1[i,j] <- min(xy)
            distance[[t]][[k]]$d2[i,j] <- max(xy)
            distance[[t]][[k]]$d3[i,j] <- max(abs(xs-ys))
            distance[[t]][[k]]$d4[i,j] <- (length(x)*length(y))/(length(x)+length(y))*max(abs(xs-ys))
          }
          if (t == 4){
            xs <- 0
            ys <- 0
            xy <- c()
            for(i1 in 1:length(x)){
              xs <- (xs+x[[i1]]$rate)/length(x)
              ys <- 0
              for(i2 in 1:length(y)){
                ys <- (ys+y[[i2]]$rate)/length(y)
                xy <- c(xy, (sum(abs(x[[i1]]$rate-y[[i2]]$rate)^p))^(1/p))
              }
            }
            distance[[t]][[k]]$d1[i,j] <- min(xy)
            distance[[t]][[k]]$d2[i,j] <- max(xy)
            distance[[t]][[k]]$d3[i,j] <- (sum(abs(xs-ys)^p))^(1/p)
            distance[[t]][[k]]$d4[i,j] <- (length(x)*length(y))/(length(x)+length(y))*(sum(abs(xs-ys)^p))^(1/p)
          }
        }
      }
    }
  }
  
return(list(distance = distance,
            cl = list(clasters2 = list(cl1 = tr1_2, cl2 = tr2_2,cl3 = tr3_2,cl4 = tr4_2), 
                      clasters3 = list(cl1 = tr1_3, cl2 = tr2_3,cl3 = tr3_3,cl4 = tr4_3),
                      clasters4 = list(cl1 = tr1_4, cl2 = tr2_4,cl3 = tr3_4,cl4 = tr4_4),
                      clasters5 = list(cl1 = tr1_5, cl2 = tr2_5,cl3 = tr3_5,cl4 = tr4_5))))
}

#k средних

ksrcls <- function(data){
  #евклидово расстояние
  d1 <- matrix(0,length(data),length(data))
  #расстояние городских кварталов
  d2 <- matrix(0,length(data),length(data))
  #расстояние Чебышева
  d3 <- matrix(0,length(data),length(data))
  #Расстояние Минковского p = 5
  d4 <- matrix(0,length(data),length(data))
  p <- 5
  for(i in 1:length(data)){
    for(j in 1:length(data)){
      x <- data[[i]]$rate
      y <- data[[j]]$rate
      d1[i,j] <- sqrt(t(x-y)%*%(x-y))
      d2[i,j] <- sum(abs(x-y))
      d3[i,j] <- max(abs(x-y))
      d4[i,j] <- (sum(abs(x-y)^p))^(1/p)
    }
  }
  
  
  tr1_2 <- unname(kmeans(as.dist(d1),2)$cluster)
  tr2_2 <- unname(kmeans(as.dist(d2),2)$cluster)
  tr3_2 <- unname(kmeans(as.dist(d3),2)$cluster)
  tr4_2 <- unname(kmeans(as.dist(d4),2)$cluster)
  
  tr1_3 <- unname(kmeans(as.dist(d1),3)$cluster)
  tr2_3 <- unname(kmeans(as.dist(d2),3)$cluster)
  tr3_3 <- unname(kmeans(as.dist(d3),3)$cluster)
  tr4_3 <- unname(kmeans(as.dist(d4),3)$cluster)
  
  tr1_4 <- unname(kmeans(as.dist(d1),4)$cluster)
  tr2_4 <- unname(kmeans(as.dist(d2),4)$cluster)
  tr3_4 <- unname(kmeans(as.dist(d3),4)$cluster)
  tr4_4 <- unname(kmeans(as.dist(d4),4)$cluster)
  
  tr1_5 <- unname(kmeans(as.dist(d1),5)$cluster)
  tr2_5 <- unname(kmeans(as.dist(d2),5)$cluster)
  tr3_5 <- unname(kmeans(as.dist(d3),5)$cluster)
  tr4_5 <- unname(kmeans(as.dist(d4),5)$cluster)
  
  
  
  #расстояние между кластерами
  #type 1 - cl1, type2 - cl2, type3 - cl3, type4 - cl4
  type12d1 <- matrix(0,2,2)#метод ближайшего соседа
  type12d2 <- matrix(0,2,2)#метод дальнего соседа
  type12d3 <- matrix(0,2,2)#метод медиан
  type12d4 <- matrix(0,2,2)#метод Варда
  
  type13d1 <- matrix(0,3,3)#метод ближайшего соседа
  type13d2 <- matrix(0,3,3)#метод дальнего соседа
  type13d3 <- matrix(0,3,3)#метод медиан
  type13d4 <- matrix(0,3,3)#метод Варда
  
  type14d1 <- matrix(0,4,4)#метод ближайшего соседа
  type14d2 <- matrix(0,4,4)#метод дальнего соседа
  type14d3 <- matrix(0,4,4)#метод медиан
  type14d4 <- matrix(0,4,4)#метод Варда
  
  type15d1 <- matrix(0,5,5)#метод ближайшего соседа
  type15d2 <- matrix(0,5,5)#метод дальнего соседа
  type15d3 <- matrix(0,5,5)#метод медиан
  type15d4 <- matrix(0,5,5)#метод Варда
  
  type12 <- list(d1 = type12d1, d2 = type12d2, d3 = type12d3, d4 = type12d4)
  type13 <- list(d1 = type13d1, d2 = type13d2, d3 = type13d3, d4 = type13d4)
  type14 <- list(d1 = type14d1, d2 = type14d2, d3 = type14d3, d4 = type14d4)
  type15 <- list(d1 = type15d1, d2 = type15d2, d3 = type15d3, d4 = type15d4)
  type1 <- list(cl2 = type12, cl3 = type13, cl4 = type14, cl5 = type15)
  
  type22d1 <- matrix(0,2,2)#метод ближайшего соседа
  type22d2 <- matrix(0,2,2)#метод дальнего соседа
  type22d3 <- matrix(0,2,2)#метод медиан
  type22d4 <- matrix(0,2,2)#метод Варда
  
  type23d1 <- matrix(0,3,3)#метод ближайшего соседа
  type23d2 <- matrix(0,3,3)#метод дальнего соседа
  type23d3 <- matrix(0,3,3)#метод медиан
  type23d4 <- matrix(0,3,3)#метод Варда
  
  type24d1 <- matrix(0,4,4)#метод ближайшего соседа
  type24d2 <- matrix(0,4,4)#метод дальнего соседа
  type24d3 <- matrix(0,4,4)#метод медиан
  type24d4 <- matrix(0,4,4)#метод Варда
  
  type25d1 <- matrix(0,5,5)#метод ближайшего соседа
  type25d2 <- matrix(0,5,5)#метод дальнего соседа
  type25d3 <- matrix(0,5,5)#метод медиан
  type25d4 <- matrix(0,5,5)#метод Варда
  
  type22 <- list(d1 = type22d1, d2 = type22d2, d3 = type22d3, d4 = type22d4)
  type23 <- list(d1 = type23d1, d2 = type23d2, d3 = type23d3, d4 = type23d4)
  type24 <- list(d1 = type24d1, d2 = type24d2, d3 = type24d3, d4 = type24d4)
  type25 <- list(d1 = type25d1, d2 = type25d2, d3 = type25d3, d4 = type25d4)
  type2 <- list(cl2 = type22, cl3 = type23, cl4 = type24, cl5 = type25)
  
  type32d1 <- matrix(0,2,2)#метод ближайшего соседа
  type32d2 <- matrix(0,2,2)#метод дальнего соседа
  type32d3 <- matrix(0,2,2)#метод медиан
  type32d4 <- matrix(0,2,2)#метод Варда
  
  type33d1 <- matrix(0,3,3)#метод ближайшего соседа
  type33d2 <- matrix(0,3,3)#метод дальнего соседа
  type33d3 <- matrix(0,3,3)#метод медиан
  type33d4 <- matrix(0,3,3)#метод Варда
  
  type34d1 <- matrix(0,4,4)#метод ближайшего соседа
  type34d2 <- matrix(0,4,4)#метод дальнего соседа
  type34d3 <- matrix(0,4,4)#метод медиан
  type34d4 <- matrix(0,4,4)#метод Варда
  
  type35d1 <- matrix(0,5,5)#метод ближайшего соседа
  type35d2 <- matrix(0,5,5)#метод дальнего соседа
  type35d3 <- matrix(0,5,5)#метод медиан
  type35d4 <- matrix(0,5,5)#метод Варда
  
  type32 <- list(d1 = type32d1, d2 = type32d2, d3 = type32d3, d4 = type32d4)
  type33 <- list(d1 = type33d1, d2 = type33d2, d3 = type33d3, d4 = type33d4)
  type34 <- list(d1 = type34d1, d2 = type34d2, d3 = type34d3, d4 = type34d4)
  type35 <- list(d1 = type35d1, d2 = type35d2, d3 = type35d3, d4 = type35d4)
  type3 <- list(cl22 = type32, cl3 = type33, cl4 = type34, cl5 = type35)
  
  type42d1 <- matrix(0,2,2)#метод ближайшего соседа
  type42d2 <- matrix(0,2,2)#метод дальнего соседа
  type42d3 <- matrix(0,2,2)#метод медиан
  type42d4 <- matrix(0,2,2)#метод Варда
  
  type43d1 <- matrix(0,3,3)#метод ближайшего соседа
  type43d2 <- matrix(0,3,3)#метод дальнего соседа
  type43d3 <- matrix(0,3,3)#метод медиан
  type43d4 <- matrix(0,3,3)#метод Варда
  
  type44d1 <- matrix(0,4,4)#метод ближайшего соседа
  type44d2 <- matrix(0,4,4)#метод дальнего соседа
  type44d3 <- matrix(0,4,4)#метод медиан
  type44d4 <- matrix(0,4,4)#метод Варда
  
  type45d1 <- matrix(0,5,5)#метод ближайшего соседа
  type45d2 <- matrix(0,5,5)#метод дальнего соседа
  type45d3 <- matrix(0,5,5)#метод медиан
  type45d4 <- matrix(0,5,5)#метод Варда
  
  type42 <- list(d1 = type42d1, d2 = type42d2, d3 = type42d3, d4 = type42d4)
  type43 <- list(d1 = type43d1, d2 = type43d2, d3 = type43d3, d4 = type43d4)
  type44 <- list(d1 = type44d1, d2 = type44d2, d3 = type44d3, d4 = type44d4)
  type45 <- list(d1 = type45d1, d2 = type45d2, d3 = type45d3, d4 = type45d4)
  type4 <- list(cl2 = type42, cl3 = type43, cl4 = type44, cl5 = type45)
  
  distance <- list(type1 = type1, type2 = type2, type3 = type3, type4 = type4)
  for(k in 1:length(cl)){
    for(t in 1:length(cl[[k]])){
      n <- max(cl[[k]][[t]])
      for(i in 1:n){
        for(j in 1:n){
          x <- data[which(cl[[k]][[t]] == i)]
          y <- data[which(cl[[k]][[t]] == j)]
          
          
          
          if (t == 1){
            xy <- c()
            xs <- 0
            ys <- 0
            for(i1 in 1:length(x)){
              xs <- (xs+x[[i1]]$rate)/length(x)
              ys <- 0
              for(i2 in 1:length(y)){
                ys <- (ys+y[[i2]]$rate)/length(y)
                xy <- c(xy, sqrt(t(x[[i1]]$rate-y[[i2]]$rate)%*%(x[[i1]]$rate-y[[i2]]$rate)))
              }
            }
            distance[[t]][[k]]$d1[i,j] <- min(xy)
            distance[[t]][[k]]$d2[i,j] <- max(xy)
            distance[[t]][[k]]$d3[i,j] <- sqrt(t(xs-ys)%*%(xs-ys))
            distance[[t]][[k]]$d4[i,j] <- (length(x)*length(y))/(length(x)+length(y))*(sqrt(t(xs-ys)%*%(xs-ys)))
          }
          if (t == 2){
            xs <- 0
            ys <- 0
            xy <- c()
            for(i1 in 1:length(x)){
              xs <- (xs+x[[i1]]$rate)/length(x)
              ys <- 0
              for(i2 in 1:length(y)){
                ys <- (ys+y[[i2]]$rate)/length(y)
                xy <- c(xy, sum(abs(x[[i1]]$rate-y[[i2]]$rate)))
              }
            }
            distance[[t]][[k]]$d1[i,j] <- min(xy)
            distance[[t]][[k]]$d2[i,j] <- max(xy)
            distance[[t]][[k]]$d3[i,j] <- sum(abs(xs-ys))
            distance[[t]][[k]]$d4[i,j] <- (length(x)*length(y))/(length(x)+length(y))*sum(abs(xs-ys))
          }
          if (t == 3){
            xs <- 0
            ys <- 0
            xy <- c()
            for(i1 in 1:length(x)){
              xs <- (xs+x[[i1]]$rate)/length(x)
              ys <- 0
              for(i2 in 1:length(y)){
                ys <- (ys+y[[i2]]$rate)/length(y)
                xy <- c(xy, max(abs(x[[i1]]$rate-y[[i2]]$rate)))
              }
            }
            distance[[t]][[k]]$d1[i,j] <- min(xy)
            distance[[t]][[k]]$d2[i,j] <- max(xy)
            distance[[t]][[k]]$d3[i,j] <- max(abs(xs-ys))
            distance[[t]][[k]]$d4[i,j] <- (length(x)*length(y))/(length(x)+length(y))*max(abs(xs-ys))
          }
          if (t == 4){
            xs <- 0
            ys <- 0
            xy <- c()
            for(i1 in 1:length(x)){
              xs <- (xs+x[[i1]]$rate)/length(x)
              ys <- 0
              for(i2 in 1:length(y)){
                ys <- (ys+y[[i2]]$rate)/length(y)
                xy <- c(xy, (sum(abs(x[[i1]]$rate-y[[i2]]$rate)^p))^(1/p))
              }
            }
            distance[[t]][[k]]$d1[i,j] <- min(xy)
            distance[[t]][[k]]$d2[i,j] <- max(xy)
            distance[[t]][[k]]$d3[i,j] <- (sum(abs(xs-ys)^p))^(1/p)
            distance[[t]][[k]]$d4[i,j] <- (length(x)*length(y))/(length(x)+length(y))*(sum(abs(xs-ys)^p))^(1/p)
          }
        }
      }
    }
  }
  #return(tr1_2)
  return(list(distance = distance,
              cl = list(clasters2 = list(cl1 = tr1_2, cl2 = tr2_2,cl3 = tr3_2,cl4 = tr4_2), 
                         clasters3 = list(cl1 = tr1_3, cl2 = tr2_3,cl3 = tr3_3,cl4 = tr4_3),
                        clasters4 = list(cl1 = tr1_4, cl2 = tr2_4,cl3 = tr3_4,cl4 = tr4_4),
                        clasters5 = list(cl1 = tr1_5, cl2 = tr2_5,cl3 = tr3_5,cl4 = tr4_5))))
}