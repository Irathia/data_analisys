data(iris)
data <- as.matrix(iris)
l <- dim(data)[1]

d1 <- matrix(0,l,l)#evklidovo
d2 <- matrix(0,l,l)#gorodsk kv
d3 <- matrix(0,l,l)#minkovskogo p = 5
d4 <- matrix(0,l,l)#chebsheva
p <- 5

for(i in 1:l){
  for(j in 1:l){
    d1[i,j] <- sqrt(t(as.numeric(data[i,-5])-as.numeric(data[j,-5]))%*%(as.numeric(data[i,-5])-as.numeric(data[j,-5])))
    d2[i,j] <- sum(abs((as.numeric(data[i,-5])-as.numeric(data[j,-5]))))
    d3[i,j] <- (sum(abs(as.numeric(data[i,-5])-as.numeric(data[j,-5]))^p))^(1/p)
    d4[i,j] <- max(abs(as.numeric(data[i,-5])-as.numeric(data[j,-5])))
  }
}

write.csv(d1,file = "InputIris/test1.csv")
write.csv(d2,file = "InputIris/test2.csv")
write.csv(d3,file = "InputIris/test3.csv")
write.csv(d4,file = "InputIris/test4.csv")

write.csv(iris, file = "InputIris/sourcedata.csv")
