quatelet_index <- function(X){
  w=dim(X)[2]
  h=dim(X)[1]
  
  Quatelet_Matrix=array(data=0,dim=c(w,h))
  for(i in 1:w){
    for(j in 1:h){
      Quatelet_Matrix[i,j] = (X[i,j]/sum(X))/((sum(X[i,])/sum(X)) * (sum(X[,j])/sum(X))) - 1
    }
  }
  
  Quatelet_Index = 0
  
  for(i in 1:w){
    for(j in 1:h){
      Quatelet_Index = Quatelet_Index + Quatelet_Matrix[i,j] * X[i,j] / sum(X)
    }
  }
  
  return(Quatelet_Index)
}

quat = quatelet_index(N)