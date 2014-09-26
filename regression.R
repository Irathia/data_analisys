# Regression

regression <- function(data){
  data_vector = c()
  for (i in 1:length(data)) {
    data_vector = c(data_vector, c(data[[i]]))
  }
  
  m <- matrix(data = data_vector, nrow = length(data_vector)/length(data), ncol = length(data))
  rm <- matrix(data = 0, nrow = length(data), ncol = length(data))
  for (i in 1:length(data)) {
    for (j in 1:length(data)) {
      rm[i,j] = lm(m[,i]~m[,j])[[1]][2]
    }
  }
  
  
  colnames(rm) <- c(names(data))
  rownames(rm) <- c(names(data))
  return(rm)
}