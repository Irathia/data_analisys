#correlation

correlation <- function(data, data_type){
  data_vector = c()
  for (i in 1:length(data)) {
    data_vector = c(data_vector, c(data[[i]][[data_type]]))
  }
  
  m <- matrix(data = data_vector, nrow = length(data_vector)/length(data), ncol = length(data))
  
  ck <- cor(m, use = "all.obs")
  colnames(ck) <- c(names(data))
  rownames(ck) <- c(names(data))
  return(ck)
}