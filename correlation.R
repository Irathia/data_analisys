#correlation

correlation <- function(data,k){
  ck <- cor(matrix(data = data, nrow = length(data)/k, ncol = k), use = "all.obs")
  return(ck)
}