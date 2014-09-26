#read industries
readInd <- function(filename){
  industry = readLines(filename)
  industry = strsplit(industry,split = ";")
  ind <- c()
  for (i in 1:30){
    ind <- c(ind,(industry)[[i]][2])
  }
  return(ind)
}
