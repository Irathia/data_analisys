source("readdata.R")
source("correlation.R")
source("regression.R")
source("readind.R")
source("tabreg.R")
source("neibors.R")
source("tablecontingency.R")
source("hi_pow_2.R")

data = readdata("data")

# crate = correlation(data, "rate")
# cvolume = correlation(data, "volume")
# 
# rrate = regression(data, "rate")
# rvolume = regression(data, "volume")
# 
# ind <- readInd("filename.txt")
# for (i in 1:length(ind)){
#   data[[i]][["industry"]] <- ind[i]
# }
#data <- list(rate=data$rate,value=data$value,date=data$date,filename=data$filename,industry = ind)
#tabreg(data)
#neibors(data,k)
t <- c()
for(i in 1:length(data)){
  for(j in 1:length(data)){
    N = tablecontingency(data, "rate", names(data)[i], names(data)[j], 10)
    t = c(t,hi_pow_2(N,10))
  }
}

t = matrix(t,length(data),length(data))