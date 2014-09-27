source("readdata.R")
source("correlation.R")
source("regression.R")
source("readind.R")
source("tabreg.R")
source("neibors.R")

data = readdata("data")

#crate = correlation(data, "rate")
#cvolume = correlation(data, "volume")

#rrate = regression(data, "rate")
#rvolume = regression(data, "volume")

ind <- readInd("filename.txt")
for (i in 1:length(ind)){
  data[[i]][["industry"]] <- ind[i]
}
#data <- list(rate=data$rate,value=data$value,date=data$date,filename=data$filename,industry = ind)
#tabreg(data)
#tabregvalue(data)
#neibors(data,k)
