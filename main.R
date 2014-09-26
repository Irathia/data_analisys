source("readdata.R")
source("correlation.R")
source("regression.R")
source("readind.R")
source("tabreg.R")
source("neibors.R")

#data = readdata("test_data")

crate = correlation(data, "rate")
cvolume = correlation(data, "volume")

rrate = regression(data, "rate")
rvolume = regression(data, "volume")

ind <- readInd("filename.txt")
data <- list(rate=data$rate,value=data$value,date=data$date,filename=data$filename,industry = ind)
tabregrate(data)
tabregvalue(data)
neibors(data,k)
