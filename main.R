source("readdata.R")
source("correlation.R")
source("regression.R")
source("readind.R")
source("tabreg.R")

#data = readdata("test_data")

crate = correlation(data, "rate")
cvolume = correlation(data, "volume")

rrate = regression(data, "rate")
rvolume = regression(data, "volume")

#data = readdata("data")

#ind <- readInd("filename.txt")
#ndata <- list(rate=data$rate,volume=data$volume,date=data$date,industry = ind)
#tabregrate(ndata)
#tabregvalue(data)
