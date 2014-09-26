source("readdata.R")
source("correlation.R")
source("linearregr.R")
source("readind.R")
source("tabreg.R")
source("neibors.R")

data = readdata("data")

ind <- readInd("filename.txt")
data <- list(rate=data$rate,value=data$value,date=data$date,filename=data$filename,industry = ind)
tabregrate(data)
tabregvalue(data)
neibors(data,k)

#cdata = correlation(data,4)
#cvolume = correlation(data$value,4)

