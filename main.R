source("readdata.R")
source("correlation.R")
source("regression.R")
source("readind.R")
source("tabreg.R")
source("neibors.R")
source("tablecontingency.R")

data = readdata("test_data")

#crate = correlation(data, "rate")
#cvolume = correlation(data, "volume")

#rrate = regression(data, "rate")
#rvolume = regression(data, "volume")

#ind <- readInd("filename.txt")
#for (i in 1:length(ind)){
#  data[[i]][["industry"]] <- ind[i]
#}
#data <- list(rate=data$rate,value=data$value,date=data$date,filename=data$filename,industry = ind)
#tabreg(data)
#tabregvalue(data)
#neibors(data,k)

#tablecontingency(data, names(data)[1], names(data)[2], 10)