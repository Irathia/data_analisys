source("readdata.R")
source("correlation.R")

#data = readdata("test_data")

crate = correlation(data$rate)
cvolume = correlation(data$volume)


rrate = regression(data$rate)
rvolume = regression(data$volume)