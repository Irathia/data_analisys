source("readdata.R")
source("correlation.R")

data = readdata("test_data")

crate = correlation(data$rate,4)
cvolume = correlation(data$value,4)
