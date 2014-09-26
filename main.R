source("readdata.R")
source("correlation.R")
source("linearregr.R")

data = readdata("data")

industry = readLines("filename.txt")
industry = strsplit(industry,split = ";")

cdata = correlation(data,4)
#cvolume = correlation(data$value,4)

