
rate_price <- function(price) {
	rate <- vector(mode = "numeric", length = length(price)-1)
	for (i in 2:length(price)) {
		rate[i-1] <- log((price[i])/price[i-1])
	}
	
	return(rate)
}

get_range <- function(arr, range_num) {
	range_arr <- vector(mode = "numeric", length = length(arr)-range_num*2)
	for (i in range_num:(length(arr) - range_num)) {
		range_arr[i-range_num] <- arr[i]
	}
	
	return(range_arr)
}

readdata <- function(path, mask="*.csv") {
	if (!file.exists(path)) {
		cat("Path: ", path, " is not exist\n")
		return()
	}
	
	output = paste(path,"..", "graphics", sep="/")
	
	if (!file.exists(output)) {
		dir.create(output)
	}
	
	library(ggplot2)
	
	j = 1
	d <- c()
	v <- c()
	files_list = list.files(path=path, pattern=mask)
	for (i in 1:length(files_list)) {
		data = read.csv(file=paste(path,files_list[i], sep="/"))
		date = data$Date
		if (length(date) == 770) {
			rate = rate_price(data$Close)
			ggsave(qplot(rate, geom="histogram"), file=paste(output,paste(files_list[i],"hist.png",sep="_"), sep="/"))
			
			srate = sort(rate);
			pr = trunc(length(srate)/100);
			print(pr)
			
			if (!pr) {
				pr = 1
			}
			
			srate = get_range(srate, pr)
			ggsave(qplot(srate, geom="histogram"),file=paste(output,paste(files_list[i],"hist_without.png",sep="_"), sep="/"))
			ggsave(qplot(x=date, y=data$Close, geom="auto"),file=paste(output,paste(files_list[i],"price.png",sep="_"), sep="/"))
			ggsave(qplot(date, data$Volume, geom="auto"),file=paste(output,paste(files_list[i],"volume.png",sep="_"), sep="/"))
			ggsave(qplot(get_range(data$Volume, pr), geom="histogram"),file=paste(output,paste(files_list[i],"hist_volume_without.png",sep="_"), sep="/"))
			
			
			d <- c(d,c(rate))
			v <- c(v,c(data$Volume))
			j = j + 1;
		}
	}
	d = matrix(d, nrow=769, ncol=j-1)
	v = matrix(v, nrow=770, ncol=j-1)
	
	return(list(rate=d,value=v))
}

data = readdata("data")

