source("readdata.R")

tablecontingency <- function(data, value, i_comp_name, j_comp_name, n_size){
  vm = c()
  
  for (i in 1:length(data)) {
    svalue = sort(data[[i]][[value]]);
    pr = trunc(length(svalue)/100);
    if (!pr) {
      pr = 1
    }
    new_lenght = length(svalue) - 2*pr;
    svalue = get_range(svalue, pr)
    vm <- c(svalue, c(vm))
  }
  
  m <- matrix(data = vm, nrow = length(vm)/length(data), ncol = length(data))
  colnames(m) <- c(names(data))
  print(m[,i_comp_name])
  
  output = "graphics";
  if (!file.exists(output)) {
    dir.create(output)
  }
  print(j_comp_name)
  print(i_comp_name)
  boxplot(m[,1], m[,2])
}

#tablecontingency(data, "rate", names(data)[1], names(data)[3], 10)