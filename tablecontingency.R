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
  
  min_x = min(m[,i_comp_name]);
  min_y = min(m[,j_comp_name]);
  max_x = max(m[,i_comp_name]);
  max_y = max(m[,j_comp_name]);
  
  N = matrix(data = 0, nrow = n_size, ncol = n_size);
  step_x = (max_x - min_x) / n_size;
  step_y = (max_y - min_y) / n_size;
  for (i in 1:n_size) {
    for (j in 1:n_size) {
      for (k in 1:(length(m)/length(data))) {
        if (m[k,i_comp_name] > min_x + step_x*(i-1)) {
          if (m[k,i_comp_name] < min_x + step_x*i) {
            if (m[k,j_comp_name] > min_y + step_y*(j-1)) {
              if (m[k,j_comp_name] < min_y + step_y*j) {
                N[(n_size - i + 1), (n_size - j +1)] = N[(n_size - i + 1), (n_size - j + 1)] + 1;
              }
            }
          }
        }
      }
    }
  }
  return(N)
}

N = tablecontingency(data, "rate", names(data)[1], names(data)[3], 10)