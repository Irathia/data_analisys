hi_pow_2 <- function(N,col_number) {
  number_n = 0;
  summ_i <- rep(0, col_number);
  summ_j <- rep(0, length(N)/col_number);
  
  for (i in 1:col_number) {
    for (j in 1:(length(N)/col_number)) {
      number_n <- number_n + N[i,j];
      summ_j[j] = summ_j[j] +  N[i,j];
      summ_i[i] = summ_i[i] +  N[i,j];
    }
  }
  print(summ_j)
  print(summ_i)
  
  p_N <- matrix(data = 0, nrow = length(N)/col_number, ncol = col_number)
  for (i in 1:col_number) {
    for (j in 1:(length(N)/col_number)) {
      p_N[i,j] = N[i,j]/number_n;
    }
  }
  print(p_N)
  
  hi = 0;
  for (i in 1:col_number) {
    for (j in 1:(length(N)/col_number)) {
      if (p_N[i,j] != 0) {
        hi = hi + (((((summ_i[i]/number_n)*(summ_j[j]/number_n)) - p_N[i,j])^2) / p_N[i,j]);
      }
    }
  }
  return(hi)
}

hi = hi_pow_2(N, 10)
