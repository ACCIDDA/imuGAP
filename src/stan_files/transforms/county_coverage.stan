
  for(i in 1:n_cnty){
    lambda_c[i,] = diagonal((lambda[(cnty_start_index[i]):(cnty_end_index[i]),]')*sch_wts[(cnty_start_index[i]):(cnty_end_index[i])])';
  }
