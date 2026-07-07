
array[n_obs] int<lower=-1> y_obs_trans;

for(i in 1:n_obs) {
  y_obs_trans[i] = y_obs[i] - 1;
}

