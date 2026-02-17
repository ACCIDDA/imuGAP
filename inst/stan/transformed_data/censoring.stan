
int<lower=-1> y_obs_trans[n_obs];

for(i in 1:n_obs) {
  y_obs_trans[i] = y_obs[i] - 1;
}

