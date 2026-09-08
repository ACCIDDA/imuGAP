
array[n_obs_right] int<lower=0> y_fail_right;

for (i in 1:n_obs_right) {
  y_fail_right[i] = y_smp_right[i] - y_obs_right[i];
}


