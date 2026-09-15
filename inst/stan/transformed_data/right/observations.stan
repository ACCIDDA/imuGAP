array[n_obs_unmixed_right] int<lower=0> y_fail_unmixed_right;
for (i in 1:n_obs_unmixed_right) {
  y_fail_unmixed_right[i] = y_smp_unmixed_right[i] - y_obs_unmixed_right[i];
}

array[n_obs_mixed_right] int<lower=0> y_fail_mixed_right;
for (i in 1:n_obs_mixed_right) {
  y_fail_mixed_right[i] = y_smp_mixed_right[i] - y_obs_mixed_right[i];
}
