int<lower=1> n_locs;
int<lower=1> n_layers;
array[n_layers] int<lower=1, upper=n_locs> layer_starts;
int<lower=0> n_parent_locs;
array[n_parent_locs] int<lower=1, upper=n_locs> parent_loc_id;
array[n_parent_locs] int<lower=1, upper=n_locs> parent_child_starts;
