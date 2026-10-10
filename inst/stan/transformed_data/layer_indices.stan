int n_unconstrained_offsets = (n_locs - 1) - n_parent_locs;
array[2, n_layers] int layer_bounds = bounds_to_range(layer_starts, n_locs);
array[2, n_parent_locs] int parent_child_bounds = bounds_to_range(parent_child_starts, n_locs);

// Direct mapping from each non-root offset index (1 .. n_locs - 1) to its layer index (1 .. n_layers - 1)
array[n_locs - 1] int<lower=1, upper=n_layers - 1> loc_layer_idx;
for (k in 1:(n_layers - 1)) {
  int st = layer_bounds[1, k + 1] - 1;
  int en = layer_bounds[2, k + 1] - 1;
  loc_layer_idx[st:en] = rep_array(k, en - st + 1);
}

// Precompute layer-wide mean population scales for sigma scaling across full layers
vector[n_locs - 1] loc_pop_scale;
for (k in 1:(n_layers - 1)) {
  int st = layer_bounds[1, k + 1];
  int en = layer_bounds[2, k + 1];
  vector[en - st + 1] layer_pop = loc_population[st:en];
  real mean_layer_pop = mean(layer_pop);
  for (i in st:en) {
    real pop_val = loc_population[i];
    loc_pop_scale[i - 1] = (pop_val > 0 && mean_layer_pop > 0) ? sqrt(mean_layer_pop / pop_val) : 1.0;
  }
}
