array[2, n_layers] int layer_bounds = bounds_to_range(layer_starts, n_locs);
array[2, n_parent_locs] int parent_child_bounds = bounds_to_range(parent_child_starts, n_locs);

// Direct mapping from each non-root offset index (1 .. n_locs - 1) to its layer index (1 .. n_layers - 1)
array[n_locs - 1] int<lower=1, upper=n_layers - 1> loc_layer_idx;
for (k in 1:(n_layers - 1)) {
  int st = layer_bounds[1, k + 1] - 1;
  int en = layer_bounds[2, k + 1] - 1;
  loc_layer_idx[st:en] = rep_array(k, en - st + 1);
}
