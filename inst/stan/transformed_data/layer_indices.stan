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

// Precomputed normalized population weights for children within each parent block
vector[n_locs - 1] loc_child_weight;

// Lookup from location ID to its index in parent_loc_id (1 .. n_parent_locs, or 0 if not a parent)
array[n_locs] int<lower=0, upper=n_parent_locs> loc_parent_idx = rep_array(0, n_locs);
for (p in 1:n_parent_locs) {
  loc_parent_idx[parent_loc_id[p]] = p;
}

// Lookup from child location ID to its parent's index in 1 .. n_parent_locs (0 for root)
array[n_locs] int<lower=0, upper=n_parent_locs> loc_to_parent_p_idx = rep_array(0, n_locs);
for (p in 1:n_parent_locs) {
  int st = parent_child_bounds[1, p];
  int en = parent_child_bounds[2, p];
  loc_to_parent_p_idx[st:en] = rep_array(p, en - st + 1);
}

// Precomputed per-parent block QR basis for weighted balanced offsets
int n_unconstrained_offsets = (n_locs - 1) - n_parent_locs;

int n_qr_entries = 0;
for (p in 1:n_parent_locs) {
  int K = parent_child_bounds[2, p] - parent_child_bounds[1, p] + 1;
  n_qr_entries += K * (K - 1);
}

array[2, n_parent_locs] int z_bounds;
array[2, n_parent_locs] int qr_bounds;
vector[n_qr_entries] qr_entries;

int cur_z = 1;
int cur_qr = 1;
for (p in 1:n_parent_locs) {
  int st = parent_child_bounds[1, p];
  int en = parent_child_bounds[2, p];
  int K = en - st + 1;

  z_bounds[1, p] = cur_z;
  z_bounds[2, p] = cur_z + K - 2;

  qr_bounds[1, p] = cur_qr;
  qr_bounds[2, p] = cur_qr + K * (K - 1) - 1;

  vector[K] pop_slice = loc_population[st:en];
  real sum_pop = sum(pop_slice);
  vector[K] w;
  if (sum_pop > 0) {
    w = pop_slice / sum_pop;
  } else {
    w = rep_vector(1.0 / K, K);
  }
  loc_child_weight[(st - 1):(en - 1)] = w;
  vector[K] w_prime = sqrt(w);
  matrix[K, K - 1] Q_star = get_weighted_qr_basis(w_prime);
  qr_entries[cur_qr:(cur_qr + K * (K - 1) - 1)] = to_vector(Q_star);

  cur_z += (K - 1);
  cur_qr += K * (K - 1);
}


