// Offsets - layer standard deviations
sigma_layer ~ cauchy(0, 1);

// Model-only local variable for unconstrained free parameter draws
row_vector[(n_locs - 1) - n_parent_locs] free_off_layer;

// Draw unconstrained free parameters for each layer slice using constrained layer_draw_map
for (k in 1:(n_layers - 1)) {
  free_off_layer[layer_draw_map[1, k]:layer_draw_map[2, k]] ~ normal(0, sigma_layer[k]);
}

// Array of unconstrained parameter vectors per parent location group
array[n_parent_locs] vector theta_parent;

for (p in 1:n_parent_locs) {
  int free_start = parent_free_draw_map[1, p];
  int free_end   = parent_free_draw_map[2, p];
  if (free_end >= free_start) {
    theta_parent[p] = free_off_layer[free_start:free_end]';
  } else {
    theta_parent[p] = rep_vector(0.0, 0);
  }
}

// Execute zero-sum transformations in parallel across CPU threads via map_rect
vector[n_locs - 1] mapped_offsets = map_rect(
  apply_zero_sum_transform,
  phi_placeholder,
  theta_parent,
  real_data_parent,
  int_data_parent
);

off_layer = mapped_offsets';

