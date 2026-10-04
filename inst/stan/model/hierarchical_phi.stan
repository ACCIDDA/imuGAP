vector[n_locs - 1] off_layer = compute_layer_offsets(
  n_locs, n_parent_locs, parent_child_bounds, z_bounds, qr_bounds, qr_entries,
  z_layer, loc_pop_scale, sigma_layer, loc_layer_idx
);
vector[n_locs] raw_phi_loc = accumulate_layer_offsets(
  n_locs, n_parent_locs, parent_child_bounds, parent_loc_id, off_layer
);
vector[n_cohort * n_locs] phi = compute_hierarchical_phi(
  raw_phi_root, raw_phi_loc, n_cohort, n_locs
);

#include model/common_phi.stan

