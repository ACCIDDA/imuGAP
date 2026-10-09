vector[n_locs] raw_phi_loc = accumulate_layer_offsets(
  n_locs, n_parent_locs, parent_child_bounds, parent_loc_id, off_layer
);
vector[n_cohort * n_locs] phi = compute_hierarchical_phi(
  raw_phi_root, raw_phi_loc, n_cohort, n_locs
);

#include model/common_phi.stan

