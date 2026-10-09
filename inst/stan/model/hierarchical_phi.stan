vector[n_locs] raw_phi_loc = accumulate_layer_offsets(
  n_locs, n_parent_locs, parent_child_bounds, parent_loc_id, off_layer
);
matrix[n_cohort, n_locs] raw_phi_mat = accumulate_hierarchical_raw_phi(
  raw_phi_root, off_layer, n_cohort, n_locs,
  n_parent_locs, parent_child_bounds, parent_loc_id, loc_child_weight,
  guess_type, solver_type
);
vector[n_cohort * n_locs] phi = to_vector(inv_link(raw_phi_mat));

#include model/common_phi.stan
