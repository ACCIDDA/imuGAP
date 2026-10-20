vector[n_locs - 1] off_layer = compute_layer_offsets(
  n_locs, n_parent_locs, parent_child_bounds, z_bounds, qr_bounds, qr_entries,
  z_layer, loc_pop_scale, sigma_layer, loc_layer_idx
);
matrix[n_cohort, n_locs] raw_phi_mat;
matrix[n_cohort, n_parent_locs] mu_offset;
{
  matrix[n_cohort, n_locs + n_parent_locs] raw_phi_and_mu = accumulate_hierarchical_raw_phi(
    raw_phi_root, off_layer, n_cohort, n_locs,
    n_parent_locs, parent_child_bounds, parent_loc_id, loc_child_weight, solver_type
  );
  raw_phi_mat = raw_phi_and_mu[:, :n_locs];
  mu_offset = raw_phi_and_mu[:, (n_locs + 1):];
}
