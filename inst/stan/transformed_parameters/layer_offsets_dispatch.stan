/**
 * @file layer_offsets_dispatch.stan
 * @brief Dynamic solver projection and scope-isolated coordinate partitioning.
 */

// Compute scaled spatial offsets enforcing exact weighted sum-to-zero constraints
vector[n_locs - 1] off_layer = compute_layer_offsets(
  n_locs, n_parent_locs, parent_child_bounds, z_bounds, qr_bounds, qr_entries,
  z_layer, loc_pop_scale, sigma_layer, loc_layer_idx
);

// Location coordinates on link scale: eta_{c, l}
matrix[n_cohort, n_locs] raw_phi_mat;

// Solved parent aggregation shifts: mu_{c, p}
matrix[n_cohort, n_parent_locs] mu_offset;

// Local evaluation block: discards packed intermediate matrix to avoid sampler memory bloat
{
  matrix[n_cohort, n_locs + n_parent_locs] raw_phi_and_mu = accumulate_hierarchical_raw_phi(
    raw_phi_root, off_layer, n_cohort, n_locs,
    n_parent_locs, parent_child_bounds, parent_loc_id, loc_child_weight, solver_type
  );
  raw_phi_mat = raw_phi_and_mu[:, :n_locs];
  mu_offset = raw_phi_and_mu[:, (n_locs + 1):];
}
