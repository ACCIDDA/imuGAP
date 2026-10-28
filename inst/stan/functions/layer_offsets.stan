
/**
 * @file layer_offsets.stan
 * @brief Orthonormal nullspace QR projection and hierarchical tree aggregation.
 */

/**
 * Accumulate multi-layer random walk offsets across hierarchical location tree (additive).
 *
 * @param n_locs Total number of locations.
 * @param n_parent_locs Number of parent nodes.
 * @param parent_child_bounds Contiguous [start, end] child bounds per parent.
 * @param parent_loc_id Location IDs of parent nodes.
 * @param off_layer Spatial offsets delta on link scale (length n_locs - 1).
 * @return Vector of cumulative location random walk offsets on link scale (length n_locs).
 */
vector accumulate_layer_offsets(
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  vector off_layer
) {
  vector[n_locs] raw_phi_loc;
  raw_phi_loc[1] = 0.0;
  for (p in 1:n_parent_locs) {
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];
    raw_phi_loc[st:en] = raw_phi_loc[parent_loc_id[p]] + off_layer[(st - 1):(en - 1)];
  }
  return raw_phi_loc;
}

/**
 * Combine cohort baseline spline effect with location hierarchy effects.
 *
 * @param raw_phi_root Baseline cohort coordinates on link scale (length n_cohort).
 * @param raw_phi_loc Cumulative location offsets on link scale (length n_locs).
 * @param n_cohort Number of cohorts.
 * @param n_locs Number of locations.
 * @return Vector of length (n_cohort * n_locs) containing unrolled inv_link probabilities.
 */
vector compute_hierarchical_phi(
  vector raw_phi_root,
  vector raw_phi_loc,
  int n_cohort,
  int n_locs
) {
  matrix[n_cohort, n_locs] raw_phi_mat =
    rep_matrix(raw_phi_root, n_locs) + rep_matrix(raw_phi_loc', n_cohort);
  return to_vector(inv_link(raw_phi_mat));
}



/**
 * Compute scaled spatial offsets from unconstrained Gaussian draws using block QR basis.
 *
 * Multiplies unconstrained z ~ N(0, 1) by orthonormal nullspace basis Q_2 to enforce
 * exact sum-to-zero constraints (sum_k w_k * delta_k = 0), and scales by inverse sqrt
 * population weight and layer standard deviation sigma_layer.
 *
 * @param n_locs Total locations.
 * @param n_parent_locs Number of parent nodes.
 * @param parent_child_bounds Contiguous [start, end] child index bounds for each parent.
 * @param z_bounds Index bounds into unconstrained parameter vector z_layer.
 * @param qr_bounds Index bounds into flattened qr_entries.
 * @param qr_entries Flattened Q_2 nullspace basis entries.
 * @param z_layer Unconstrained standard normal parameter draws.
 * @param loc_pop_scale Inverse sqrt relative population scale factor per location.
 * @param sigma_layer Standard deviation of random walk offsets per layer.
 * @param loc_layer_idx Lookup from location offset index to hierarchy layer.
 * @return Vector of length (n_locs - 1) containing scaled spatial offsets delta.
 */
vector compute_layer_offsets(
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[,] int z_bounds,
  array[,] int qr_bounds,
  vector qr_entries,
  vector z_layer,
  vector loc_pop_scale,
  vector sigma_layer,
  array[] int loc_layer_idx
) {
  vector[n_locs - 1] off_layer;
  for (p in 1:n_parent_locs) {
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];
    int K = en - st + 1;
    int z_st = z_bounds[1, p];
    int z_en = z_bounds[2, p];
    int q_st = qr_bounds[1, p];
    int q_en = qr_bounds[2, p];

    matrix[K, K - 1] Q_star = to_matrix(qr_entries[q_st:q_en], K, K - 1);
    vector[K] raw_off = Q_star * z_layer[z_st:z_en];
    int l_st = st - 1;
    int l_en = en - 1;
    off_layer[l_st:l_en] = (raw_off .* loc_pop_scale[l_st:l_en]) * sigma_layer[loc_layer_idx[l_st]];
  }
  return off_layer;
}
