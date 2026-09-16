
// Accumulate multi-layer random walk offsets across hierarchical location tree
vector accumulate_layer_offsets(
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  vector off_layer
) {
  vector[n_locs] logit_phi_loc;
  logit_phi_loc[1] = 0.0;
  for (p in 1:n_parent_locs) {
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];
    logit_phi_loc[st:en] = logit_phi_loc[parent_loc_id[p]] + off_layer[(st - 1):(en - 1)];
  }
  return logit_phi_loc;
}

// Combine cohort baseline spline effect with location hierarchy effects
vector compute_hierarchical_phi(
  vector logit_phi_st,
  vector logit_phi_loc,
  int n_cohort,
  int n_locs
) {
  matrix[n_cohort, n_locs] logit_phi_mat =
    rep_matrix(logit_phi_st, n_locs) + rep_matrix(logit_phi_loc', n_cohort);
  return to_vector(inv_logit(logit_phi_mat));
}

// Compute K x (K-1) orthonormal basis Q* orthogonal to weight vector w
matrix get_weighted_qr_basis(vector w) {
  int K = num_elements(w);
  vector[K] v1 = w / sqrt(sum(square(w)));
  matrix[K, K] M;
  M[:, 1] = v1;
  for (j in 1:(K - 1)) {
    for (i in 1:K) {
      M[i, j + 1] = (i == j) ? 1.0 : 0.0;
    }
  }
  matrix[K, K] Q = qr_thin_Q(M);
  return Q[:, 2:K];
}

// Compute scaled layer offsets from unconstrained deviations using block QR basis
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
