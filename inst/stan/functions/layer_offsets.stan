
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
  matrix[n_cohort, n_locs] logit_phi_mat = rep_matrix(logit_phi_st, n_locs) + rep_matrix(to_row_vector(logit_phi_loc), n_cohort);
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

// Compute scaled layer offsets from unconstrained standard normal deviations
vector compute_layer_offsets(
  matrix qr_basis,
  vector z_layer,
  vector loc_pop_scale,
  vector sigma_layer,
  array[] int loc_layer_idx
) {
  return ((qr_basis * z_layer) .* loc_pop_scale) .* sigma_layer[loc_layer_idx];
}
