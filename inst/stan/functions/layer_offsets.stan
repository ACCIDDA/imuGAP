
// Accumulate multi-layer random walk offsets across hierarchical location tree
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

// Combine cohort baseline spline effect with location hierarchy effects
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

// Accumulate hierarchical link-scale effects (spline baseline, per-parent mu offsets, and layer offsets)
// Evaluates link(p_{l, t}) = link(p_{parent(l), t}) + mu_{parent(l), t} + delta_l sequentially top-down
matrix accumulate_hierarchical_raw_phi(
  vector raw_phi_root,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  vector loc_child_weight
) {
  matrix[n_cohort, n_locs] raw_phi_mat;
  raw_phi_mat[:, 1] = raw_phi_root;
  for (p in 1:n_parent_locs) {
    int pid = parent_loc_id[p];
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];
    int K = en - st + 1;
    vector[K] w = loc_child_weight[(st - 1):(en - 1)];
    vector[K] delta = off_layer[(st - 1):(en - 1)];

    // Compute weighted moments of child offsets under parent p
    vector[K] delta_sq = square(delta);
    real m2 = dot_product(w, delta_sq);
    real m3 = dot_product(w, delta_sq .* delta);
    real m4 = dot_product(w, square(delta_sq));
    vector[3] moments = [m2, m3, m4]';

    // Enclosing parent probability across cohorts
    vector[n_cohort] p_enclosing = inv_link(raw_phi_mat[:, pid]);
    vector[n_cohort] mu_p = evaluate_mu_from_moments(p_enclosing, moments);

    for (l in st:en) {
      raw_phi_mat[:, l] = raw_phi_mat[:, pid] + mu_p + off_layer[l - 1];
    }
  }
  return raw_phi_mat;
}

// Combine cohort baseline spline effect with location hierarchy effects and link-scale mu offsets
vector compute_hierarchical_phi(
  vector raw_phi_root,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  vector loc_child_weight
) {
  matrix[n_cohort, n_locs] raw_phi_mat = accumulate_hierarchical_raw_phi(
    raw_phi_root, off_layer, n_cohort, n_locs,
    n_parent_locs, parent_child_bounds, parent_loc_id, loc_child_weight
  );
  return to_vector(inv_link(raw_phi_mat));
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

// Compute link-scale offsets mu_offset per parent location and cohort from enclosing p and moments
matrix compute_mu_offsets(
  matrix raw_phi_mat,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  vector loc_child_weight
) {
  matrix[n_cohort, n_parent_locs] mu_offset;
  for (p in 1:n_parent_locs) {
    int pid = parent_loc_id[p];
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];
    int K = en - st + 1;
    vector[K] w = loc_child_weight[(st - 1):(en - 1)];
    vector[K] delta = off_layer[(st - 1):(en - 1)];

    vector[K] delta_sq = square(delta);
    real m2 = dot_product(w, delta_sq);
    real m3 = dot_product(w, delta_sq .* delta);
    real m4 = dot_product(w, square(delta_sq));
    vector[3] moments = [m2, m3, m4]';

    vector[n_cohort] p_enclosing = inv_link(raw_phi_mat[:, pid]);
    mu_offset[:, p] = evaluate_mu_from_moments(p_enclosing, moments);
  }
  return mu_offset;
}



