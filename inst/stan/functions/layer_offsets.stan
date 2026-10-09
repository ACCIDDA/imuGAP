/**
 * @file layer_offsets.stan
 * @brief Spatial random walk offset accumulation and hierarchical propensity evaluation.
 */

/**
 * Accumulate multi-layer random walk offsets across hierarchical location tree.
 *
 * Propagates offsets down the tree: raw_phi_loc[c] = raw_phi_loc[parent(c)] + off_layer[c].
 *
 * @param n_locs Total number of hierarchy locations (including root).
 * @param n_parent_locs Number of non-leaf parent locations.
 * @param parent_child_bounds 2D array [2, n_parent_locs] of child location index bounds [st, en].
 * @param parent_loc_id 1D array of parent location IDs.
 * @param off_layer Vector of length `n_locs - 1` containing non-root spatial offsets.
 * @return Vector of length `n_locs` containing accumulated spatial offsets (root = 0).
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
 * Broadcasts root baseline across locations and adds accumulated location offsets.
 *
 * @param raw_phi_root Vector of cohort baseline effects on link scale (length `n_cohort`).
 * @param raw_phi_loc Vector of accumulated location offsets (length `n_locs`).
 * @param n_cohort Total number of cohorts.
 * @param n_locs Total number of hierarchy locations.
 * @return Flattened column-major vector of length `n_cohort * n_locs` on probability scale.
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
 * Accumulate hierarchical link-scale effects (spline baseline, per-parent mu shifts, and offsets).
 *
 * Evaluates link(p_{l, t}) = link(p_{parent(l), t}) + mu_{parent(l), t} + delta_l sequentially top-down.
 *
 * @param raw_phi_root Vector of cohort baseline effects on link scale (length `n_cohort`).
 * @param off_layer Vector of length `n_locs - 1` containing balanced spatial offsets.
 * @param n_cohort Total number of cohorts.
 * @param n_locs Total number of hierarchy locations.
 * @param n_parent_locs Number of parent locations.
 * @param parent_child_bounds 2D array [2, n_parent_locs] of child location index bounds.
 * @param parent_loc_id 1D array of parent location IDs.
 * @param loc_child_weight Normalized population weights of children within parent blocks.
 * @param guess_type Integer code specifying initial guesser (1..6).
 * @param solver_type Integer code specifying rootfinder solver (1..5).
 * @return Matrix of dimension [n_cohort, n_locs] containing link-scale latent parameters.
 */
matrix accumulate_hierarchical_raw_phi(
  vector raw_phi_root,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  data vector loc_child_weight,
  data int guess_type,
  data int solver_type
) {
  matrix[n_cohort, n_locs] raw_phi_mat;
  raw_phi_mat[:, 1] = raw_phi_root;
  for (p in 1:n_parent_locs) {
    int pid = parent_loc_id[p];
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];

    vector[n_cohort] eta0 = raw_phi_mat[:, pid];
    vector[n_cohort] p0 = inv_link(eta0);
    vector[n_cohort] mu_init = guess_subpop_shift(
      eta0, p0,
      loc_child_weight[(st - 1):(en - 1)],
      off_layer[(st - 1):(en - 1)],
      guess_type
    );

    vector[n_cohort] mu_p = solve_subpop_shift(
      mu_init, eta0, p0,
      loc_child_weight[(st - 1):(en - 1)],
      off_layer[(st - 1):(en - 1)],
      solver_type
    );

    for (l in st:en) {
      raw_phi_mat[:, l] = raw_phi_mat[:, pid] + mu_p + off_layer[l - 1];
    }
  }
  return raw_phi_mat;
}

matrix accumulate_hierarchical_raw_phi(
  vector raw_phi_root,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  data vector loc_child_weight
) {
  return accumulate_hierarchical_raw_phi(
    raw_phi_root, off_layer, n_cohort, n_locs,
    n_parent_locs, parent_child_bounds, parent_loc_id, loc_child_weight,
    3, 1
  );
}

/**
 * Combine cohort baseline spline effect with location hierarchy effects and link-scale mu offsets.
 *
 * @param raw_phi_root Vector of cohort baseline effects on link scale.
 * @param off_layer Vector of balanced spatial offsets.
 * @param n_cohort Total number of cohorts.
 * @param n_locs Total number of hierarchy locations.
 * @param n_parent_locs Number of parent locations.
 * @param parent_child_bounds 2D array [2, n_parent_locs] of child location index bounds.
 * @param parent_loc_id 1D array of parent location IDs.
 * @param loc_child_weight Normalized population weights of children within parent blocks.
 * @param guess_type Integer code specifying initial guesser (1..6).
 * @param solver_type Integer code specifying rootfinder solver (1..5).
 * @return Flattened column-major vector of length `n_cohort * n_locs` on probability scale.
 */
vector compute_hierarchical_phi(
  vector raw_phi_root,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  data vector loc_child_weight,
  data int guess_type,
  data int solver_type
) {
  matrix[n_cohort, n_locs] raw_phi_mat = accumulate_hierarchical_raw_phi(
    raw_phi_root, off_layer, n_cohort, n_locs,
    n_parent_locs, parent_child_bounds, parent_loc_id, loc_child_weight,
    guess_type, solver_type
  );
  return to_vector(inv_link(raw_phi_mat));
}

vector compute_hierarchical_phi(
  vector raw_phi_root,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  data vector loc_child_weight
) {
  return compute_hierarchical_phi(
    raw_phi_root, off_layer, n_cohort, n_locs,
    n_parent_locs, parent_child_bounds, parent_loc_id, loc_child_weight,
    3, 1
  );
}

/**
 * Compute K x (K-1) orthonormal basis Q* orthogonal to weight vector w.
 *
 * @param w Vector of normalized child weights (length K).
 * @return Orthonormal basis matrix of dimension [K, K - 1].
 */
matrix get_weighted_qr_basis(data vector w) {
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

/**
 * Compute scaled layer offsets from unconstrained deviations using block QR basis.
 *
 * @param n_locs Total number of hierarchy locations.
 * @param n_parent_locs Number of parent locations.
 * @param parent_child_bounds 2D array [2, n_parent_locs] of child location index bounds.
 * @param z_bounds 2D array [2, n_parent_locs] of unconstrained offset parameter indices.
 * @param qr_bounds 2D array [2, n_parent_locs] of QR basis matrix indices.
 * @param qr_entries Precomputed data vector containing flattened block QR bases.
 * @param z_layer Parameter vector of unconstrained spatial innovations.
 * @param loc_pop_scale Precomputed data vector of population-variance scale factors.
 * @param sigma_layer Parameter vector of per-layer innovation standard deviations.
 * @param loc_layer_idx 1D array mapping offset index to layer index.
 * @return Vector of length `n_locs - 1` containing balanced, scaled spatial offsets.
 */
vector compute_layer_offsets(
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[,] int z_bounds,
  array[,] int qr_bounds,
  data vector qr_entries,
  vector z_layer,
  data vector loc_pop_scale,
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

/**
 * Compute link-scale offsets mu_offset per parent location and cohort from enclosing eta and shift.
 *
 * @param raw_phi_mat Matrix [n_cohort, n_locs] of accumulated link-scale values.
 * @param off_layer Vector of balanced spatial offsets.
 * @param n_cohort Total number of cohorts.
 * @param n_locs Total number of hierarchy locations.
 * @param n_parent_locs Number of parent locations.
 * @param parent_child_bounds 2D array [2, n_parent_locs] of child location index bounds.
 * @param parent_loc_id 1D array of parent location IDs.
 * @param loc_child_weight Normalized population weights of children within parent blocks.
 * @param guess_type Integer code specifying initial guesser (1..6).
 * @param solver_type Integer code specifying rootfinder solver (1..5).
 * @return Matrix of dimension [n_cohort, n_parent_locs] containing shift offsets mu.
 */
matrix compute_mu_offsets(
  matrix raw_phi_mat,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  data vector loc_child_weight,
  data int guess_type,
  data int solver_type
) {
  matrix[n_cohort, n_parent_locs] mu_offset;
  for (p in 1:n_parent_locs) {
    int pid = parent_loc_id[p];
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];

    vector[n_cohort] eta0 = raw_phi_mat[:, pid];
    vector[n_cohort] p0 = inv_link(eta0);
    vector[n_cohort] mu_init = guess_subpop_shift(
      eta0, p0,
      loc_child_weight[(st - 1):(en - 1)],
      off_layer[(st - 1):(en - 1)],
      guess_type
    );

    mu_offset[:, p] = solve_subpop_shift(
      mu_init, eta0, p0,
      loc_child_weight[(st - 1):(en - 1)],
      off_layer[(st - 1):(en - 1)],
      solver_type
    );
  }
  return mu_offset;
}

matrix compute_mu_offsets(
  matrix raw_phi_mat,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  data vector loc_child_weight
) {
  return compute_mu_offsets(
    raw_phi_mat, off_layer, n_cohort, n_locs,
    n_parent_locs, parent_child_bounds, parent_loc_id,
    loc_child_weight, 3, 1
  );
}
