
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

// Accumulate hierarchical link-scale effects (spline baseline, per-parent mu shifts, and offsets)
// Evaluates link(p_{l, t}) = link(p_{parent(l), t}) + mu_{parent(l), t} + delta_l sequentially top-down
// Returns [n_cohort, n_locs + n_parent_locs] matrix with raw_phi_mat in cols 1..n_locs and
// mu_offset in cols (n_locs + 1)..(n_locs + n_parent_locs), avoiding duplicate solver evaluations
matrix accumulate_hierarchical_raw_phi(
  vector raw_phi_root,
  vector off_layer,
  int n_cohort,
  int n_locs,
  int n_parent_locs,
  array[,] int parent_child_bounds,
  array[] int parent_loc_id,
  data vector loc_child_weight,
  int solver_type
) {
  matrix[n_cohort, n_locs + n_parent_locs] out_mat;
  out_mat[:, 1] = raw_phi_root;
  for (p in 1:n_parent_locs) {
    int pid = parent_loc_id[p];
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];

    vector[n_cohort] eta0 = out_mat[:, pid];
    vector[n_cohort] p0 = inv_link(eta0);
    out_mat[:, n_locs + p] = solve_subpop_shift(
      eta0, p0,
      loc_child_weight[(st - 1):(en - 1)],
      off_layer[(st - 1):(en - 1)],
      solver_type
    );

    vector[n_cohort] parent_shifted = out_mat[:, pid] + out_mat[:, n_locs + p];
    for (l in st:en) {
      out_mat[:, l] = parent_shifted + off_layer[l - 1];
    }

  }
  return out_mat;
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
  matrix[n_cohort, n_locs + n_parent_locs] out_mat;
  out_mat[:, 1] = raw_phi_root;
  for (p in 1:n_parent_locs) {
    int pid = parent_loc_id[p];
    int st = parent_child_bounds[1, p];
    int en = parent_child_bounds[2, p];

    vector[n_cohort] eta0 = out_mat[:, pid];
    vector[n_cohort] p0 = inv_link(eta0);
    out_mat[:, n_locs + p] = solve_subpop_shift(
      eta0, p0,
      loc_child_weight[(st - 1):(en - 1)],
      off_layer[(st - 1):(en - 1)]
    );

    vector[n_cohort] parent_shifted = out_mat[:, pid] + out_mat[:, n_locs + p];
    for (l in st:en) {
      out_mat[:, l] = parent_shifted + off_layer[l - 1];
    }
  }
  return out_mat;
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
