/**
 * @file layer_offsets_dispatch.stan
 * @brief Dynamic rootfinding dispatch tree aggregation for hierarchical offsets.
 */

/**
 * Sequentially accumulate hierarchical link-scale effects down the tree with rootfinding shifts.
 *
 * Top-down evaluation: link(p_{l, t}) = link(p_{parent(l), t}) + mu_{parent(l), t} + delta_l.
 * Solves the nonlinear aggregation equation for mu_{parent, t} using solver_type to guarantee
 * that parent aggregate coverage equals the population-weighted sum of child probabilities.
 *
 * @param raw_phi_root Baseline cohort coordinates on link scale (length n_cohort).
 * @param off_layer Spatial offsets delta on link scale (length n_locs - 1).
 * @param n_cohort Number of cohorts.
 * @param n_locs Total location entities.
 * @param n_parent_locs Number of parent nodes.
 * @param parent_child_bounds Contiguous child index bounds per parent.
 * @param parent_loc_id Location IDs of parent nodes.
 * @param loc_child_weight Normalized child weights summing to 1 within each parent.
 * @param solver_type Integer code specifying rootfinder algorithm (1..5).
 * @return Packed matrix of shape [n_cohort, n_locs + n_parent_locs], where cols :n_locs
 *   contain eta coordinates and cols (n_locs + 1): contain solved mu shifts.
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
