skip_if_not_installed("rstan")
# ' "functions/layer_offsets.stan" defines
# ' `vector accumulate_layer_offsets(...)` and
# ' `vector compute_hierarchical_phi(...)` which propagate multi-layer spatial
# ' random walk offsets down hierarchical location trees and evaluate spatial
# ' cohort vaccination propensities.

target <- "functions/layer_offsets.stan"

skip_if_stan_unchanged(c("functions/link/logit.stan", target))

model_layer_offsets <- sprintf(
  "
functions {
  #include functions/link/logit.stan
  #include %s
}
data {
  int n_locs;
  int n_parent_locs;
  array[2, n_parent_locs] int parent_child_bounds;
  array[n_parent_locs] int parent_loc_id;
  vector[n_locs - 1] off_layer;

  int n_cohort;
  vector[n_cohort] raw_phi_root;

  int n_unconstrained;
  int n_qr_entries;
  int n_layers;
  array[2, n_parent_locs] int z_bounds;
  array[2, n_parent_locs] int qr_bounds;
  vector[n_qr_entries] qr_entries;
  vector[n_unconstrained] z_layer;
  vector[n_locs - 1] loc_pop_scale;
  vector[n_layers - 1] sigma_layer;
  array[n_locs - 1] int loc_layer_idx;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_locs] out_raw_phi_loc = accumulate_layer_offsets(
    n_locs, n_parent_locs, parent_child_bounds, parent_loc_id, off_layer
  );
  vector[n_cohort * n_locs] out_phi = compute_hierarchical_phi(
    raw_phi_root, out_raw_phi_loc, n_cohort, n_locs
  );
  vector[n_locs - 1] out_computed_offsets = compute_layer_offsets(
    n_locs, n_parent_locs, parent_child_bounds, z_bounds, qr_bounds, qr_entries,
    z_layer, loc_pop_scale, sigma_layer, loc_layer_idx
  );
}
",
  target
) |>
  compile_stan_harness()

test_that("accumulate_layer_offsets and compute_hierarchical_phi compute correctly", {
  data("locations_sim", package = "imuGAP")
  locs_sim <- imuGAP:::canonicalize_locations(locations_sim)
  ld_sim <- imuGAP:::assemble_layer_data(locs_sim)

  set.seed(42)
  off_layer <- rnorm(ld_sim$n_locs - 1L)
  raw_phi_root <- c(-0.5, 0.2, 1.0)

  # the hierarchical phi should be:
  #  - layer 1 should be expit(raw_phi_root) (i.e. inverse logit of cohort series)
  #  - a layer 2 location should be expit(raw_phi_root + delta)
  #  - a layer k location should be expit(raw_phi_root + sum(delta)), where
  #    sum(delta) is that location + all its ancestors' deltas

  parent_child_bounds <- matrix(
    as.integer(c(
      ld_sim$parent_child_starts,
      c(tail(ld_sim$parent_child_starts, -1) - 1L, ld_sim$n_locs)
    )),
    nrow = 2,
    byrow = TRUE
  )

  n_unconstrained <- (ld_sim$n_locs - 1L) - ld_sim$n_parent_locs
  z_layer <- rnorm(n_unconstrained)
  loc_pop_scale <- runif(ld_sim$n_locs - 1L, 0.5, 2.0)
  sigma_layer <- c(0.4, 0.8)
  loc_layer_idx <- rep(
    seq_len(ld_sim$n_layers - 1L),
    times = diff(c(ld_sim$layer_starts, ld_sim$n_locs + 1L))[-1L]
  )

  z_bounds <- ld_sim$z_bounds
  qr_bounds <- ld_sim$qr_bounds
  qr_entries <- ld_sim$qr_entries

  data_list <- c(
    ld_sim,
    list(
      parent_child_bounds = parent_child_bounds,
      off_layer = off_layer,
      n_cohort = length(raw_phi_root),
      raw_phi_root = raw_phi_root,
      n_unconstrained = n_unconstrained,
      z_layer = z_layer,
      loc_pop_scale = loc_pop_scale,
      sigma_layer = sigma_layer,
      loc_layer_idx = loc_layer_idx
    )
  )

  raw_phi_loc <- run_stan_harness(
    model_layer_offsets,
    data = data_list,
    out_raw_phi_loc
  )

  phi <- run_stan_harness(
    model_layer_offsets,
    data = data_list,
    out_phi
  )

  computed_offsets <- run_stan_harness(
    model_layer_offsets,
    data = data_list,
    out_computed_offsets
  )

  # Check raw_phi_loc manual accumulation
  expected_raw_phi_loc <- numeric(ld_sim$n_locs)
  expected_raw_phi_loc[1] <- 0.0
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- parent_child_bounds[1, p]
    en <- parent_child_bounds[2, p]
    expected_raw_phi_loc[
      st:en
    ] <- expected_raw_phi_loc[ld_sim$parent_loc_id[p]] +
      off_layer[(st - 1L):(en - 1L)]
  }

  expect_equal(raw_phi_loc, expected_raw_phi_loc, tolerance = 1e-6)

  # Check phi matrix expansion and flattening (column-major)
  expected_mat <- outer(raw_phi_root, expected_raw_phi_loc, `+`)
  expected_phi <- as.vector(stats::plogis(expected_mat))

  expect_equal(phi, expected_phi, tolerance = 1e-6)

  # Check compute_layer_offsets against original full-matrix formulation
  # using dense qr_basis multiplied by z_layer and scaling factors
  qr_basis_dense <- matrix(0, nrow = ld_sim$n_locs - 1L, ncol = n_unconstrained)
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- parent_child_bounds[1, p]
    en <- parent_child_bounds[2, p]
    k_len <- en - st + 1L
    z_st <- z_bounds[1, p]
    z_en <- z_bounds[2, p]
    q_st <- qr_bounds[1, p]
    q_en <- qr_bounds[2, p]
    q_star <- matrix(qr_entries[q_st:q_en], nrow = k_len, ncol = k_len - 1L)
    qr_basis_dense[(st - 1L):(en - 1L), z_st:z_en] <- q_star
  }

  expected_computed_offsets <- as.vector(
    (qr_basis_dense %*% z_layer) * loc_pop_scale * sigma_layer[loc_layer_idx]
  )

  expect_equal(
    as.numeric(computed_offsets),
    expected_computed_offsets,
    tolerance = 1e-6
  )

  # Check compute_layer_qr orthogonality and orthonormality
  w <- c(0.2, 0.3, 0.5)
  w_norm <- sqrt(w) / sqrt(sum(w))
  qr_res <- imuGAP:::compute_layer_qr(1L, 2L, 4L, c(1.0, w))
  q_star <- matrix(qr_res$qr_entries, nrow = 3L, ncol = 2L)
  expect_equal(as.numeric(t(q_star) %*% w_norm), c(0, 0), tolerance = 1e-6)
  expect_equal(t(q_star) %*% q_star, diag(2), tolerance = 1e-6)
})
