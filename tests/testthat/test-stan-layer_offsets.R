skip_if_not_installed("rstan")
# ' "functions/layer_offsets.stan" defines
# ' `vector accumulate_layer_offsets(...)` and
# ' `vector compute_hierarchical_phi(...)` which propagate multi-layer spatial
# ' random walk offsets down hierarchical location trees and evaluate spatial
# ' cohort vaccination propensities.

target <- "functions/layer_offsets.stan"

skip_if_stan_unchanged(target)

model_layer_offsets <- sprintf(
  "
functions {
  #include %s
}
data {
  int n_locs;
  int n_parent_locs;
  array[2, n_parent_locs] int parent_child_bounds;
  array[n_parent_locs] int parent_loc_id;
  vector[n_locs - 1] off_layer;

  int n_cohort;
  vector[n_cohort] logit_phi_st;

  int n_unconstrained;
  int n_layers;
  matrix[n_locs - 1, n_unconstrained] qr_basis;
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
  vector[n_locs] out_logit_phi_loc = accumulate_layer_offsets(
    n_locs, n_parent_locs, parent_child_bounds, parent_loc_id, off_layer
  );
  vector[n_cohort * n_locs] out_phi = compute_hierarchical_phi(
    logit_phi_st, out_logit_phi_loc, n_cohort, n_locs
  );
  matrix[3, 2] out_qr_test = get_weighted_qr_basis([0.2, 0.3, 0.5]');
  vector[n_locs - 1] out_computed_offsets = compute_layer_offsets(
    qr_basis, z_layer, loc_pop_scale, sigma_layer, loc_layer_idx
  );
}
",
  target
) |>
  compile_stan_harness()

test_that("accumulate_layer_offsets and compute_hierarchical_phi compute correctly", {
  data("locations_sim", package = "imuGAP")
  locs_sim <- canonicalize_locations(locations_sim)
  ld_sim <- assemble_layer_data(locs_sim)

  set.seed(42)
  off_layer <- rnorm(ld_sim$n_locs - 1L)
  logit_phi_st <- c(-0.5, 0.2, 1.0)

  # the hierarchical phi should be:
  #  - layer 1 should be expit(logit_phi_st) (i.e. inverse logit of cohort series)
  #  - a layer 2 location should be expit(logit_phi_st + delta)
  #  - a layer k location should be expit(logit_phi_st + sum(delta)), where
  #    sum(delta) is that location + all its ancestors' deltas

  parent_child_bounds <- matrix(
    as.integer(c(
      ld_sim$parent_child_starts,
      c(tail(ld_sim$parent_child_starts, -1) - 1L, ld_sim$n_locs)
    )),
    nrow = 2,
    byrow = TRUE
  )

  n_unconstrained <- 2L
  qr_basis <- matrix(
    rnorm((ld_sim$n_locs - 1L) * n_unconstrained),
    nrow = ld_sim$n_locs - 1L,
    ncol = n_unconstrained
  )
  z_layer <- rnorm(n_unconstrained)
  loc_pop_scale <- runif(ld_sim$n_locs - 1L, 0.5, 2.0)
  sigma_layer <- c(0.4, 0.8)
  loc_layer_idx <- as.integer(rep(1:2, length.out = ld_sim$n_locs - 1L))

  data_list <- c(
    ld_sim,
    list(
      parent_child_bounds = parent_child_bounds,
      off_layer = off_layer,
      n_cohort = length(logit_phi_st),
      logit_phi_st = logit_phi_st,
      n_unconstrained = n_unconstrained,
      qr_basis = qr_basis,
      z_layer = z_layer,
      loc_pop_scale = loc_pop_scale,
      sigma_layer = sigma_layer,
      loc_layer_idx = loc_layer_idx
    )
  )

  logit_phi_loc <- run_stan_harness(
    model_layer_offsets,
    data = data_list,
    out_logit_phi_loc
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

  # Check logit_phi_loc manual accumulation
  expected_logit_phi_loc <- numeric(ld_sim$n_locs)
  expected_logit_phi_loc[1] <- 0.0
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- parent_child_bounds[1, p]
    en <- parent_child_bounds[2, p]
    expected_logit_phi_loc[
      st:en
    ] <- expected_logit_phi_loc[ld_sim$parent_loc_id[p]] +
      off_layer[(st - 1L):(en - 1L)]
  }

  expect_equal(logit_phi_loc, expected_logit_phi_loc, tolerance = 1e-6)

  # Check phi matrix expansion and flattening (column-major)
  expected_mat <- outer(logit_phi_st, expected_logit_phi_loc, `+`)
  expected_phi <- as.vector(stats::plogis(expected_mat))

  expect_equal(phi, expected_phi, tolerance = 1e-6)

  # Check compute_layer_offsets scaling
  expected_computed_offsets <- as.vector(
    ((qr_basis %*% z_layer) * loc_pop_scale) * sigma_layer[loc_layer_idx]
  )
  expect_equal(
    as.numeric(computed_offsets),
    expected_computed_offsets,
    tolerance = 1e-6
  )

  # Check get_weighted_qr_basis orthogonality and orthonormality
  qr_test <- run_stan_harness(
    model_layer_offsets,
    data = data_list,
    out_qr_test
  )
  w <- c(0.2, 0.3, 0.5)
  w_norm <- w / sqrt(sum(w^2))
  expect_equal(as.numeric(t(qr_test) %*% w_norm), c(0, 0), tolerance = 1e-6)
  expect_equal(t(qr_test) %*% qr_test, diag(2), tolerance = 1e-6)
})
