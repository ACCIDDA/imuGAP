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

  data_list <- c(
    ld_sim,
    list(
      parent_child_bounds = parent_child_bounds,
      off_layer = off_layer,
      n_cohort = length(logit_phi_st),
      logit_phi_st = logit_phi_st
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
})
