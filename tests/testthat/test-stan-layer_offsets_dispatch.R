skip_if_not_installed("rstan")

target <- "functions/layer_offsets_dispatch.stan"

skip_if_stan_unchanged(c(
  "functions/link/logit.stan",
  "functions/guess/dispatch_logit.stan",
  "functions/solvers/dispatch.stan",
  "functions/layer_offsets.stan",
  target
))

model_layer_offsets_dispatch <- sprintf(
  "
functions {
  #include functions/link/logit.stan
  #include functions/guess/dispatch_logit.stan
  #include functions/solvers/dispatch.stan
  #include functions/layer_offsets.stan
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
  vector[n_locs - 1] loc_child_weight;
  int guess_type;
  int solver_type;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  matrix[n_cohort, n_locs + n_parent_locs] out_raw_phi_and_mu = accumulate_hierarchical_raw_phi(
    raw_phi_root, off_layer, n_cohort, n_locs,
    n_parent_locs, parent_child_bounds, parent_loc_id, loc_child_weight,
    guess_type, solver_type
  );
  matrix[n_cohort, n_locs] out_raw_phi_mat = out_raw_phi_and_mu[:, :n_locs];
  matrix[n_cohort, n_parent_locs] out_mu_offset = out_raw_phi_and_mu[:, (n_locs + 1):];
}
",
  target
) |>
  compile_stan_harness()

test_that("accumulate_hierarchical_raw_phi with solver_type dispatches correctly", {
  n_cohort <- 3L
  raw_phi_root <- c(0.1, -0.2, 0.4)
  n_locs <- 4L
  n_parent_locs <- 1L
  parent_child_bounds <- matrix(c(2L, 4L), nrow = 2L, ncol = 1L)
  parent_loc_id <- as.array(1L)
  off_layer <- c(-0.1, 0.05, 0.05)
  loc_child_weight <- c(0.4, 0.3, 0.3)

  data_list <- list(
    n_locs = n_locs,
    n_parent_locs = n_parent_locs,
    parent_child_bounds = parent_child_bounds,
    parent_loc_id = parent_loc_id,
    off_layer = off_layer,
    n_cohort = n_cohort,
    raw_phi_root = raw_phi_root,
    loc_child_weight = loc_child_weight,
    guess_type = 1L,
    solver_type = 1L
  )

  res_mat <- run_stan_harness(
    model_layer_offsets_dispatch,
    data = data_list,
    out_raw_phi_mat
  )
  res_mu <- run_stan_harness(
    model_layer_offsets_dispatch,
    data = data_list,
    out_mu_offset
  )

  expect_equal(dim(res_mat), c(n_cohort, n_locs))
  expect_equal(dim(res_mu), c(n_cohort, n_parent_locs))
  # With zero guess and direct solver, mu_offset should be 0
  expect_equal(unname(res_mu[, 1L]), rep(0.0, n_cohort), tolerance = 1e-9)
  # Root location equals raw_phi_root
  expect_equal(unname(res_mat[, 1L]), raw_phi_root, tolerance = 1e-9)

  # With taylor2 guess and direct solver, mu_offset is non-zero and finite
  data_t2 <- data_list
  data_t2$guess_type <- 2L
  res_mu_t2 <- run_stan_harness(
    model_layer_offsets_dispatch,
    data = data_t2,
    out_mu_offset
  )
  expect_true(all(is.finite(res_mu_t2)))
})
