# Unit tests for ensure_draws_parameters() helper in R/helpers.R

test_that("ensure_draws_parameters returns untouched draws if no parent locs or empty", {
  draws <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("a", "b")))
  expect_identical(ensure_draws_parameters(draws, list()), draws)
  expect_identical(
    ensure_draws_parameters(draws, list(n_parent_locs = 0L)),
    draws
  )
})

test_that("ensure_draws_parameters returns untouched draws if already present or missing", {
  draws_z <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(NULL, c("z_layer[1]", "sigma_layer[1]"))
  )
  dat <- list(n_parent_locs = 1L)
  expect_identical(ensure_draws_parameters(draws_z, dat), draws_z)

  draws_no_off <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(NULL, c("beta[1]", "beta[2]"))
  )
  expect_identical(ensure_draws_parameters(draws_no_off, dat), draws_no_off)
})

test_that("ensure_draws_parameters reconstructs z_layer from Stan transformed offsets", {
  skip_if_not_installed("rstan")

  target <- "functions/layer_offsets.stan"
  skip_if_stan_unchanged(c(
    "functions/bounds_to_range.stan",
    "functions/link/logit.stan",
    target,
    "data/locations.stan",
    "transformed_data/layer_indices.stan"
  ))

  model_reconstruct <- sprintf(
    "
functions {
  #include functions/bounds_to_range.stan
  #include functions/link/logit.stan
  #include %s
}
data {
  #include data/locations.stan
  int n_unconstrained;
  vector[n_unconstrained] z_layer;
  vector[n_layers - 1] sigma_layer;
}
transformed data {
  #include transformed_data/layer_indices.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[n_locs - 1] off_layer = compute_layer_offsets(
    n_locs, n_parent_locs, parent_child_bounds, z_bounds, qr_bounds, qr_entries,
    z_layer, loc_pop_scale, sigma_layer, loc_layer_idx
  );
}
",
    target
  ) |>
    compile_stan_harness()

  data("locations_sim")
  loc_info <- canonicalize_locations(locations_sim)
  ld <- imuGAP:::assemble_layer_data(loc_info)

  n_draws <- 5L
  n_unconstrained <- (ld$n_locs - 1L) - ld$n_parent_locs
  n_layers_eff <- ld$n_layers - 1L

  set.seed(42)
  z_mat <- matrix(rnorm(n_draws * n_unconstrained), nrow = n_draws)
  sigma_mat <- matrix(runif(n_draws * n_layers_eff, 0.5, 1.5), nrow = n_draws)

  off_mat <- matrix(0.0, nrow = n_draws, ncol = ld$n_locs - 1L)
  for (i in seq_len(n_draws)) {
    stan_data <- c(
      ld,
      list(
        n_unconstrained = n_unconstrained,
        z_layer = z_mat[i, ],
        sigma_layer = sigma_mat[i, ]
      )
    )
    off_mat[i, ] <- run_stan_harness(model_reconstruct, stan_data, "off_layer")
  }

  colnames(off_mat) <- paste0("off_layer[", seq_len(ld$n_locs - 1L), "]")
  colnames(sigma_mat) <- paste0("sigma_layer[", seq_len(n_layers_eff), "]")
  draws_mat <- cbind(off_mat, sigma_mat)

  res <- imuGAP:::ensure_draws_parameters(draws_mat, ld)

  z_colnames <- paste0("z_layer[", seq_len(n_unconstrained), "]")
  expect_true(all(z_colnames %in% colnames(res)))

  # Verify exact recovery of input z_layer from Stan-generated off_layer
  z_recovered <- res[, z_colnames, drop = FALSE]
  expect_equal(unname(z_recovered), z_mat, tolerance = 1e-10)
})
