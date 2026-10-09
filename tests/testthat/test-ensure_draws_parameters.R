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

test_that("ensure_draws_parameters reconstructs z_layer from off_layer draws accurately", {
  data("locations_sim")
  loc_info <- canonicalize_locations(locations_sim)
  ld <- imuGAP:::assemble_layer_data(loc_info)

  # Construct simulated draws matrix with off_layer and sigma_layer
  n_draws <- 5L
  n_off <- ld$n_locs - 1L
  n_layers_eff <- ld$n_layers - 1L

  set.seed(42)
  sigma_vals <- matrix(runif(n_draws * n_layers_eff, 0.5, 1.5), nrow = n_draws)
  colnames(sigma_vals) <- paste0("sigma_layer[", seq_len(n_layers_eff), "]")

  off_vals <- matrix(rnorm(n_draws * n_off, 0, 0.2), nrow = n_draws)
  colnames(off_vals) <- paste0("off_layer[", seq_len(n_off), "]")

  draws_mat <- cbind(off_vals, sigma_vals)
  res <- imuGAP:::ensure_draws_parameters(draws_mat, ld)

  n_unconstrained <- (ld$n_locs - 1L) - ld$n_parent_locs
  expected_z_names <- paste0("z_layer[", seq_len(n_unconstrained), "]")

  expect_true(all(expected_z_names %in% colnames(res)))
  expect_equal(nrow(res), n_draws)
  expect_equal(ncol(res), ncol(draws_mat) + n_unconstrained)
  expect_false(anyNA(res[, expected_z_names]))
})
