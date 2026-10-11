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

test_that("ensure_draws_parameters reconstructs z_layer from synthetic off_mat in pure R", {
  data("locations_sim", package = "imuGAP")
  loc_info <- canonicalize_locations(locations_sim)
  ld <- assemble_layer_data(loc_info)

  n_draws <- 5L
  n_unconstrained <- (ld$n_locs - 1L) - ld$n_parent_locs
  n_layers_eff <- ld$n_layers - 1L

  set.seed(42)
  z_mat <- matrix(rnorm(n_draws * n_unconstrained), nrow = n_draws)
  sigma_mat <- matrix(runif(n_draws * n_layers_eff, 0.5, 1.5), nrow = n_draws)

  layer_starts <- as.integer(ld$layer_starts)
  layer_bounds <- rbind(layer_starts, c(layer_starts[-1L] - 1L, ld$n_locs))
  parent_child_starts <- as.integer(ld$parent_child_starts)
  parent_child_bounds <- rbind(
    parent_child_starts,
    c(parent_child_starts[-1L] - 1L, ld$n_locs)
  )

  loc_layer_idx <- integer(ld$n_locs - 1L)
  for (k in seq_len(n_layers_eff)) {
    st <- layer_bounds[1L, k + 1L] - 1L
    en <- layer_bounds[2L, k + 1L] - 1L
    loc_layer_idx[st:en] <- k
  }

  loc_pop_scale <- numeric(ld$n_locs - 1L)
  for (k in seq_len(n_layers_eff)) {
    st <- layer_bounds[1L, k + 1L]
    en <- layer_bounds[2L, k + 1L]
    layer_pop <- ld$loc_population[st:en]
    mean_layer_pop <- mean(layer_pop)
    for (i in st:en) {
      pop_val <- ld$loc_population[i]
      loc_pop_scale[i - 1L] <- if (pop_val > 0 && mean_layer_pop > 0) {
        sqrt(mean_layer_pop / pop_val)
      } else {
        1.0
      }
    }
  }

  off_mat <- matrix(0.0, nrow = n_draws, ncol = ld$n_locs - 1L)
  for (p in seq_len(ld$n_parent_locs)) {
    st <- parent_child_bounds[1L, p]
    en <- parent_child_bounds[2L, p]
    k_size <- en - st + 1L

    z_st <- ld$z_bounds[1L, p]
    z_en <- ld$z_bounds[2L, p]
    q_st <- ld$qr_bounds[1L, p]
    q_en <- ld$qr_bounds[2L, p]
    q_star <- matrix(
      ld$qr_entries[q_st:q_en],
      nrow = k_size,
      ncol = k_size - 1L
    )

    l_st <- st - 1L
    l_en <- en - 1L
    layer_idx <- loc_layer_idx[l_st]
    pop_scale <- loc_pop_scale[l_st:l_en]

    z_sub <- z_mat[, z_st:z_en, drop = FALSE]
    for (d in seq_len(n_draws)) {
      off_slice <- as.vector(q_star %*% z_sub[d, ]) *
        pop_scale *
        sigma_mat[d, layer_idx]
      off_mat[d, l_st:l_en] <- off_slice
    }
  }

  colnames(off_mat) <- paste0("off_layer[", seq_len(ld$n_locs - 1L), "]")
  colnames(sigma_mat) <- paste0("sigma_layer[", seq_len(n_layers_eff), "]")
  draws_mat <- cbind(off_mat, sigma_mat)

  res <- ensure_draws_parameters(draws_mat, ld)

  z_colnames <- paste0("z_layer[", seq_len(n_unconstrained), "]")
  expect_true(all(z_colnames %in% colnames(res)))

  z_recovered <- res[, z_colnames, drop = FALSE]
  expect_equal(unname(z_recovered), z_mat, tolerance = 1e-10)
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
