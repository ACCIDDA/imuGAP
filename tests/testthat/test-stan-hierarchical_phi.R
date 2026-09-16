skip_if_not_installed("rstan")
#' "model/hierarchical_phi.stan" evaluates
#' hierarchical spatial observation probabilities `p_obs_*` by accumulating
#' baseline spline effects and multi-layer spatial random walk offsets across
#' location hierarchies.

target <- "model/hierarchical_phi.stan"

skip_if_stan_unchanged(c(
  "functions/unrolled_dose_static_lambda.stan",
  "functions/bounds_to_range.stan",
  "functions/lookups.stan",
  "functions/layer_offsets.stan",
  "data/shared.stan",
  "data/locations.stan",
  "data/uncensored/weights_location.stan",
  "data/right/weights_location.stan",
  "data/left/weights_location.stan",
  "data/bspline.stan",
  "transformed_data/common_indices.stan",
  "transformed_data/layer_indices.stan",
  "transformed_data/layer_phi_lookup.stan",
  "model/common_phi.stan",
  target
))

model_hierarchical_phi <- sprintf(
  "
functions {
  #include functions/unrolled_dose_static_lambda.stan
  #include functions/bounds_to_range.stan
  #include functions/lookups.stan
  #include functions/layer_offsets.stan
}
data {
  #include data/shared.stan
  #include data/locations.stan
  #include data/uncensored/weights_location.stan
  #include data/right/weights_location.stan
  #include data/left/weights_location.stan
  #include data/bspline.stan

  // Deterministic parameter inputs passed via data for exact testing
  vector[k_bs] beta_bs;
  vector[n_doses] lambda_raw;
  vector[(n_locs - 1) - n_parent_locs] z_layer;
  vector[n_layers - 1] sigma_layer;
}
transformed data {
  #include transformed_data/common_indices.stan
  #include transformed_data/layer_indices.stan
  #include transformed_data/layer_phi_lookup.stan
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  #include %s
}
",
  target
) |>
  compile_stan_harness()

test_that("hierarchical_phi.stan computes observation probabilities across hierarchy layers", {
  data("locations_sim", package = "imuGAP")
  locs_sim <- imuGAP:::canonicalize_locations(locations_sim)
  ld_sim <- imuGAP:::assemble_layer_data(locs_sim)

  # Three uncensored observations at different hierarchy depths:
  # 1. State level (loc 1, 2 contributing weights) - mixed
  # 2. County level (loc 2, 2 contributing weights) - mixed
  # 3. School level (loc 5, 1 contributing weight) - unmixed
  obs_bounds <- c(1L, 3L, 5L)
  w_cohort <- c(1L, 2L, 1L, 2L, 2L)
  w_loc <- c(1L, 1L, 2L, 2L, 5L)
  w_dose <- c(1L, 1L, 1L, 1L, 1L)
  w_life_year <- c(1L, 2L, 2L, 3L, 2L)
  weights <- c(0.4, 0.6, 0.5, 0.5, 1.0)

  bs <- matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2L, ncol = 2L)
  dose_sched <- matrix(1.0, nrow = 3L, ncol = 1L)
  n_intervals <- nrow(dose_sched)
  beta_bs <- c(-0.2, 0.3)
  lambda_val <- 1.2

  set.seed(42)
  n_unc <- (ld_sim$n_locs - 1L) - ld_sim$n_parent_locs
  z_layer <- rnorm(n_unc, mean = 0, sd = 1)
  sigma_layer <- rep(0.5, ld_sim$n_layers - 1L)

  data_list <- c(
    list(
      n_yr = nrow(dose_sched),
      n_cohort = nrow(bs),
      n_doses = ncol(dose_sched),
      n_intervals = n_intervals,
      dt_vec = rep(1.0, n_intervals),
      dose_sched = dose_sched,
      age_to_interval_map = seq_len(nrow(dose_sched)),
      predict_mode = 0L,
      num_threads = 1L,
      # Unmixed subset (observation 3)
      n_obs_unmixed_uncensored = 1L,
      y_obs_unmixed_uncensored = as.array(10L),
      y_smp_unmixed_uncensored = as.array(20L),
      w_cohort_unmixed_uncensored = as.array(2L),
      w_age_unmixed_uncensored = as.array(2L),
      w_dose_unmixed_uncensored = as.array(1L),
      w_loc_unmixed_uncensored = as.array(5L),
      # Mixed subset (observations 1 & 2)
      n_obs_mixed_uncensored = 2L,
      y_obs_mixed_uncensored = c(10L, 10L),
      y_smp_mixed_uncensored = c(20L, 20L),
      n_weights_mixed_uncensored = 4L,
      obs_bounds_mixed_uncensored = c(1L, 3L),
      w_cohort_mixed_uncensored = c(1L, 2L, 1L, 2L),
      w_age_mixed_uncensored = c(1L, 2L, 2L, 3L),
      w_dose_mixed_uncensored = c(1L, 1L, 1L, 1L),
      w_loc_mixed_uncensored = c(1L, 1L, 2L, 2L),
      weights_mixed_uncensored = c(0.4, 0.6, 0.5, 0.5)
    ),
    empty_obs_stream("right"),
    empty_obs_stream("left"),
    ld_sim,
    list(
      k_bs = ncol(bs),
      bs = bs,
      beta_bs = beta_bs,
      lambda_raw = as.array(log(lambda_val)),
      z_layer = z_layer,
      sigma_layer = sigma_layer
    )
  )

  p_unmix <- run_stan_harness(
    model_hierarchical_phi,
    data = data_list,
    p_obs_unmixed_uncensored
  )
  p_mix <- run_stan_harness(
    model_hierarchical_phi,
    data = data_list,
    p_obs_mixed_uncensored
  )

  expect_length(p_unmix, 1L)
  expect_length(p_mix, 2L)
  expect_true(all(p_unmix > 0 & p_unmix < 1))
  expect_true(all(p_mix > 0 & p_mix < 1))

  # Analytical closed-form expectation:
  # 1. Accumulate spatial offsets across layers
  qr_basis <- matrix(0, nrow = ld_sim$n_locs - 1L, ncol = n_unc)
  col_off <- 0L
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- ld_sim$parent_child_starts[p]
    en <- if (p < ld_sim$n_parent_locs) {
      ld_sim$parent_child_starts[p + 1L] - 1L
    } else {
      ld_sim$n_locs
    }
    k_len <- en - st + 1L
    pop_slice <- ld_sim$loc_population[st:en]
    w <- if (sum(pop_slice) > 0) {
      pop_slice / sum(pop_slice)
    } else {
      rep(1 / k_len, k_len)
    }
    mat_m <- cbind(
      sqrt(w) / sqrt(sum(w)),
      diag(k_len)[, seq_len(k_len - 1L), drop = FALSE]
    )
    q_star <- qr.Q(qr(mat_m))[, -1L, drop = FALSE]
    qr_basis[
      (st - 1L):(en - 1L),
      (col_off + 1L):(col_off + k_len - 1L)
    ] <- q_star
    col_off <- col_off + k_len - 1L
  }
  loc_pop_scale <- numeric(ld_sim$n_locs - 1L)
  for (k in seq_len(ld_sim$n_layers - 1L)) {
    st <- ld_sim$layer_starts[k + 1L]
    en <- if (k + 1L < ld_sim$n_layers) {
      ld_sim$layer_starts[k + 2L] - 1L
    } else {
      ld_sim$n_locs
    }
    lp <- ld_sim$loc_population[st:en]
    loc_pop_scale[(st - 1L):(en - 1L)] <- sqrt(mean(lp) / lp)
  }
  loc_layer_idx <- rep(
    seq_len(ld_sim$n_layers - 1L),
    times = diff(c(ld_sim$layer_starts, ld_sim$n_locs + 1L))[-1L]
  )
  off_layer <- as.vector((qr_basis %*% z_layer) * loc_pop_scale) *
    sigma_layer[loc_layer_idx]

  expected_logit_phi_loc <- numeric(ld_sim$n_locs)
  expected_logit_phi_loc[1] <- 0.0
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- ld_sim$parent_child_starts[p]
    en <- if (p < ld_sim$n_parent_locs) {
      ld_sim$parent_child_starts[p + 1L] - 1L
    } else {
      ld_sim$n_locs
    }
    expected_logit_phi_loc[
      st:en
    ] <- expected_logit_phi_loc[ld_sim$parent_loc_id[p]] +
      off_layer[(st - 1L):(en - 1L)]
  }

  # 2. Compute individual weight contributions and aggregate per observation slice
  logit_phi_st <- as.vector(bs %*% beta_bs)
  phi_inv <- 1.0 -
    stats::plogis(logit_phi_st[w_cohort] + expected_logit_phi_loc[w_loc])
  cdfs <- 1.0 - exp(-lambda_val * w_life_year)
  weighted <- weights * phi_inv * cdfs

  obs_starts <- obs_bounds
  obs_ends <- c(tail(obs_bounds, -1) - 1L, length(w_cohort))
  expected_p <- vapply(
    seq_along(obs_starts),
    function(i) sum(weighted[obs_starts[i]:obs_ends[i]]),
    numeric(1)
  )

  expect_equal(as.numeric(p_mix), expected_p[1:2], tolerance = 1e-6)
  expect_equal(as.numeric(p_unmix), expected_p[3], tolerance = 1e-6)
})
