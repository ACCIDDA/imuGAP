skip_if_not_installed("rstan")
#' "model/hierarchical_phi.stan" evaluates
#' hierarchical spatial observation probabilities `p_obs_*` by accumulating
#' baseline spline effects and multi-layer spatial random walk offsets across
#' location hierarchies.

target <- "model/hierarchical_phi.stan"

skip_if_stan_unchanged(c(
  "functions/diff.stan",
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
  #include functions/diff.stan
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
  real epsilon_p;

  // Deterministic parameter inputs passed via data for exact testing
  vector[k_bs] beta_bs;
  vector[n_doses] lambda_raw;
  vector[n_locs - 1] off_layer;
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
  locs_sim <- canonicalize_locations(locations_sim)
  ld_sim <- assemble_layer_data(locs_sim)

  # Three uncensored observations at different hierarchy depths:
  # 1. State level (loc 1, 2 contributing weights)
  # 2. County level (loc 2, 2 contributing weights)
  # 3. School level (loc 5, 1 contributing weight)
  obs_bounds <- c(1L, 3L, 5L)
  w_cohort <- c(1L, 2L, 1L, 2L, 2L)
  w_loc <- c(1L, 1L, 2L, 2L, 5L)
  w_dose <- c(1L, 1L, 1L, 1L, 1L)
  w_life_year <- c(1L, 2L, 2L, 3L, 2L)
  weights <- c(0.4, 0.6, 0.5, 0.5, 1.0)

  bs <- matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2L, ncol = 2L)
  dose_sched <- matrix(c(1.0, 1.0), nrow = 3L, ncol = 1L)
  beta_bs <- c(-0.2, 0.3)
  lambda_val <- 1.2

  set.seed(42)
  off_layer <- rnorm(ld_sim$n_locs - 1L, mean = 0, sd = 0.5)

  empty_stream <- function(tag) {
    setNames(
      list(
        0L,
        integer(0),
        integer(0),
        0L,
        integer(0),
        integer(0),
        integer(0),
        integer(0),
        integer(0),
        numeric(0)
      ),
      paste0(
        c(
          "n_obs_",
          "y_obs_",
          "y_smp_",
          "n_weights_",
          "obs_to_weights_bounds_",
          "weights_cohort_",
          "weights_location_",
          "weights_dose_",
          "weights_life_year_",
          "weights_"
        ),
        tag
      )
    )
  }

  data_list <- c(
    list(
      n_yr = nrow(dose_sched),
      n_cohort = nrow(bs),
      n_doses = ncol(dose_sched),
      dose_sched = dose_sched,
      predict_mode = 0L,
      n_obs_uncensored = length(obs_bounds),
      y_obs_uncensored = rep(10L, length(obs_bounds)),
      y_smp_uncensored = rep(20L, length(obs_bounds)),
      n_weights_uncensored = length(w_cohort),
      obs_to_weights_bounds_uncensored = obs_bounds,
      weights_cohort_uncensored = w_cohort,
      weights_location_uncensored = w_loc,
      weights_dose_uncensored = w_dose,
      weights_life_year_uncensored = w_life_year,
      weights_uncensored = weights
    ),
    empty_stream("right"),
    empty_stream("left"),
    ld_sim,
    list(
      k_bs = ncol(bs),
      bs = bs,
      epsilon_p = 1e-9,
      beta_bs = beta_bs,
      lambda_raw = as.array(log(lambda_val)),
      off_layer = off_layer
    )
  )

  p_obs <- run_stan_harness(
    model_hierarchical_phi,
    data = data_list,
    p_obs_uncensored
  )

  expect_length(p_obs, length(obs_bounds))
  expect_true(all(p_obs > 0 & p_obs < 1))

  # Analytical closed-form expectation:
  # 1. Accumulate spatial offsets across layers
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

  expect_equal(as.numeric(p_obs), expected_p, tolerance = 1e-6)
})
