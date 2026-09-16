skip_if_not_installed("rstan")
#' "functions/observation_likelihood_reduce.stan" defines
#' partial log-likelihood reduction kernels for reduce_sum.

target <- "functions/observation_likelihood_reduce.stan"

skip_if_stan_unchanged(target)

model_reduce <- sprintf(
  "
functions {
  #include %s
}
data {
  int N_unmixed;
  array[N_unmixed] int y_obs_unmixed;
  array[N_unmixed] int y_smp_unmixed;
  array[N_unmixed] int phi_lookup_unmixed;
  array[N_unmixed] int cdf_lookup_unmixed;

  int N_mixed;
  int N_weights;
  array[N_mixed] int y_obs_mixed;
  array[N_mixed] int y_smp_mixed;
  array[2, N_mixed] int obs_map_mixed;
  vector[N_weights] weighted_mixed;

  int N_phi;
  int N_unrolled;
  vector[N_phi] phi;
  vector[N_unrolled] unrolled_dose_probs;
  int grainsize;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
  if (N_unmixed > 0) {
    vector[N_unmixed] p_unmixed =
      (1.0 - phi[phi_lookup_unmixed]) .* unrolled_dose_probs[cdf_lookup_unmixed];
    target += reduce_sum(
      unmixed_uncensored_partial_lupmf,
      y_obs_unmixed,
      grainsize,
      y_smp_unmixed,
      p_unmixed
    );
    target += reduce_sum(
      unmixed_right_partial_lcdf,
      y_obs_unmixed,
      grainsize,
      y_smp_unmixed,
      phi_lookup_unmixed,
      cdf_lookup_unmixed,
      phi,
      unrolled_dose_probs
    );
    target += reduce_sum(
      unmixed_left_partial_lcdf,
      y_obs_unmixed,
      grainsize,
      y_smp_unmixed,
      phi_lookup_unmixed,
      cdf_lookup_unmixed,
      phi,
      unrolled_dose_probs
    );
  }
  if (N_mixed > 0) {
    target += reduce_sum(
      mixed_uncensored_partial_lupmf,
      y_obs_mixed,
      grainsize,
      y_smp_mixed,
      obs_map_mixed,
      weighted_mixed
    );
    target += reduce_sum(
      mixed_right_partial_lcdf,
      y_obs_mixed,
      grainsize,
      y_smp_mixed,
      obs_map_mixed,
      weighted_mixed
    );
    target += reduce_sum(
      mixed_left_partial_lcdf,
      y_obs_mixed,
      grainsize,
      y_smp_mixed,
      obs_map_mixed,
      weighted_mixed
    );
  }
}
",
  target
) |>
  compile_stan_harness()

test_that("reduce_sum partial functions evaluate equivalent log-probabilities across grainsizes", {
  n_unmixed <- 4L
  y_obs_u <- c(10L, 15L, 20L, 25L)
  y_smp_u <- c(20L, 30L, 40L, 50L)
  phi_u <- c(1L, 2L, 1L, 2L)
  cdf_u <- c(1L, 1L, 2L, 2L)
  phi_vec <- c(0.1, 0.4)
  unrolled_vec <- c(0.5, 0.8)

  n_mixed <- 2L
  y_obs_m <- c(8L, 12L)
  y_smp_m <- c(16L, 24L)
  obs_map_m <- matrix(c(1L, 2L, 3L, 4L), nrow = 2, ncol = 2)
  weighted_m <- c(0.2, 0.3, 0.25, 0.35)

  d_g1 <- list(
    N_unmixed = n_unmixed,
    y_obs_unmixed = y_obs_u,
    y_smp_unmixed = y_smp_u,
    phi_lookup_unmixed = phi_u,
    cdf_lookup_unmixed = cdf_u,
    N_mixed = n_mixed,
    N_weights = 4L,
    y_obs_mixed = y_obs_m,
    y_smp_mixed = y_smp_m,
    obs_map_mixed = obs_map_m,
    weighted_mixed = weighted_m,
    N_phi = 2L,
    N_unrolled = 2L,
    phi = phi_vec,
    unrolled_dose_probs = unrolled_vec,
    grainsize = 1L
  )

  d_g2 <- d_g1
  d_g2$grainsize <- 2L

  lp_fit1 <- run_stan_harness(
    model_reduce,
    data = d_g1,
    pars = NULL,
    return_fit = TRUE
  )
  lp1 <- rstan::log_prob(lp_fit1, 0.0, adjust_transform = FALSE)

  lp_fit2 <- run_stan_harness(
    model_reduce,
    data = d_g2,
    pars = NULL,
    return_fit = TRUE
  )
  lp2 <- rstan::log_prob(lp_fit2, 0.0, adjust_transform = FALSE)

  expect_equal(lp1, lp2, tolerance = 1e-6)
})
