skip_if_not_installed("rstan")
#' "transformed_data/uncensored/single_phi_lookup.stan" defines
#' precomputed 1D phi index mappings from weights cohort indices
#' for single-location models.

target <- "transformed_data/uncensored/single_phi_lookup.stan"

skip_if_stan_unchanged(target)

model_single_phi_lookup <- sprintf(
  "
data {
  int<lower=0> n_obs_unmixed_uncensored;
  array[n_obs_unmixed_uncensored] int w_cohort_unmixed_uncensored;

  int<lower=0> n_weights_mixed_uncensored;
  array[n_weights_mixed_uncensored] int w_cohort_mixed_uncensored;
}
transformed data {
  #include %s
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  array[n_obs_unmixed_uncensored] int out_phi_lookup_unmixed = phi_lookup_unmixed_uncensored;
  array[n_weights_mixed_uncensored] int out_phi_lookup_mixed = phi_lookup_mixed_uncensored;
}
",
  target
) |>
  compile_stan_harness()

test_that("uncensored/single_phi_lookup.stan computes single-layer phi lookups", {
  w_cohort_unmix <- c(1L, 2L)
  w_cohort_mix <- c(1L, 3L)

  data_list <- list(
    n_obs_unmixed_uncensored = length(w_cohort_unmix),
    w_cohort_unmixed_uncensored = w_cohort_unmix,
    n_weights_mixed_uncensored = length(w_cohort_mix),
    w_cohort_mixed_uncensored = w_cohort_mix
  )

  results <- run_stan_harness(
    model_single_phi_lookup,
    data = data_list
  )

  expect_equal(as.numeric(results$out_phi_lookup_unmixed), w_cohort_unmix)
  expect_equal(as.numeric(results$out_phi_lookup_mixed), w_cohort_mix)
})
