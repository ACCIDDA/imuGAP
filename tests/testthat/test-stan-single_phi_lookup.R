skip_if_not_installed("rstan")
# ' "transformed_data/uncensored/single_phi_lookup.stan" defines
# ' precomputed 1D phi index mappings from weights cohort indices
# ' for single-location models.

target <- "transformed_data/uncensored/single_phi_lookup.stan"

skip_if_stan_unchanged(target)

model_single_phi_lookup <- sprintf(
  "
data {
  int<lower=0> n_weights_uncensored;
  array[n_weights_uncensored] int weights_cohort_uncensored;
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
  array[n_weights_uncensored] int out_phi_lookup_unc = phi_lookup_uncensored;
}
",
  target
) |>
  compile_stan_harness()

test_that("uncensored/single_phi_lookup.stan computes single-layer phi lookups", {
  w_cohort <- c(1L, 2L, 1L, 3L)

  data_list <- list(
    n_weights_uncensored = length(w_cohort),
    weights_cohort_uncensored = w_cohort
  )

  phi_lookup <- run_stan_harness(
    model_single_phi_lookup,
    data = data_list,
    out_phi_lookup_unc
  )

  expect_equal(
    as.numeric(phi_lookup),
    w_cohort
  )
})
