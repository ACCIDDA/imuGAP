# End-to-end smoke test for the cmdstanr backend. Skips unless cmdstanr and a
# working CmdStan toolchain are available, so it is safe on CI and CRAN (which
# don't have them) and runs locally for anyone set up for cmdstanr. Mirrors the
# rstan smoke test in test-sampling-smoke.R: minimal sampling settings, light on
# convergence, just confirming the R -> Stan -> R round-trip via cmdstanr.

test_that("imuGAP::sampling() fits via the cmdstanr backend", {
  skip_if_not_installed("cmdstanr")
  skip_if(
    inherits(try(cmdstanr::cmdstan_version(), silent = TRUE), "try-error"),
    "CmdStan toolchain not available"
  )

  locs <- canonicalize_locations(locations_sim)
  obs <- canonicalize_observations(observations_sim)
  pop <- canonicalize_populations(populations_sim, obs, locs)

  fit <- suppressWarnings(suppressMessages(imuGAP::sampling(
    obs,
    pop,
    locs,
    stan_opts = stan_options(
      backend = "cmdstanr",
      iter_warmup = 100,
      iter_sampling = 100,
      chains = 1,
      refresh = 0,
      seed = 1L
    )
  )))

  expect_s3_class(fit, "imugap_fit")
  expect_s3_class(fit$stanfit, "CmdStanMCMC")

  fit_pars <- fit$stanfit$metadata()$stan_variables
  for (par in c("beta_bs", "lambda_raw")) {
    expect_true(par %in% fit_pars, info = paste("missing parameter:", par))
  }
  for (par in c("logit_phi_st", "phi")) {
    expect_false(
      par %in% fit_pars,
      info = paste("parameter should be absent:", par)
    )
  }

  draws <- fit$stanfit$draws(variables = "beta_bs")
  expect_true(all(is.finite(draws)))
})
