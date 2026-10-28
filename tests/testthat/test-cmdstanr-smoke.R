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
  skip_if(
    base::system.file(
      "stan",
      "bspline_static_zero_halley2_logit.stan",
      package = "imuGAP"
    ) ==
      "",
    "Stan source files not found in package installation"
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
      iter_warmup = 10,
      iter_sampling = 10,
      chains = 1,
      refresh = 0,
      seed = 1L
    )
  )))

  verify_imugap_smoke_fit(fit, obs, pop, locs, backend = "cmdstanr")
})
