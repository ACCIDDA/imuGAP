# Smoke test for the imuGAP() Stan model.
#
# Runs imuGAP() end-to-end on the bundled *_sim data with minimal sampling
# settings (1 chain, 100 iterations) and verifies that the fit returns the
# expected parameters with sensible values. This is intentionally light on
# convergence checking -- 100 iterations is nowhere near enough -- but it
# does confirm the R -> Stan -> R round-trip is wired up correctly.

test_that("imuGAP() runs end-to-end on bundled *_sim data", {
  locs <- canonicalize_locations(locations_sim)
  obs <- canonicalize_observations(observations_sim)
  pop <- canonicalize_populations(populations_sim, obs, locs)

  fit <- suppressWarnings(imuGAP(
    obs,
    pop,
    locs,
    stan_opts = stan_options(
      iter = 100,
      chains = 1,
      refresh = 0,
      seed = 1L
    )
  ))

  expect_s4_class(fit, "stanfit")

  fit_pars <- fit@model_pars
  for (par in c("logit_phi_st", "phi", "p_obs", "lambda_raw")) {
    expect_true(par %in% fit_pars, info = paste("missing parameter:", par))
  }

  phi <- rstan::extract(fit, pars = "phi")$phi
  expect_true(all(is.finite(phi)))
  expect_true(all(phi >= 0 & phi <= 1))
})
