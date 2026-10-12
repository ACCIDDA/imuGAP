# Smoke test for the imuGAP::sampling() Stan model.
#
# Runs imuGAP::sampling() end-to-end on the bundled *_sim data with minimal sampling
# settings (1 chain, 100 iterations) and verifies that the fit returns the
# expected parameters with sensible values. This is intentionally light on
# convergence checking -- 100 iterations is nowhere near enough -- but it
# does confirm the R -> Stan -> R round-trip is wired up correctly.

test_that("imuGAP::sampling() runs end-to-end on bundled *_sim data", {
  locs <- canonicalize_locations(locations_sim)
  obs <- canonicalize_observations(observations_sim)
  pop <- canonicalize_populations(populations_sim, obs, locs)

  fit <- suppressWarnings(imuGAP::sampling(
    obs,
    pop,
    locs,
    stan_opts = stan_options(
      iter = 20,
      chains = 1,
      refresh = 0,
      seed = 1L
    )
  ))

  verify_imugap_smoke_fit(fit, obs, pop, locs, backend = "rstan")
})

test_that("imuGAP::sampling() runs end-to-end with 1-layer location hierarchy", {
  locs_1layer <- canonicalize_locations(locations_sim)[layer <= 1]
  pops_1layer <- data.table::copy(populations_sim)[, loc_id := "State"]
  pops_1layer <- pops_1layer[,
    .(weight = sum(weight)),
    by = .(obs_id, loc_id, cohort, age, dose)
  ]
  pops_1layer <- pops_1layer[loc_id %in% locs_1layer$loc_id]
  obs_1layer <- observations_sim[obs_id %in% pops_1layer$obs_id]

  fit <- suppressWarnings(imuGAP::sampling(
    obs_1layer,
    pops_1layer,
    locs_1layer,
    stan_opts = stan_options(
      iter = 20,
      chains = 1,
      refresh = 0,
      seed = 1L
    )
  ))

  expect_s3_class(fit, "imugap_fit")
  expect_s4_class(fit$raw_fit, "stanfit")
  expect_null(fit$data$n_layers)
  expect_null(fit$data$n_locs)
  expect_true("beta_bs" %in% fit$raw_fit@model_pars)
  expect_false("off_layer" %in% fit$raw_fit@model_pars)
  expect_false("mu_offset" %in% fit$raw_fit@model_pars)

  phi_1l <- compute_phi(fit)
  expect_equal(dim(phi_1l)[3], 1L)
  expect_true(all(phi_1l >= 0 & phi_1l <= 1))
})

test_that("imuGAP::sampling() runs end-to-end with 2-layer location hierarchy", {
  locs_2layer <- canonicalize_locations(locations_sim)[layer <= 2]
  pops_2layer <- data.table::copy(populations_sim)[
    loc_id %in% locs_2layer$loc_id
  ]
  obs_2layer <- data.table::copy(observations_sim)[
    obs_id %in% pops_2layer$obs_id
  ]

  fit <- suppressWarnings(imuGAP::sampling(
    obs_2layer,
    pops_2layer,
    locs_2layer,
    stan_opts = stan_options(
      iter = 20,
      chains = 1,
      refresh = 0,
      seed = 1L
    )
  ))

  expect_s3_class(fit, "imugap_fit")
  expect_s4_class(fit$raw_fit, "stanfit")
  expect_equal(fit$data$n_layers, max(locs_2layer$layer))
  expect_equal(fit$data$n_locs, nrow(locs_2layer))
  expect_true("beta_bs" %in% fit$raw_fit@model_pars)
  expect_true("off_layer" %in% fit$raw_fit@model_pars)
  expect_true("mu_offset" %in% fit$raw_fit@model_pars)
})
