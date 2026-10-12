test_that("compute_phi validates input object class", {
  expect_error(
    compute_phi(list()),
    err_pattern(ERR_NOT_IMUGAP_FIT, name = "object")
  )
})

test_that("compute_phi works on multi-layer fit_sim", {
  data("fit_sim", package = "imuGAP")

  phi <- suppressWarnings(compute_phi(fit_sim, posterior_size = 8L))
  expect_true(is.array(phi))
  expect_equal(dim(phi)[1], 8L)
  expect_equal(dim(phi)[2], fit_sim$data$n_cohort)
  expect_equal(dim(phi)[3], fit_sim$data$n_locs)
  expect_true(all(phi >= 0 & phi <= 1))

  # Test link scale
  eta <- suppressWarnings(compute_phi(
    fit_sim,
    link_scale = TRUE,
    posterior_size = 8L
  ))
  expect_true(is.array(eta))
  expect_equal(dim(eta), dim(phi))
  expect_equal(phi, stats::plogis(eta))

  # Test dimnames
  expect_equal(dimnames(phi)$location, fit_sim$locations$loc_id)
})

test_that("compute_phi works on 1-layer fit_sim_1layer", {
  data("fit_sim_1layer", package = "imuGAP")

  phi_1l <- suppressWarnings(compute_phi(fit_sim_1layer, posterior_size = 8L))
  expect_true(is.array(phi_1l))
  expect_equal(dim(phi_1l)[1], 8L)
  expect_equal(dim(phi_1l)[2], fit_sim_1layer$data$n_cohort)
  expect_equal(dim(phi_1l)[3], 1L)
  expect_true(all(phi_1l >= 0 & phi_1l <= 1))
})
