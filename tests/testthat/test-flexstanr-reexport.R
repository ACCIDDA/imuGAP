# imuGAP re-exports flexstanr::stan_options (#122). Its full option-validation
# behavior (backend vocabulary, rejecting object/data/init, etc.) is tested in
# flexstanr; this only confirms the re-export is wired up and usable here.

test_that("stan_options is re-exported from flexstanr and usable", {
  opts <- stan_options()
  expect_type(opts, "list")
  expect_identical(opts$chains, 4L)
  expect_identical(opts$backend, "rstan")
  expect_identical(stan_options(chains = 2L)$chains, 2L)
})
