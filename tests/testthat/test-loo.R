test_that("log_lik.imugap_fit computes valid pointwise log-likelihood matrix", {
  data("fit_sim", package = "imuGAP")

  # Test direct method call
  ll_sub <- suppressWarnings(log_lik.imugap_fit(fit_sim, posterior_size = 50))
  expect_true(is.matrix(ll_sub))
  expect_equal(nrow(ll_sub), 52L) # 50 rounded up to multiple of 4 chains = 52
  expect_true(ncol(ll_sub) > 0L)
  expect_true(all(is.finite(ll_sub)))
  expect_true(all(ll_sub <= 0))

  # Test with all draws
  ll_all <- log_lik.imugap_fit(fit_sim)
  expect_true(is.matrix(ll_all))
  expect_equal(nrow(ll_all), 2000L) # fit_sim has 2000 posterior draws (500 iter x 4 chains)
  expect_equal(ncol(ll_all), ncol(ll_sub))
  expect_true(all(is.finite(ll_all)))
  expect_true(all(ll_all <= 0))

  # Test via rstantools generic
  ll_generic <- suppressWarnings(rstantools::log_lik(
    fit_sim,
    posterior_size = 50
  ))
  expect_identical(ll_generic, ll_sub)
})

test_that("log_lik and loo raise appropriate errors for invalid inputs", {
  expect_error(
    log_lik.imugap_fit("not_a_fit"),
    err_pattern(ERR_NOT_IMUGAP_FIT, name = "object")
  )

  expect_error(
    loo.imugap_fit("not_a_fit"),
    err_pattern(ERR_NOT_IMUGAP_FIT, name = "x")
  )
})

test_that("loo.imugap_fit errors informatively when loo package is not available", {
  data("fit_sim", package = "imuGAP")
  testthat::with_mocked_bindings(
    expect_error(
      loo.imugap_fit(fit_sim),
      err_pattern(ERR_LOO_NOT_INSTALLED)
    ),
    requireNamespace = function(package, ...) {
      if (package == "loo") FALSE else base::requireNamespace(package, ...)
    },
    .package = "base"
  )
})

test_that("loo.imugap_fit works with loo package and accounts for r_eff", {
  skip_if_not_installed("loo")
  data("fit_sim", package = "imuGAP")

  # Auto-computed r_eff for multi-chain fit
  loo_res <- suppressWarnings(loo::loo(fit_sim, posterior_size = 100))
  expect_s3_class(loo_res, "loo")
  expect_true(all(
    c("estimates", "pointwise", "diagnostics") %in% names(loo_res)
  ))
  expect_true("elpd_loo" %in% rownames(loo_res$estimates))
  expect_true("p_loo" %in% rownames(loo_res$estimates))
  expect_true("looic" %in% rownames(loo_res$estimates))
  expect_false(is.null(loo_res$diagnostics$r_eff))
  expect_true(all(is.finite(loo_res$diagnostics$r_eff)))
  expect_true(all(loo_res$diagnostics$r_eff > 0))

  # User override of r_eff is respected
  custom_reff <- rep(0.5, length(loo_res$diagnostics$r_eff))
  loo_custom <- suppressWarnings(loo::loo(
    fit_sim,
    posterior_size = 100,
    r_eff = custom_reff
  ))
  expect_s3_class(loo_custom, "loo")
  expect_equal(loo_custom$diagnostics$r_eff, custom_reff)
})

test_that("log_lik.imugap_fit() generates and extracts pointwise log-likelihood", {
  data("fit_sim", package = "imuGAP")

  # Test log_lik on fit_sim where log_lik was not pre-computed during sampling
  ll_mat <- suppressWarnings(rstantools::log_lik(
    fit_sim,
    posterior_size = 50
  ))
  expect_true(is.matrix(ll_mat))
  expect_equal(nrow(ll_mat), 52L) # 52 draws (13 * 4 chains)
  expect_equal(ncol(ll_mat), nrow(fit_sim$observations))

  # Test extraction when log_lik is pre-computed in fit draws
  fake_fit <- fit_sim
  raw_draws <- flexstanr::backend_draws_array(fake_fit$raw_fit)
  n_obs_total <- ncol(ll_mat)
  ll_names <- paste0("log_lik[", seq_len(n_obs_total), "]")
  fake_ll_draws <- array(
    -runif(dim(raw_draws)[1] * dim(raw_draws)[2] * n_obs_total),
    dim = c(dim(raw_draws)[1], dim(raw_draws)[2], n_obs_total),
    dimnames = list(NULL, NULL, ll_names)
  )
  combined_draws <- abind::abind(raw_draws, fake_ll_draws, along = 3)
  # Mock backend_draws_array to return combined_draws containing log_lik parameters
  testthat::with_mocked_bindings(
    {
      ll_extracted <- suppressWarnings(rstantools::log_lik(
        fake_fit,
        posterior_size = 50
      ))
      expect_true(is.matrix(ll_extracted))
      expect_equal(nrow(ll_extracted), 52L)
      expect_equal(ncol(ll_extracted), n_obs_total)
      expect_equal(
        colnames(ll_extracted),
        paste0("obs[", seq_len(n_obs_total), "]")
      )
      expect_equal(
        unname(ll_extracted),
        unname(apply(
          combined_draws[
            seq.int(dim(raw_draws)[1] - 13L + 1L, dim(raw_draws)[1]),
            seq_len(dim(raw_draws)[2]),
            ll_names,
            drop = FALSE
          ],
          3L,
          c
        ))
      )
    },
    backend_draws_array = function(...) combined_draws,
    .package = "imuGAP"
  )
})
