# Tests for summary.imugap_predict (added with the draws-compatible predict
# refactor, #70). summary() operates purely on the `draws` matrix + `target`
# data.table of an `imugap_predict` object, so we construct a minimal synthetic
# object rather than run the sampler — keeps the test fast and deterministic.

# Build a fake imugap_predict: 100 posterior draws across 3 target observations.
make_pred <- function(seed = 1L) {
  set.seed(seed)
  n_draws <- 100L
  n_chains <- 2L
  n_obs <- 3L
  draws <- array(
    runif(n_draws * n_chains * n_obs),
    dim = c(n_draws, n_chains, n_obs)
  )
  target <- data.table::data.table(
    obs_c_id = 1:3,
    loc_id = c("a", "b", "c"),
    age = c(5L, 5L, 5L),
    cohort = c(1L, 1L, 1L),
    dose = c(2L, 2L, 2L)
  )
  structure(list(draws = draws, target = target), class = "imugap_predict")
}

test_that("summary returns one row per target obs with target cols + stats", {
  pred <- make_pred()
  s <- summary(pred)

  expect_s3_class(s, "data.table")
  expect_equal(nrow(s), 3L) # one row per target observation
  # carries the target columns through
  expect_true(all(
    c("obs_c_id", "loc_id", "age", "cohort", "dose") %in% names(s)
  ))
  # default probs c(.025,.5,.975) -> mean + q2_5 / q50 / q97_5
  expect_true(all(c("mean", "q2_5", "q50", "q97_5") %in% names(s)))
})

test_that("summary statistics match a manual reduction of the draws", {
  pred <- make_pred()
  s <- summary(pred)

  expect_equal(s$mean, colMeans(pred$draws, dims = 2), tolerance = 1e-12)
  manual_q <- t(apply(
    pred$draws,
    3,
    stats::quantile,
    probs = c(0.025, 0.5, 0.975)
  ))
  expect_equal(s$q2_5, unname(manual_q[, 1]), tolerance = 1e-12)
  expect_equal(s$q50, unname(manual_q[, 2]), tolerance = 1e-12)
  expect_equal(s$q97_5, unname(manual_q[, 3]), tolerance = 1e-12)
})

test_that("custom probs control which quantile columns appear", {
  pred <- make_pred()
  s <- summary(pred, probs = c(0.1, 0.9))

  expect_true(all(c("q10", "q90") %in% names(s)))
  expect_false(any(c("q2_5", "q50", "q97_5") %in% names(s)))
  expect_equal(
    s$q10,
    unname(apply(pred$draws, 3, stats::quantile, probs = 0.1)),
    tolerance = 1e-12
  )
})

test_that("summary errors on a non-imugap_predict object", {
  expect_error(
    summary.imugap_predict(list(
      draws = matrix(0),
      target = data.table::data.table()
    )),
    "must be of class 'imugap_predict'"
  )
})

test_that("summary does not mutate the original target (copy semantics)", {
  pred <- make_pred()
  before <- data.table::copy(pred$target)
  invisible(summary(pred))
  expect_equal(pred$target, before)
})
