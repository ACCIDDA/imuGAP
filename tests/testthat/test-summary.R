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
    "`object` must be an object of class 'imugap_predict'"
  )
})

test_that("summary does not mutate the original target (copy semantics)", {
  pred <- make_pred()
  before <- data.table::copy(pred$target)
  invisible(summary(pred))
  expect_equal(pred$target, before)
})

test_that("summary handles 2D draws matrix and 1D draws vector", {
  # 2D draws matrix (e.g. n_draws x n_obs)
  set.seed(42)
  draws_2d <- matrix(runif(100 * 3), nrow = 100, ncol = 3)
  target_2d <- data.table::data.table(
    obs_c_id = 1:3,
    loc_id = c("a", "b", "c"),
    age = c(5L, 5L, 5L),
    cohort = c(1L, 1L, 1L),
    dose = c(2L, 2L, 2L)
  )
  pred_2d <- structure(
    list(draws = draws_2d, target = target_2d),
    class = "imugap_predict"
  )
  s_2d <- summary(pred_2d)
  expect_s3_class(s_2d, "data.table")
  expect_equal(nrow(s_2d), 3L)
  expect_equal(s_2d$mean, colMeans(draws_2d))

  # 1D draws vector (single target observation)
  draws_1d <- runif(100)
  target_1d <- data.table::data.table(
    obs_c_id = 1L,
    loc_id = "a",
    age = 5L,
    cohort = 1L,
    dose = 2L
  )
  pred_1d <- structure(
    list(draws = draws_1d, target = target_1d),
    class = "imugap_predict"
  )
  s_1d <- summary(pred_1d)
  expect_s3_class(s_1d, "data.table")
  expect_equal(nrow(s_1d), 1L)
  expect_equal(s_1d$mean, mean(draws_1d))
})
