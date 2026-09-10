skip_if_not_installed("rstan")
#' "functions/bounds_to_range.stan" defines
#' `array[,] int bounds_to_range(array[] int lowers, int ub)`
#' which receives a sequence of integers defining lower bounds and an upper
#' bound, and returns an array [[lb_1, lb_2, ...], [ub_1, ub_2, ...]]
#' that array can then be used to obtain convenient dynamic range slicing:
#' some_array[1, k]:some_array[2, k] => the range slice lower:upper for item k

target <- "functions/bounds_to_range.stan"

skip_if_stan_unchanged(target)

model_bounds <- sprintf(
  "
functions {
  #include %s
}
data {
  int N;
  array[N] int lowers;
  int ub;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  array[2, N] int out_bounds = bounds_to_range(lowers, ub);
}
",
  target
) |>
  compile_stan_harness()

test_that("bounds_to_range converts lower bounds to (lower, upper) index spans", {
  lbounds <- c(1L, 5L, 10L)
  ubound <- 20L
  bounds <- run_stan_harness(
    model_bounds,
    data = list(N = length(lbounds), lowers = lbounds, ub = ubound),
    out_bounds
  )

  expect_equal(bounds[1, ], lbounds)
  expect_equal(bounds[2, ], c(tail(lbounds, -1) - 1L, ubound))
})

test_that("bounds_to_range handles single segment edge case", {
  lbounds <- 1L
  ubound <- 10L
  bounds <- run_stan_harness(
    model_bounds,
    data = list(N = length(lbounds), lowers = as.array(lbounds), ub = ubound),
    out_bounds
  )

  expect_equal(bounds[1, ], lbounds)
  expect_equal(bounds[2, ], ubound)
})

test_that("bounds_to_range errors if lower bounds are not strictly increasing", {
  expect_error(
    run_stan_harness(
      model_bounds,
      data = list(N = 3L, lowers = c(1L, 5L, 5L), ub = 10L),
      out_bounds
    ),
    "Lower bounds must be strictly increasing"
  )

  expect_error(
    run_stan_harness(
      model_bounds,
      data = list(N = 3L, lowers = c(1L, 5L, 3L), ub = 10L),
      out_bounds
    ),
    "Lower bounds must be strictly increasing"
  )
})

test_that("bounds_to_range errors if first lower bound is less than 1", {
  expect_error(
    run_stan_harness(
      model_bounds,
      data = list(N = 2L, lowers = c(0L, 5L), ub = 10L),
      out_bounds
    ),
    "First lower bound must be >= 1"
  )
})

test_that("bounds_to_range errors if last lower bound exceeds upper bound", {
  expect_error(
    run_stan_harness(
      model_bounds,
      data = list(N = 2L, lowers = c(1L, 15L), ub = 10L),
      out_bounds
    ),
    "Upper bound"
  )
})
