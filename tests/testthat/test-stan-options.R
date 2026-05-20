# Tests for stan_options()

test_that("stan_options returns an empty list with no arguments", {
  sopts <- stan_options()
  expect_type(sopts, "list")
  expect_length(sopts, 0L)
})

test_that("stan_options passes named arguments through", {
  sopts <- stan_options(iter = 100, chains = 2, warmup = 50)
  expect_equal(sopts$iter, 100)
  expect_equal(sopts$chains, 2)
  expect_equal(sopts$warmup, 50)
})

test_that("stan_options preserves argument names and values verbatim", {
  sopts <- stan_options(seed = 42L, cores = 4L, refresh = 0)
  expect_setequal(names(sopts), c("seed", "cores", "refresh"))
  expect_equal(sopts$seed, 42L)
  expect_equal(sopts$cores, 4L)
  expect_equal(sopts$refresh, 0)
})

test_that("stan_options rejects 'object' argument", {
  expect_error(
    stan_options(object = "foo"),
    regexp = "object.*imugap_options"
  )
})

test_that("stan_options rejects 'object' even when mixed with valid args", {
  expect_error(
    stan_options(iter = 100, object = "foo"),
    regexp = "object.*imugap_options"
  )
})

test_that("stan_options rejects 'data' argument", {
  expect_error(
    stan_options(data = list()),
    regexp = "data.*sampling"
  )
})

test_that("stan_options rejects 'data' even when mixed with valid args", {
  expect_error(
    stan_options(iter = 100, data = list()),
    regexp = "data.*sampling"
  )
})

test_that("stan_options rejects non-positive-int iter/chains/warmup/cores", {
  for (arg in c("iter", "chains", "warmup", "cores")) {
    expect_error(do.call(stan_options, setNames(list(-1L), arg)), arg)
    expect_error(do.call(stan_options, setNames(list(0L), arg)), arg)
    expect_error(do.call(stan_options, setNames(list(1.5), arg)), arg)
    expect_error(do.call(stan_options, setNames(list(c(1L, 2L)), arg)), arg)
    expect_error(do.call(stan_options, setNames(list(NA_integer_), arg)), arg)
    expect_error(do.call(stan_options, setNames(list("100"), arg)), arg)
  }
})

test_that("stan_options coerces iter/chains/warmup/cores to integer", {
  sopts <- stan_options(iter = 200, chains = 2, warmup = 50, cores = 1)
  expect_type(sopts$iter, "integer")
  expect_type(sopts$chains, "integer")
  expect_type(sopts$warmup, "integer")
  expect_type(sopts$cores, "integer")
})

test_that("stan_options coerces seed via as.integer per rstan convention", {
  expect_silent(stan_options(seed = 1L))
  expect_silent(stan_options(seed = -1L))
  expect_equal(stan_options(seed = "12345")$seed, 12345L)

  expect_error(stan_options(seed = c(1L, 2L)), "seed")
  expect_error(stan_options(seed = NA_integer_), "seed")
  expect_error(stan_options(seed = "abc"), "seed")
})

test_that("stan_options leaves non-integer-coerced args (e.g. refresh) alone", {
  sopts <- stan_options(refresh = 0)
  expect_equal(sopts$refresh, 0)
})
