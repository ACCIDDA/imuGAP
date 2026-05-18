# Tests for imugap_options() and stan_options()

# --- imugap_options ----------------------------------------------------------

test_that("imugap_options returns expected structure with defaults", {
  opts <- imugap_options()
  expect_type(opts, "list")
  expect_setequal(names(opts), c("df", "dose_schedule", "object"))
  expect_equal(opts$df, 5L)
  expect_equal(opts$dose_schedule, c(1, 4))
  expect_s4_class(opts$object, "stanmodel")
})

test_that("imugap_options default object is the v6 stanmodel", {
  opts <- imugap_options()
  expect_equal(opts$object@model_name, "impute_school_coverage_process_v6")
})

test_that("imugap_options df can be overridden", {
  opts <- imugap_options(df = 10L)
  expect_equal(opts$df, 10L)
  expect_equal(opts$dose_schedule, c(1, 4))
  expect_s4_class(opts$object, "stanmodel")
})

test_that("imugap_options dose_schedule can be overridden", {
  opts <- imugap_options(dose_schedule = c(2, 5, 7))
  expect_equal(opts$dose_schedule, c(2, 5, 7))
  expect_equal(opts$df, 5L)
})

test_that("imugap_options errors on unknown object", {
  expect_error(
    imugap_options(object = "unknown_model"),
    "Unknown model object"
  )
  expect_error(
    imugap_options(object = "stateonly"),
    "Unknown model object"
  )
})

test_that("imugap_options accepts default keyword explicitly", {
  opts <- imugap_options(object = "default")
  expect_s4_class(opts$object, "stanmodel")
  expect_equal(opts$object@model_name, "impute_school_coverage_process_v6")
})

# --- stan_options ------------------------------------------------------------

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
    "Passing 'object' in stan_options is not allowed"
  )
})

test_that("stan_options rejects 'object' even when mixed with valid args", {
  expect_error(
    stan_options(iter = 100, object = "foo"),
    "Passing 'object' in stan_options is not allowed"
  )
})

test_that("stan_options error message points users to imugap_options", {
  expect_error(
    stan_options(object = NULL),
    "imugap_options"
  )
})

# --- validation: imugap_options ----------------------------------------------

test_that("imugap_options accepts numeric whole-number df", {
  opts <- imugap_options(df = 4)
  expect_equal(opts$df, 4L)
})

test_that("imugap_options rejects invalid df", {
  expect_error(imugap_options(df = -5L), "df")
  expect_error(imugap_options(df = 0L), "df")
  expect_error(imugap_options(df = 5.5), "df")
  expect_error(imugap_options(df = c(5L, 5L)), "df")
  expect_error(imugap_options(df = NA_integer_), "df")
  expect_error(imugap_options(df = "5"), "df")
})

test_that("imugap_options rejects invalid dose_schedule", {
  valid_schedule <- c(1, 4)
  expect_error(
    imugap_options(dose_schedule = -valid_schedule), "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(0, valid_schedule)), "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(valid_schedule, NA)), "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = numeric(0)), "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = as.character(valid_schedule)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = rev(valid_schedule)), "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(1.5, 4)), "dose_schedule"
  )
})

test_that("imugap_options coerces dose_schedule to integer", {
  opts <- imugap_options(dose_schedule = c(2, 5, 7))
  expect_type(opts$dose_schedule, "integer")
  expect_equal(opts$dose_schedule, c(2L, 5L, 7L))
})

# --- validation: stan_options ------------------------------------------------

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
