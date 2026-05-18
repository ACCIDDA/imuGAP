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
