# Tests for imugap_options()

test_that("imugap_options returns expected structure with defaults", {
  defaults <- imugap_options()
  expect_s3_class(defaults, "imugap_options")
  expect_type(defaults, "list")
  expect_setequal(
    names(defaults),
    c(
      "df",
      "dose_schedule",
      "compute_log_lik",
      "time",
      "offsets",
      "link",
      "model",
      "solver",
      "solver_type"
    )
  )
  expect_equal(defaults$df, 5L)
  expect_equal(defaults$dose_schedule, c(1, 4))
  expect_false(defaults$compute_log_lik)
  expect_equal(defaults$time, "bspline")
  expect_equal(defaults$offsets, "static")
  expect_equal(defaults$link, "logit")
  expect_equal(defaults$model, "bspline_static_zero_direct_logit")
  expect_equal(defaults$solver, "direct")
  expect_equal(defaults$solver_type, 1L)
})

test_that("imugap_options df can be overridden", {
  defaults <- imugap_options()
  override_df <- 10L
  opts <- imugap_options(df = override_df)
  expected <- defaults
  expected$df <- override_df
  expect_equal(opts, expected)
})

test_that("imugap_options dose_schedule can be overridden", {
  defaults <- imugap_options()
  override_sched <- c(2, 5, 7)
  opts <- imugap_options(dose_schedule = override_sched)
  expected <- defaults
  expected$dose_schedule <- override_sched
  expect_equal(opts, expected)
})

test_that("imugap_options errors on unknown link / model / offsets", {
  expect_error(
    imugap_options(link = "unknown_link"),
    "should be"
  )
  expect_error(
    imugap_options(model = "unknown_model"),
    err_pattern(ERR_OPT_UNKNOWN_MODEL, model = "unknown_model")
  )
  expect_error(
    imugap_options(link = "stateonly"),
    "should be"
  )
  expect_error(
    imugap_options(offsets = "single"),
    "should be"
  )
  expect_error(
    imugap_options(offsets = "unknown_offset"),
    "should be"
  )
})

test_that("imugap_options accepts default, logit, and probit link options", {
  defaults <- imugap_options()
  opts_default <- imugap_options(link = "default")
  expect_equal(opts_default, defaults)

  opts_logit <- imugap_options(link = "logit")
  expect_equal(opts_logit$link, "logit")
  expect_equal(opts_logit$model, "bspline_static_zero_direct_logit")

  opts_probit <- imugap_options(link = "probit")
  expect_equal(opts_probit$link, "probit")
  expect_equal(opts_probit$model, "bspline_static_zero_direct_probit")

  # Backwards compatibility with model parameter
  opts_model_probit <- imugap_options(model = "probit")
  expect_equal(opts_model_probit$link, "probit")
  expect_equal(opts_model_probit$model, "bspline_static_zero_direct_probit")

  # Full model template name
  opts_model_full <- imugap_options(model = "bspline_static_zero_direct_probit")
  expect_equal(opts_model_full$link, "probit")
  expect_equal(opts_model_full$model, "bspline_static_zero_direct_probit")

  # Legacy model alias
  opts_model_legacy <- imugap_options(model = "bspline_static_probit")
  expect_equal(opts_model_legacy$model, "bspline_static_zero_direct_probit")
})

test_that("imugap_options accepts numeric whole-number df", {
  defaults <- imugap_options()
  override_df <- 4
  opts <- imugap_options(df = override_df)
  expected <- defaults
  expected$df <- as.integer(override_df)
  expect_equal(opts, expected)
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
    imugap_options(dose_schedule = -valid_schedule),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(0, valid_schedule)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(valid_schedule, NA)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = numeric(0)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = as.character(valid_schedule)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = rev(valid_schedule)),
    "dose_schedule"
  )
  expect_error(
    imugap_options(dose_schedule = c(1.5, 4)),
    "dose_schedule"
  )
})

test_that("imugap_options rejects invalid compute_log_lik", {
  expect_error(
    imugap_options(compute_log_lik = "TRUE"),
    err_pattern(ERR_OPT_COMPUTE_LOG_LIK)
  )
  expect_error(
    imugap_options(compute_log_lik = c(TRUE, FALSE)),
    err_pattern(ERR_OPT_COMPUTE_LOG_LIK)
  )
  expect_error(
    imugap_options(compute_log_lik = NA),
    err_pattern(ERR_OPT_COMPUTE_LOG_LIK)
  )
})

test_that("imugap_options accepts valid solver values including default", {
  opts_def <- imugap_options(solver = "default")
  expect_equal(opts_def$solver, "direct")
  expect_equal(opts_def$solver_type, 1L)

  opts_halley2 <- imugap_options(solver = "halley2")
  expect_equal(opts_halley2$solver, "halley2")
  expect_equal(opts_halley2$solver_type, 2L)

  opts_newton2 <- imugap_options(solver = "newton2")
  expect_equal(opts_newton2$solver, "newton2")
  expect_equal(opts_newton2$solver_type, 3L)

  opts_builtin <- imugap_options(solver = "builtin")
  expect_equal(opts_builtin$solver, "builtin")
  expect_equal(opts_builtin$solver_type, 4L)

  opts_halley10 <- imugap_options(solver = "halley10")
  expect_equal(opts_halley10$solver, "halley10")
  expect_equal(opts_halley10$solver_type, 5L)
})

test_that("imugap_options rejects invalid solver values", {
  expect_error(imugap_options(solver = "unknown_solver"), "should be")
  expect_error(
    imugap_options(solver = 0L),
    "must be NULL or a character vector"
  )
  expect_error(
    imugap_options(solver = 2L),
    "must be NULL or a character vector"
  )
  expect_error(
    imugap_options(solver = 6L),
    "must be NULL or a character vector"
  )
  expect_error(
    imugap_options(solver = 2.5),
    "must be NULL or a character vector"
  )
  expect_error(
    imugap_options(solver = c("halley2", "direct")),
    "must be of length 1"
  )
})

test_that("resolve_model_name resolves defaults and multi-layer model specs", {
  res_def <- resolve_model_name()
  expect_equal(res_def$model_name, "bspline_static_zero_direct_logit")
  expect_equal(res_def$time, "bspline")
  expect_equal(res_def$offsets, "static")
  expect_equal(res_def$solver, "direct")
  expect_equal(res_def$solver_module, "direct")
  expect_equal(res_def$solver_type, 1L)
  expect_equal(res_def$link, "logit")
  expect_false(res_def$is_dispatch)

  # default_default_default_* template resolution
  res_tmpl_logit <- resolve_model_name(model = "default_default_default_logit")
  expect_equal(res_tmpl_logit$model_name, "bspline_static_zero_direct_logit")

  res_tmpl_probit <- resolve_model_name(
    model = "default_default_default_probit"
  )
  expect_equal(res_tmpl_probit$model_name, "bspline_static_zero_direct_probit")
  expect_equal(res_tmpl_probit$link, "probit")

  # Character vector input
  res_vec <- resolve_model_name(
    model = c("default", "default", "default", "probit")
  )
  expect_equal(res_vec$model_name, "bspline_static_zero_direct_probit")

  # Legacy model strings
  res_leg_logit <- resolve_model_name(model = "bspline_static_logit")
  expect_equal(res_leg_logit$model_name, "bspline_static_zero_direct_logit")

  res_leg_probit <- resolve_model_name(model = "bspline_static_probit")
  expect_equal(res_leg_probit$model_name, "bspline_static_zero_direct_probit")

  # Single shortcuts
  expect_equal(
    resolve_model_name(model = "default")$model_name,
    "bspline_static_zero_direct_logit"
  )
  expect_equal(
    resolve_model_name(model = "logit")$model_name,
    "bspline_static_zero_direct_logit"
  )
  expect_equal(
    resolve_model_name(model = "probit")$model_name,
    "bspline_static_zero_direct_probit"
  )
})

test_that("resolve_model_name resolves dynamic dispatch solvers", {
  res_h2 <- resolve_model_name(solver = "halley2")
  expect_equal(res_h2$model_name, "bspline_static_zero_dispatch_logit")
  expect_equal(res_h2$solver, "halley2")
  expect_equal(res_h2$solver_module, "dispatch")
  expect_equal(res_h2$solver_type, 2L)
  expect_true(res_h2$is_dispatch)

  res_n2_probit <- resolve_model_name(
    solver = "newton2",
    link = "probit"
  )
  expect_equal(res_n2_probit$model_name, "bspline_static_zero_dispatch_probit")
  expect_equal(res_n2_probit$solver, "newton2")
  expect_equal(res_n2_probit$solver_type, 3L)
  expect_equal(res_n2_probit$link, "probit")
  expect_true(res_n2_probit$is_dispatch)
})

test_that("resolve_model_name validates unknown options and models", {
  expect_error(
    resolve_model_name(offsets = "single"),
    err_pattern(ERR_OPT_UNKNOWN_MODEL, model = "single")
  )
  expect_error(
    resolve_model_name(offsets = "unknown_offset"),
    err_pattern(ERR_OPT_UNKNOWN_MODEL, model = "unknown_offset")
  )
  expect_error(
    resolve_model_name(model = "unknown_model_name"),
    err_pattern(ERR_OPT_UNKNOWN_MODEL, model = "unknown_model_name")
  )
  expect_error(
    resolve_model_name(time = "invalid_time"),
    err_pattern(ERR_OPT_UNKNOWN_MODEL, model = "invalid_time")
  )
  expect_error(
    resolve_model_name(link = "invalid_link"),
    err_pattern(ERR_OPT_UNKNOWN_MODEL, model = "invalid_link")
  )
  expect_error(
    resolve_model_name(solver = "unknown_solver"),
    err_pattern(ERR_OPT_UNKNOWN_SOLVER, solver = "unknown_solver")
  )
  expect_error(
    resolve_model_name(solver = 0L),
    err_pattern(ERR_OPT_UNKNOWN_SOLVER, solver = "0")
  )
  expect_error(
    resolve_model_name(solver = 2L),
    err_pattern(ERR_OPT_UNKNOWN_SOLVER, solver = "2")
  )
  expect_error(
    resolve_model_name(solver = 6L),
    err_pattern(ERR_OPT_UNKNOWN_SOLVER, solver = "6")
  )
})

test_that("resolve_layer_model handles multilayer vs single-layer conversion", {
  # Default multi-layer
  res_multi <- resolve_layer_model(imugap_options(), is_multilayer = TRUE)
  expect_equal(res_multi, "bspline_static_zero_direct_logit")

  # Single-layer conversion
  res_single <- resolve_layer_model(imugap_options(), is_multilayer = FALSE)
  expect_equal(res_single, "bspline_single_logit")

  # Single-layer with probit
  res_single_probit <- resolve_layer_model(
    imugap_options(link = "probit"),
    is_multilayer = FALSE
  )
  expect_equal(res_single_probit, "bspline_single_probit")

  # Solver ignored in single-layer conversion (always <time>_single_<link>)
  res_h2_single <- resolve_layer_model(
    imugap_options(solver = "halley2"),
    is_multilayer = FALSE
  )
  expect_equal(res_h2_single, "bspline_single_logit")

  # Solver retained in multi-layer
  res_h2_multi <- resolve_layer_model(
    imugap_options(solver = "halley2"),
    is_multilayer = TRUE
  )
  expect_equal(res_h2_multi, "bspline_static_zero_dispatch_logit")

  # Type assertion
  expect_error(
    resolve_layer_model("not_an_imugap_options"),
    err_pattern(ERR_IMUGAP_OPTS_CLASS)
  )
  expect_error(
    resolve_layer_model(list(df = 5L)),
    err_pattern(ERR_IMUGAP_OPTS_CLASS)
  )
})
