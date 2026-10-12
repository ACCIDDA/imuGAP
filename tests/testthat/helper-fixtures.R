make_test_locs <- function() {
  data.frame(
    loc_id = c("state", "cnty1", "cnty2", "schl1", "schl2"),
    parent_id = c(NA, "state", "state", "cnty1", "cnty1")
  )
}

make_test_locs_implicit_root <- function() {
  data.frame(
    loc_id = c("a", "b", "c", "d"),
    parent_id = c("root", "root", "a", "a")
  )
}

make_test_obs <- function() {
  data.frame(
    obs_id = c("o1", "o2"),
    positive = c(5L, 10L),
    sample_n = c(10L, 20L)
  )
}

make_test_pops <- function() {
  data.frame(
    obs_id = c("o1", "o2"),
    loc_id = c("schl1", "schl2"),
    cohort = c(1L, 1L),
    age = c(2L, 2L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )
}

#' Generate a regex pattern from an sprintf-style or named template string
#'
#' @param template format string (e.g. `ERR_*` or `MSG_*` constant).
#' @param ... optional slot values (named or positional); unsupplied, `NULL`, or `NA`
#'   slots match wildcards.
#'
#' @return a regular expression character string.
err_pattern <- function(template, ...) {
  args <- list(...)
  re_escape <- function(x) gsub("([.\\\\|()[{\\^$*+?])", "\\\\\\1", x)

  # Check if template has {name} placeholders
  has_braces <- grepl("\\{[a-zA-Z0-9_.]+\\}", template)

  if (has_braces) {
    m <- gregexpr("\\{[a-zA-Z0-9_.]+\\}", template)
    matches <- regmatches(template, m)[[1]]
    var_names <- gsub("^\\{|\\}$", "", matches)

    starts <- m[[1]]
    ends <- starts + attr(m[[1]], "match.length") - 1L

    lit_starts <- c(1L, ends + 1L)
    lit_ends <- c(starts - 1L, nchar(template))
    escaped_lits <- re_escape(substring(template, lit_starts, lit_ends))

    arg_names <- names(args)
    spec_patterns <- character(length(matches))

    for (i in seq_along(matches)) {
      vname <- var_names[i]
      arg <- if (!is.null(arg_names) && vname %in% arg_names) {
        args[[vname]]
      } else if (
        i <= length(args) && (is.null(arg_names) || !nzchar(arg_names[i]))
      ) {
        args[[i]]
      } else {
        NA
      }

      if (is.null(arg) || (length(arg) == 1L && is.na(arg))) {
        spec_patterns[i] <- ".*?"
      } else {
        spec_patterns[i] <- re_escape(toString(arg))
      }
    }

    res <- character(length(escaped_lits) + length(spec_patterns))
    res[seq(1, length(res), by = 2)] <- escaped_lits
    res[seq(2, length(res), by = 2)] <- spec_patterns
    return(paste0(res, collapse = ""))
  }

  # Fall back to sprintf format specifiers (%s, %d, etc.)
  m <- gregexpr("%[-+0-9.]*[a-zA-Z%]", template)
  matches <- regmatches(template, m)[[1]]

  if (length(matches) == 0L || (length(matches) == 1L && matches == "%%")) {
    return(re_escape(template))
  }

  starts <- m[[1]]
  ends <- starts + attr(m[[1]], "match.length") - 1L

  lit_starts <- c(1L, ends + 1L)
  lit_ends <- c(starts - 1L, nchar(template))
  escaped_lits <- re_escape(substring(template, lit_starts, lit_ends))

  spec_patterns <- character(length(matches))
  for (i in seq_along(matches)) {
    spec <- matches[i]
    if (spec == "%%") {
      spec_patterns[i] <- "%"
      next
    }
    arg <- if (i <= length(args)) args[[i]] else NA
    if (is.null(arg) || (length(arg) == 1L && is.na(arg))) {
      type <- substr(spec, nchar(spec), nchar(spec))
      spec_patterns[i] <- switch(
        type,
        "d" = ,
        "i" = "-?[0-9]+",
        "s" = ".+",
        "f" = ,
        "g" = "[-+]?[0-9]*\\.?[0-9]+",
        ".+"
      )
    } else {
      spec_patterns[i] <- re_escape(sprintf(spec, arg))
    }
  }

  res <- character(length(escaped_lits) + length(spec_patterns))
  res[seq(1, length(res), by = 2)] <- escaped_lits
  res[seq(2, length(res), by = 2)] <- spec_patterns
  paste0(res, collapse = "")
}

#' Extract and evaluate diagnostic expression embedded in error message
#'
#' @param err error condition object.
#' @param env list or environment containing the data object(s).
#' @return the result of evaluating the extracted `subset(...)` expression.
eval_err_diagnostic <- function(err, env) {
  m <- regmatches(err$message, regexpr("`subset\\([^`]+\\)`", err$message))
  if (length(m) == 0L) {
    stop("No diagnostic `subset(...)` found in error message")
  }
  expr_text <- gsub("^`|`$", "", m)
  eval(parse(text = expr_text), envir = env)
}

#' Verify that an imuGAP fit object from a smoke test meets all expectations
#'
#' @param fit an object of class `imugap_fit`.
#' @param obs canonicalized observations data.table.
#' @param pop canonicalized populations data.table.
#' @param locs canonicalized locations data.table.
#' @param backend character; `"rstan"` or `"cmdstanr"`.
verify_imugap_smoke_fit <- function(
  fit,
  obs,
  pop,
  locs,
  backend = c("rstan", "cmdstanr")
) {
  backend <- match.arg(backend)
  expect_s3_class(fit, "imugap_fit")

  if (backend == "rstan") {
    expect_s4_class(fit$raw_fit, "stanfit")
    saved_pars <- fit$raw_fit@sim$pars_oi
  } else {
    expect_s3_class(fit$raw_fit, "CmdStanMCMC")
    saved_pars <- fit$raw_fit$metadata()$stan_variables
  }

  # Essential model parameters must always be present
  expected_pars <- c(
    "beta_bs",
    "lambda_raw",
    "raw_phi_root",
    "off_layer",
    "mu_offset"
  )
  for (par in expected_pars) {
    expect_true(par %in% saved_pars, info = paste("missing parameter:", par))
  }

  # Model-block locals are never saved in any backend
  model_locals <- c("phi", "raw_phi_and_mu")
  for (par in model_locals) {
    expect_false(
      par %in% saved_pars,
      info = paste("parameter should be absent:", par)
    )
  }

  # Dropped parameters: rstan supports drop_pars so z_layer and raw_phi_mat
  # are excluded from saved draws.
  if (backend == "rstan") {
    for (par in c("raw_phi_mat", "z_layer")) {
      expect_false(
        par %in% saved_pars,
        info = paste("parameter should be absent:", par)
      )
    }
  }

  # Check parameter dimensions
  n_parent <- length(unique(locs$parent_id[!is.na(locs$parent_id)]))
  n_cohort <- length(unique(pop$cohort))
  n_locs <- nrow(locs)

  if (backend == "rstan") {
    off_layer_draws <- as.matrix(fit$raw_fit, pars = "off_layer")
    n_off_cols <- ncol(off_layer_draws)
    mu_offset_draws <- as.matrix(fit$raw_fit, pars = "mu_offset")
    n_mu_cols <- ncol(mu_offset_draws)
    beta_draws <- as.matrix(fit$raw_fit, pars = "beta_bs")
  } else {
    off_layer_draws <- fit$raw_fit$draws(variables = "off_layer")
    n_off_cols <- dim(off_layer_draws)[3]
    mu_offset_draws <- fit$raw_fit$draws(variables = "mu_offset")
    n_mu_cols <- dim(mu_offset_draws)[3]
    beta_draws <- fit$raw_fit$draws(variables = "beta_bs")
  }

  expect_equal(n_off_cols, n_locs - 1L)
  expect_equal(n_mu_cols, n_cohort * n_parent)
  expect_true(all(is.finite(beta_draws)))

  # Downstream predict()
  target_pops <- data.table::copy(pop)[!duplicated(obs_id)]
  target_pops[, weight := 1.0]
  preds <- suppressWarnings(predict(fit, target_pops, posterior_size = 10))
  expect_s3_class(preds, "imugap_predict")
  expect_true(is.array(preds$draws))
  expect_equal(dim(preds$draws)[2], 1L)

  # Downstream compute_phi()
  phi_arr <- suppressWarnings(compute_phi(fit, posterior_size = 10))
  expect_equal(dim(phi_arr), c(10L, n_cohort, n_locs))
  expect_true(all(phi_arr >= 0 & phi_arr <= 1))
  eta_arr <- suppressWarnings(
    compute_phi(fit, posterior_size = 10, link_scale = TRUE)
  )
  expect_equal(phi_arr, stats::plogis(eta_arr))
}
