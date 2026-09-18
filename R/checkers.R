# Internal error message format strings for checkers.R
ERR_MUST_BE_INTEGER <- "`%s` column '%s' must contain integers"
ERR_CANNOT_HAVE_NA <- "`%s` column '%s' cannot contain NA values"
ERR_MUST_BE_GT_ZERO <- "`%s` column '%s' must contain values > 0"
ERR_MUST_BE_GTE_ZERO <- "`%s` column '%s' must contain values >= 0"
ERR_MUST_BE_LTE_MAX <- "`%s` column '%s' must contain values <= %d"
ERR_SET_EQUIV_MISSING <- "`%s` column '%s' is missing required set values"
ERR_SET_EQUIV_EXTRA <- "`%s` column '%s' contains values outside permitted set"
ERR_SUBSET_MISSING <- "`%s` column '%s' contains values not in parent set: missing %s"
ERR_MISSING_COLS <- "`%s` is missing required column(s): %s"
ERR_MUST_BE_NUMERIC <- "`%s` column '%s' must contain numeric values"
MSG_EXTRA_COLS <- "`%s` contains unexpected extra column(s): %s"

ERR_ARG_MUST_BE_NUMERIC <- "'%s' must be numeric"
ERR_ARG_MIN_LENGTH <- "`length(%s)` must be >= 1"
ERR_ARG_CANNOT_HAVE_NA <- "'%s' may not contain NAs"
ERR_ARG_MUST_BE_INTEGER <- "'%s' must be integers"
ERR_ARG_MUST_BE_GT_ZERO <- "'%s' must be positive"

# Shared object and option validation format strings
ERR_NOT_IMUGAP_FIT <- "`%s` must be an object of class 'imugap_fit'"
ERR_NOT_IMUGAP_PREDICT <- "`%s` must be an object of class 'imugap_predict'"
ERR_OPT_UNKNOWN_MODEL <- "`imugap_opts` unknown model '%s'"
ERR_STAN_OPTS_CLASS <- "`stan_opts` must be created by stan_options()"

`%||%` <- function(x, y) if (is.null(x)) y else x

#' @title Signal an error if a condition is met with formatted message
#'
#' @description
#' Evaluates `cond` and if `TRUE`, raises an error formatted with [sprintf()].
#'
#' @param cond logical expression to evaluate.
#' @param fmt character format string for [sprintf()].
#' @param ... additional arguments passed to [sprintf()].
#' @param n frame offset integer specifying call stack depth for call attribution
#'   (default: `1L`). If `n <= 0L`, call attribution is suppressed (`NULL`).
#'
#' @keywords internal
#' @noRd
stop_fmt_if <- function(cond, fmt, ..., n = 1L) {
  if (isTRUE(cond)) {
    call_obj <- if (n > 0L) sys.call(-n) else NULL
    stop(simpleError(sprintf(fmt, ...), call = call_obj))
  }
}

#' @title Signal a warning if a condition is met with formatted message
#'
#' @description
#' Evaluates `cond` and if `TRUE`, signals a warning formatted with [sprintf()].
#'
#' @param cond logical expression to evaluate.
#' @param fmt character format string for [sprintf()].
#' @param ... additional arguments passed to [sprintf()].
#' @param n frame offset integer specifying call stack depth for call attribution
#'   (default: `1L`). If `n <= 0L`, call attribution is suppressed (`NULL`).
#'
#' @return a logical scalar, indicating whether `cond` evaluated to `TRUE`.
#'
#' @keywords internal
#' @noRd
warn_fmt_if <- function(cond, fmt, ..., n = 1L) {
  cond_val <- isTRUE(cond)
  if (cond_val) {
    call_obj <- if (n > 0L) sys.call(-n) else NULL
    warning(simpleWarning(sprintf(fmt, ...), call = call_obj))
  }
  cond_val
}

#' @title Assert and coerce column to integer
#'
#' @description
#' Verifies that column `x` in `dt` can be represented as integer and coerces if needed.
#'
#' @param dt a `[data.table()]`.
#' @param x column name as character string.
#' @param na_allowed logical; allow `NA` values? (default: `FALSE`).
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @keywords internal
#' @noRd
assert_as_integer <- function(dt, x, na_allowed = FALSE, n = 1L) {
  if (dt[, !is.integer(get(x))]) {
    stop_fmt_if(
      !all(as.integer(dt[, get(x)]) == dt[, get(x)]),
      ERR_MUST_BE_INTEGER,
      deparse(substitute(dt)),
      x,
      n = n + 1L
    )
    expr <- parse(text = sprintf("%s := as.integer(%s)", x, x))
    dt[, eval(expr)]
  }
  stop_fmt_if(
    !na_allowed && dt[, any(is.na(get(x)))],
    ERR_CANNOT_HAVE_NA,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  dt[]
}

#' @title Assert column contains positive integers
#'
#' @description
#' Verifies that column `x` in `dt` contains integers strictly greater than 0.
#'
#' @param dt a `[data.table()]`.
#' @param x column name as character string.
#' @param na_allowed logical; allow `NA` values? (default: `FALSE`).
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @keywords internal
#' @noRd
assert_positive_integer <- function(dt, x, na_allowed = FALSE, n = 1L) {
  stop_fmt_if(
    assert_as_integer(dt, x, na_allowed, n = n + 1L)[, any(get(x) < 1L)],
    ERR_MUST_BE_GT_ZERO,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  dt[]
}

#' @title Assert column contains non-negative integers
#'
#' @description
#' Verifies that column `x` in `dt` contains integers greater than or equal to 0.
#'
#' @param dt a `[data.table()]`.
#' @param x column name as character string.
#' @param na_allowed logical; allow `NA` values? (default: `FALSE`).
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @keywords internal
#' @noRd
assert_nonneg_integer <- function(dt, x, na_allowed = FALSE, n = 1L) {
  stop_fmt_if(
    assert_as_integer(dt, x, na_allowed, n = n + 1L)[, any(get(x) < 0L)],
    ERR_MUST_BE_GTE_ZERO,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  dt[]
}

#' @title Assert column contains positive integers with upper bound
#'
#' @description
#' Verifies that column `x` in `dt` contains positive integers less than or equal to `max`.
#'
#' @param dt a `[data.table()]`.
#' @param x column name as character string.
#' @param max optional maximum integer limit.
#' @param na_allowed logical; allow `NA` values? (default: `FALSE`).
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @keywords internal
#' @noRd
assert_maxed_pos_integer <- function(dt, x, max, na_allowed = FALSE, n = 1L) {
  assert_positive_integer(dt, x, na_allowed, n = n + 1L)
  stop_fmt_if(
    !missing(max) && dt[, any(get(x) > max)],
    ERR_MUST_BE_LTE_MAX,
    deparse(substitute(dt)),
    x,
    max,
    n = n + 1L
  )
  dt[]
}

#' @title Assert column contains exact set equivalence
#'
#' @description
#' Verifies that unique values of column `x` in `dt` exactly match the set `tarset`.
#'
#' @param dt a `[data.table()]`.
#' @param x column name as character string.
#' @param tarset expected set vector.
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @keywords internal
#' @noRd
assert_set_equivalence <- function(dt, x, tarset, n = 1L) {
  tarset <- unique(tarset)
  setlen <- length(tarset)
  stop_fmt_if(
    length(intersect(tarset, dt[, get(x)])) != setlen,
    ERR_SET_EQUIV_MISSING,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  stop_fmt_if(
    length(union(tarset, dt[, get(x)])) != setlen,
    ERR_SET_EQUIV_EXTRA,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  dt[]
}

#' @title Assert column contains a subset of allowed values
#'
#' @description
#' Verifies that all values of column `x` in `dt` are contained within `tarset`.
#'
#' @param dt a `[data.table()]`.
#' @param x column name as character string.
#' @param tarset allowed superset vector.
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @keywords internal
#' @noRd
assert_subset <- function(dt, x, tarset, n = 1L) {
  checkset <- unique(dt[, get(x)])
  stop_fmt_if(
    !all(checkset %in% tarset),
    ERR_SUBSET_MISSING,
    deparse(substitute(dt)),
    x,
    toString(setdiff(checkset, tarset)),
    n = n + 1L
  )
  dt[]
}

#' @title Assert object can be converted to data.table
#'
#' @description
#' Converts or checks table conversion to `data.table`.
#'
#' @param dt a `[data.frame()]`.
#' @param copy logical; create a copy via [data.table::as.data.table()]?
#'   (default: `FALSE`).
#'
#' @keywords internal
#' @noRd
#' @importFrom data.table setDT
#' @importFrom data.table as.data.table
assert_dt_able <- function(dt, copy = FALSE) {
  if (copy) as.data.table(dt) else setDT(dt)
}

#' @title Assert required columns are present
#'
#' @description
#' Verifies that table `dt` contains all required columns in `cols`.
#'
#' @param dt a `[data.table()]`.
#' @param cols character vector of required column names.
#' @param warn_extra logical; warn if extra columns are present? (default: `FALSE`).
#' @param allowed_extra character vector of extra columns exempt from warnings
#'   (default: `character(0)`).
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @keywords internal
#' @noRd
assert_cols <- function(
  dt,
  cols,
  warn_extra = FALSE,
  allowed_extra = character(0),
  n = 1L
) {
  missing_cols <- setdiff(cols, names(dt))
  stop_fmt_if(
    length(missing_cols) > 0,
    ERR_MISSING_COLS,
    deparse(substitute(dt)),
    toString(missing_cols),
    n = n + 1L
  )
  if (warn_extra) {
    extra_cols <- setdiff(names(dt), c(cols, allowed_extra))
    warn_fmt_if(
      length(extra_cols) > 0,
      MSG_EXTRA_COLS,
      deparse(substitute(dt)),
      toString(extra_cols)
    )
  }
  dt[]
}

#' @title Assert column contains positive numeric values
#'
#' @description
#' Verifies that column `x` in `dt` is numeric, non-NA, and strictly greater than 0.
#'
#' @param dt a `[data.table()]`.
#' @param x column name as character string.
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @keywords internal
#' @noRd
assert_positive_numeric <- function(dt, x, n = 1L) {
  stop_fmt_if(
    dt[, !is.numeric(get(x))],
    ERR_MUST_BE_NUMERIC,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  stop_fmt_if(
    dt[, any(is.na(get(x)))],
    ERR_CANNOT_HAVE_NA,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  stop_fmt_if(
    dt[, any(get(x) <= 0)],
    ERR_MUST_BE_GT_ZERO,
    deparse(substitute(dt)),
    x,
    n = n + 1L
  )
  dt[]
}

#' @title Assert argument is a positive integer vector
#'
#' @description
#' Verifies that `val` is numeric, non-empty, non-NA, integer-valued, and strictly positive.
#'
#' @param val numeric vector to validate.
#' @param name argument name for error messages.
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#'
#' @return an integer vector, the validated positive integer vector.
#'
#' @keywords internal
#' @noRd
assert_positive_int <- function(val, name, n = 1L) {
  stop_fmt_if(!is.numeric(val), ERR_ARG_MUST_BE_NUMERIC, name, n = n + 1L)
  stop_fmt_if(length(val) < 1L, ERR_ARG_MIN_LENGTH, name, n = n + 1L)
  stop_fmt_if(any(is.na(val)), ERR_ARG_CANNOT_HAVE_NA, name, n = n + 1L)
  stop_fmt_if(
    any(val != as.integer(val)),
    ERR_ARG_MUST_BE_INTEGER,
    name,
    n = n + 1L
  )
  stop_fmt_if(any(val < 1L), ERR_ARG_MUST_BE_GT_ZERO, name, n = n + 1L)
  as.integer(val)
}
