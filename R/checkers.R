# Internal error message format strings for checkers.R
ERR_MUST_BE_INTEGER   <- "`%s` column '%s' must contain integers"
ERR_CANNOT_HAVE_NA    <- "`%s` column '%s' cannot contain NA values"
ERR_MUST_BE_GT_ZERO   <- "`%s` column '%s' must contain values > 0"
ERR_MUST_BE_GTE_ZERO  <- "`%s` column '%s' must contain values >= 0"
ERR_MUST_BE_LTE_MAX   <- "`%s` column '%s' must contain values <= %d"
ERR_SET_EQUIV_MISSING <- "`%s` column '%s' is missing required set values"
ERR_SET_EQUIV_EXTRA   <- "`%s` column '%s' contains values outside permitted set"
ERR_SUBSET_MISSING    <- "`%s` column '%s' contains values not in parent set: missing %s"
ERR_MISSING_COLS      <- "`%s` is missing required column(s): %s"
ERR_MUST_BE_NUMERIC   <- "`%s` column '%s' must contain numeric values"
MSG_EXTRA_COLS        <- "`%s` contains unexpected extra column(s): %s"

ERR_ARG_MUST_BE_NUMERIC <- "'%s' must be numeric"
ERR_ARG_MIN_LENGTH      <- "length('%s') must be >= 1"
ERR_ARG_CANNOT_HAVE_NA  <- "'%s' may not contain NAs"
ERR_ARG_MUST_BE_INTEGER <- "'%s' must be integers"
ERR_ARG_MUST_BE_GT_ZERO <- "'%s' must be positive"

#' Signal an error if a condition is met with formatted message
#'
#' @param cond Logical expression to evaluate.
#' @param fmt Character format string for [sprintf()].
#' @param ... Additional arguments passed to [sprintf()].
#' @param n Frame offset integer specifying call stack depth for call attribution
#'   (default `1L`). If `n <= 0L`, call attribution is suppressed (`NULL`).
#'
#' @keywords internal
stop_fmt_if <- function(cond, fmt, ..., n = 1L) {
  if (isTRUE(cond)) {
    call_obj <- if (n > 0L) sys.call(-n) else NULL
    stop(simpleError(sprintf(fmt, ...), call = call_obj))
  }
}

#' Signal a warning if a condition is met with formatted message
#'
#' @param cond Logical expression to evaluate.
#' @param fmt Character format string for [sprintf()].
#' @param ... Additional arguments passed to [sprintf()].
#' @param n Frame offset integer specifying call stack depth for call attribution
#'   (default `1L`). If `n <= 0L`, call attribution is suppressed (`NULL`).
#'
#' @return Logical scalar indicating whether `cond` evaluated to `TRUE`.
#'
#' @keywords internal
warn_fmt_if <- function(cond, fmt, ..., n = 1L) {
  cond_val <- isTRUE(cond)
  if (cond_val) {
    call_obj <- if (n > 0L) sys.call(-n) else NULL
    warning(simpleWarning(sprintf(fmt, ...), call = call_obj))
  }
  cond_val
}

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
#' @importFrom data.table setDT
#' @importFrom data.table as.data.table
assert_dt_able <- function(dt, copy = FALSE) {
  if (copy) as.data.table(dt) else setDT(dt)
}

#' @keywords internal
assert_cols <- function(dt, cols, warn_extra = FALSE, n = 1L) {
  missing_cols <- setdiff(cols, names(dt))
  stop_fmt_if(
    length(missing_cols) > 0,
    ERR_MISSING_COLS,
    deparse(substitute(dt)),
    toString(missing_cols),
    n = n + 1L
  )
  if (warn_extra) {
    extra_cols <- setdiff(names(dt), cols)
    warn_fmt_if(
      length(extra_cols) > 0,
      MSG_EXTRA_COLS,
      deparse(substitute(dt)),
      toString(extra_cols)
    )
  }
  dt[]
}

#' @keywords internal
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

#' @keywords internal
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
