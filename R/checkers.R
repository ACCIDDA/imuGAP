`%||%` <- function(x, y) if (is.null(x)) y else x


#' @title Assert and coerce column to integer
#'
#' @description
#' Verifies that column `x` in `dt` can be represented as integer and coerces if needed.
#'
#' @param dt a `[data.table()]`.
#' @param x column name as character string.
#' @param na_allowed logical; allow `NA` values? (default: `FALSE`).
#' @param n frame offset integer specifying call stack depth for error attribution (default: `1L`).
#' @param name table name for error messages (default: the expression passed as `dt`).
#'
#' @keywords internal
#' @noRd
assert_as_integer <- function(
  dt,
  x,
  na_allowed = FALSE,
  n = 1L,
  name = deparse(substitute(dt))
) {
  if (dt[, !is.integer(get(x))]) {
    non_int <- dt[, which(as.integer(get(x)) != get(x))]
    stop_fmt_if(
      length(non_int) > 0L,
      ERR_MUST_BE_INTEGER,
      name = name,
      col = x,
      rows = non_int,
      n = n + 1L
    )
    expr <- parse(text = sprintf("%s := as.integer(%s)", x, x))
    dt[, eval(expr)]
  }
  stop_fmt_if(
    !na_allowed && dt[, any(is.na(get(x)))],
    ERR_CANNOT_HAVE_NA,
    name = name,
    col = x,
    rows = dt[, which(is.na(get(x)))],
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
#' @param name table name for error messages (default: the expression passed as `dt`).
#'
#' @keywords internal
#' @noRd
assert_positive_integer <- function(
  dt,
  x,
  na_allowed = FALSE,
  n = 1L,
  name = deparse(substitute(dt))
) {
  bad <- assert_as_integer(dt, x, na_allowed, n = n + 1L, name = name)[,
    which(get(x) < 1L)
  ]
  stop_fmt_if(
    length(bad) > 0L,
    ERR_MUST_BE_GT_ZERO,
    name = name,
    col = x,
    rows = bad,
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
#' @param name table name for error messages (default: the expression passed as `dt`).
#'
#' @keywords internal
#' @noRd
assert_nonneg_integer <- function(
  dt,
  x,
  na_allowed = FALSE,
  n = 1L,
  name = deparse(substitute(dt))
) {
  bad <- assert_as_integer(dt, x, na_allowed, n = n + 1L, name = name)[,
    which(get(x) < 0L)
  ]
  stop_fmt_if(
    length(bad) > 0L,
    ERR_MUST_BE_GTE_ZERO,
    name = name,
    col = x,
    rows = bad,
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
#' @param name table name for error messages (default: the expression passed as `dt`).
#'
#' @keywords internal
#' @noRd
assert_maxed_pos_integer <- function(
  dt,
  x,
  max,
  na_allowed = FALSE,
  n = 1L,
  name = deparse(substitute(dt))
) {
  assert_positive_integer(dt, x, na_allowed, n = n + 1L, name = name)
  stop_fmt_if(
    !missing(max) && dt[, any(get(x) > max)],
    ERR_MUST_BE_LTE_MAX,
    name = name,
    col = x,
    max_val = max,
    rows = if (!missing(max)) dt[, which(get(x) > max)],
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
#' @param name table name for error messages (default: the expression passed as `dt`).
#'
#' @keywords internal
#' @noRd
assert_set_equivalence <- function(
  dt,
  x,
  tarset,
  n = 1L,
  name = deparse(substitute(dt))
) {
  tarset <- unique(tarset)
  setlen <- length(tarset)
  stop_fmt_if(
    length(intersect(tarset, dt[, get(x)])) != setlen,
    ERR_SET_EQUIV_MISSING,
    name = name,
    col = x,
    n = n + 1L
  )
  stop_fmt_if(
    length(union(tarset, dt[, get(x)])) != setlen,
    ERR_SET_EQUIV_EXTRA,
    name = name,
    col = x,
    rows = dt[, which(!get(x) %in% tarset)],
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
#' @param name table name for error messages (default: the expression passed as `dt`).
#'
#' @keywords internal
#' @noRd
assert_subset <- function(
  dt,
  x,
  tarset,
  n = 1L,
  name = deparse(substitute(dt))
) {
  # a blank is reported as a blank, not as a value missing from the parent set
  stop_fmt_if(
    dt[, any(is.na(get(x)))],
    ERR_CANNOT_HAVE_NA,
    name = name,
    col = x,
    rows = dt[, which(is.na(get(x)))],
    n = n + 1L
  )
  checkset <- unique(dt[, get(x)])
  stop_fmt_if(
    !all(checkset %in% tarset),
    ERR_SUBSET_MISSING,
    name = name,
    col = x,
    missing = setdiff(checkset, tarset),
    rows = dt[, which(!get(x) %in% tarset)],
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
    name = deparse(substitute(dt)),
    missing = missing_cols,
    n = n + 1L
  )
  if (warn_extra) {
    extra_cols <- setdiff(names(dt), c(cols, allowed_extra))
    warn_fmt_if(
      length(extra_cols) > 0,
      MSG_EXTRA_COLS,
      name = deparse(substitute(dt)),
      extra = extra_cols
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
#' @param name table name for error messages (default: the expression passed as `dt`).
#'
#' @keywords internal
#' @noRd
assert_positive_numeric <- function(
  dt,
  x,
  n = 1L,
  name = deparse(substitute(dt))
) {
  stop_fmt_if(
    dt[, !is.numeric(get(x))],
    ERR_MUST_BE_NUMERIC,
    name = name,
    col = x,
    n = n + 1L
  )
  stop_fmt_if(
    dt[, any(is.na(get(x)))],
    ERR_CANNOT_HAVE_NA,
    name = name,
    col = x,
    rows = dt[, which(is.na(get(x)))],
    n = n + 1L
  )
  stop_fmt_if(
    dt[, any(get(x) <= 0)],
    ERR_MUST_BE_GT_ZERO,
    name = name,
    col = x,
    rows = dt[, which(get(x) <= 0)],
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
  stop_fmt_if(
    !is.numeric(val),
    ERR_ARG_MUST_BE_NUMERIC,
    name = name,
    n = n + 1L
  )
  stop_fmt_if(length(val) < 1L, ERR_ARG_MIN_LENGTH, name = name, n = n + 1L)
  stop_fmt_if(any(is.na(val)), ERR_ARG_CANNOT_HAVE_NA, name = name, n = n + 1L)
  stop_fmt_if(
    any(val != as.integer(val)),
    ERR_ARG_MUST_BE_INTEGER,
    name = name,
    n = n + 1L
  )
  stop_fmt_if(any(val < 1L), ERR_ARG_MUST_BE_GT_ZERO, name = name, n = n + 1L)
  as.integer(val)
}
