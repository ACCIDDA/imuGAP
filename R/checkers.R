#' @keywords internal
stopper <- function(dt, x, fmt, ...) {
  msg <- sprintf(fmt, dt, x, ...)
  stop(msg, call. = FALSE)
}

#' @keywords internal
assert_as_integer <- function(dt, x, na_allowed = FALSE) {
  if (dt[, !is.integer(get(x))]) {
    if (!all(as.integer(dt[, get(x)]) == dt[, get(x)])) {
      stopper(deparse(substitute(dt)), x, "'%s' column '%s' must be integer")
    } else {
      expr <- parse(text = sprintf("%s := as.integer(%s)", x, x))
      dt[, eval(expr)]
    }
  }
  if (!na_allowed && dt[, any(is.na(get(x)))]) {
    stopper(
      deparse(substitute(dt)),
      x,
      "'%s' column '%s' cannot have NA values"
    )
  }
  dt[]
}

#' @keywords internal
assert_positive_integer <- function(dt, x, na_allowed = FALSE) {
  if (assert_as_integer(dt, x, na_allowed)[, any(get(x) < 1L)]) {
    stopper(deparse(substitute(dt)), x, "'%s' column '%s' must all be > 0")
  }
  dt[]
}

#' @keywords internal
assert_nonneg_integer <- function(dt, x, na_allowed = FALSE) {
  if (assert_as_integer(dt, x, na_allowed)[, any(get(x) < 0L)]) {
    stopper(deparse(substitute(dt)), x, "'%s' column '%s' must all be >= 0")
  }
  dt[]
}

#' @keywords internal
assert_maxed_pos_integer <- function(dt, x, max, na_allowed = FALSE) {
  assert_positive_integer(dt, x, na_allowed)
  if (!missing(max)) {
    if (dt[, any(get(x) > max)]) {
      stopper(
        deparse(substitute(dt)),
        x,
        "'%s' column '%s' must all be <= %i",
        max
      )
    }
  }
  dt[]
}

#' @keywords internal
assert_set_equivalence <- function(dt, x, tarset) {
  tarset <- unique(tarset)
  setlen <- length(tarset)
  if (length(intersect(tarset, dt[, get(x)])) != setlen) {
    stopper(
      deparse(substitute(dt)),
      x,
      "'%s' column '%s' must contain all values in set"
    )
  } else if (length(union(tarset, dt[, get(x)])) != setlen) {
    stopper(
      deparse(substitute(dt)),
      x,
      "'%s' column '%s' may not contain values outside of set"
    )
  }
  dt[]
}

#' @keywords internal
assert_subset <- function(dt, x, tarset) {
  checkset <- unique(dt[, get(x)])
  if (!all(checkset %in% tarset)) {
    stopper(
      deparse(substitute(dt)),
      x,
      "'%s' column '%s' must be contained in parent set: missing %s",
      toString(setdiff(checkset, tarset))
    )
  }
  dt[]
}

#' @keywords internal
#' @importFrom data.table setDT
#' @importFrom data.table as.data.table
assert_dt_able <- function(dt, copy = FALSE) {
  if (copy) as.data.table(dt) else setDT(dt)
}

#' @keywords internal
assert_cols <- function(dt, cols, warn_extra = FALSE) {
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      "'",
      deparse(substitute(dt)),
      "' is missing the following required column(s): ",
      toString(missing_cols)
    )
  }
  if (warn_extra) {
    extra_cols <- setdiff(names(dt), cols)
    if (length(extra_cols) > 0) {
      warning(
        "'",
        deparse(substitute(dt)),
        "' has the following extra columns: ",
        toString(extra_cols)
      )
    }
  }
  dt[]
}

#' @keywords internal
checked_positive_numeric <- function(dt, x, na_allowed = FALSE) {
  if (dt[, !is.numeric(get(x))]) {
    stopper(deparse(substitute(dt)), x, "'%s' column '%s' must be numeric")
  }
  if (!na_allowed && dt[, any(is.na(get(x)))]) {
    stopper(
      deparse(substitute(dt)),
      x,
      "'%s' column '%s' cannot have NA values"
    )
  }
  if (dt[, any(get(x) <= 0)]) {
    stopper(deparse(substitute(dt)), x, "'%s' column '%s' must all be > 0")
  }
  dt[]
}
