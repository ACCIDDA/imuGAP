
stopper <- function(dt, x, fmt, ...) {
  msg <- sprintf(fmt, dt, x, ...)
  stop(msg, call. = FALSE)
}

checked_as_integer <- function(dt, x, na_allowed = FALSE) {
  if (dt[, !is.integer(get(x))]) {
    if (!all(as.integer(dt[, get(x)]) == dt[, get(x)])) {
      stopper(deparse(substitute(dt)), x, "'%s' column '%s' must be integer")
    } else {
      expr <- parse(text = sprintf("%s := as.integer(%s)", x, x))
      dt[, eval(expr)]
    }
  }
  if (!na_allowed && dt[, any(is.na(get(x)))]) {
    stopper(deparse(substitute(dt)), x, "'%s' column '%s' cannot have NA values")
  }
  return(dt[])
}

checked_positive_integer <- function(dt, x, na_allowed = FALSE) {
  if (checked_as_integer(dt, x, na_allowed)[, any(get(x) < 1L)]) {
    stopper(deparse(substitute(dt)), x, "'%s' column '%s' must all be > 0")
  }
  return(dt[])
}

checked_nonneg_integer <- function(dt, x, na_allowed = FALSE) {
  if (checked_as_integer(dt, x, na_allowed)[, any(get(x) < 0L)]) {
    stopper(deparse(substitute(dt)), x, "'%s' column '%s' must all be >= 0")
  }
  return(dt[])
}

checked_maxed_pos_integer <- function(dt, x, max, na_allowed = FALSE) {
  checked_positive_integer(dt, x, na_allowed)
  if (!missing(max)) {
    if (dt[, any(get(x) > max)]) {
      stopper(
        deparse(substitute(dt)), x, "'%s' column '%s' must all be <= %i", max
      )
    }
  }
  return(dt[])
}

checked_set_equivalence <- function(dt, x, tarset) {
  tarset <- unique(tarset)
  setlen <- length(tarset)
  if (length(intersect(tarset, dt[, get(x)])) != setlen) {
    stopper(
      deparse(substitute(dt)), x,
      "'%s' column '%s' must contain all values in set"
    )
  }
  return(dt[])
}

#' @importFrom data.table setDT
checked_dt_able <- function(dt, copy = FALSE) {
  return(tryCatch(
    if (copy) as.data.table(dt)[] else setDT(dt)[],
    error = function(e) {
      stop(
        "'", deparse(substitute(dt)), "' must be valid argument to `data.table::setDT`\n",
        "\tclass(", deparse(substitute(dt)), ") = ", paste(class(dt), collapse = "/")
      )
    }
  ))
}

checked_cols <- function(dt, cols) {
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      "'", deparse(substitute(dt)), "' is missing the following required column(s): ", toString(missing_cols)
    )
  }
  return(dt[])
}
