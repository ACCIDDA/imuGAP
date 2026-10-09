#' @title Create observation populations
#'
#' @description
#' `create_observation_populations` is a convenience function to construct a properly weighted
#' `populations` object for typical modes of observation.
#'
#' @param observations a pre- or post-canonicalization `[data.frame()]`.
#'   Optionally contains additional columns required for the specified `mode` that
#'   vary by row.
#' @param mode character string; the mode for populations creation (default: `"snapshot"`).
#' @param ... additional arguments determined by the specified `mode` requirements
#'   for values which do not vary by row.
#'
#' @details
#' This function uses a combination of varying information from `observations` and
#' fixed information from `...` arguments to provide the necessary information
#' for the modes to produce a `[canonicalize_populations()]` ready-object.
#'
#' Supported modes and required information:
#'
#' # "snapshot" mode
#' As with `[create_target()]`, a snapshot view is looking at a particular place,
#' time, and dose target, but with varying birth cohorts. That means the sum of
#' birth cohort and age is constant: if birth cohort 1 is age 10, then cohort 2 is 9,
#' and so on.
#'
#' Snapshots requires `obs_id`, `loc_id`, `dose`, `age_min`, and `cohort`
#' the reference cohort corresponding to the oldest age. `age_max` may be provided,
#' but if missing or `NA`, is assumed to be `age_min` + 1. `age_max` corresponds
#' to the first *excluded* age - i.e.
#' \code{[age_min, age_max)}
#'
#' Taking this approach to `age_max` enables this method to naturally support partial
#' cohorts. For example, if `age_max = 18.5` and `age_min = 17`, then age 17 population
#' has two-thirds the weight and the age 18 population has one-third. `age_min` works the
#' same way.
#'
#' Note that "snapshot" mode assumes that all populations are uniformly sized with
#' respect to weighting. This assumption may be inadequate when population age
#' groups contributing to an observation are very differently sized.
#'
#' @return a `[data.table()]`, representing the populations mapping.
#' @autoglobal
#' @export
create_observation_populations <- function(
  observations,
  mode = "snapshot",
  ...
) {
  mode <- match.arg(mode)

  obs_dt <- canonicalize_observations(observations, drop_extra = FALSE)

  required_cols <- switch(
    mode,
    "snapshot" = c("loc_id", "cohort", "age_min", "dose")
  )

  # check that required columns are present in either observations or ...
  dot_args <- list(...)
  missing_cols <- setdiff(
    required_cols,
    c(names(observations), names(dot_args))
  )
  stop_fmt_if(
    length(missing_cols) > 0,
    ERR_HELP_MODE_MISSING_COLS,
    mode = mode,
    required = required_cols,
    missing = missing_cols
  )

  optional_cols <- c("age_max")

  dup_cols <- intersect(
    c(required_cols, optional_cols),
    intersect(names(observations), names(dot_args))
  )
  stop_fmt_if(
    length(dup_cols) > 0,
    ERR_HELP_MODE_DUP_COLS,
    cols = dup_cols
  )

  # merge required columns into obs_dt
  if (length(dot_args) > 0) {
    obs_dt[, c(names(dot_args)) := dot_args]
  }

  if (mode == "snapshot") {
    # confirm: dose and cohort are positive integers
    assert_positive_integer(obs_dt, "dose")
    assert_positive_integer(obs_dt, "cohort")

    # confirm: age_min, age_max are in positive numerics, with age_min <= age_max
    assert_positive_numeric(obs_dt, "age_min")
    # age_max is optional; if completely missing or rows == NA, assumed to be age_min
    if (!("age_max") %in% names(obs_dt)) {
      obs_dt[, age_max := NA_integer_]
    }
    obs_dt[is.na(age_max), age_max := age_min + 1L]
    assert_positive_numeric(obs_dt, "age_max")

    stop_fmt_if(obs_dt[, !all(age_min < age_max)], ERR_HELP_AGE_MIN_MAX)

    pop_dt <- obs_dt[,
      {
        age_span <- age_max - age_min
        a_min <- as.integer(age_min)
        a_max <- as.integer(ceiling(age_max - 1L))
        age_seq <- seq.int(a_min, a_max)

        contrib <- pmin(age_seq + 1, age_max) - pmax(age_seq, age_min)
        wts <- contrib / age_span

        .(
          loc_id = loc_id,
          cohort = as.integer(cohort + a_max - age_seq),
          age = age_seq,
          dose = dose,
          weight = wts
        )
      },
      by = obs_id
    ]

    col_order <- intersect(
      c("obs_id", "loc_id", "cohort", "age", "dose", "weight"),
      names(pop_dt)
    )
    data.table::setcolorder(pop_dt, col_order)

    pop_dt[]
  }
}

#' @title Greatest common divisor
#'
#' @description
#' Computes the greatest common divisor of two integers.
#'
#' @param a integer value.
#' @param b integer value.
#'
#' @return an integer, the greatest common divisor.
#'
#' @keywords internal
#' @noRd
gcd <- function(a, b) {
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  a
}

#' @title Least common multiple
#'
#' @description
#' Computes the least common multiple of two integers.
#'
#' @param a integer value.
#' @param b integer value.
#'
#' @return a numeric, the least common multiple.
#'
#' @keywords internal
#' @noRd
lcm <- function(a, b) {
  (a * b) / gcd(a, b)
}

#' @title Compute recycled target vector length
#'
#' @description
#' Computes the least common multiple of a vector of integer lengths.
#'
#' @param lens integer vector of lengths.
#'
#' @return a numeric, the combined recycled length.
#'
#' @keywords internal
#' @noRd
compute_recycled_target_len <- function(lens) {
  target_len <- lens[1]
  for (len in lens[-1L]) {
    target_len <- lcm(target_len, len)
  }
  target_len
}

#' @title Validate vector inputs for target creation
#'
#' @description
#' Validates non-missing, non-NA, and non-empty vector arguments for target grid generation.
#'
#' @param location vector of location IDs.
#' @param age vector of ages.
#' @param cohort vector of cohorts.
#' @param dose vector of doses.
#'
#' @return a named integer vector, of input lengths.
#'
#' @keywords internal
#' @noRd
validate_vec_inputs <- function(location, age, cohort, dose) {
  stop_fmt_if(
    missing(age) || missing(cohort) || missing(dose),
    ERR_HELP_VEC_INPUTS_MISSING,
    n = 2L
  )

  na_args <- c("location", "age", "cohort", "dose")[which(
    c(
      any(is.na(location)),
      any(is.na(age)),
      any(is.na(cohort)),
      any(is.na(dose))
    )
  )]

  stop_fmt_if(
    length(na_args) > 0,
    ERR_HELP_VEC_INPUTS_NA,
    args = na_args,
    n = 2L
  )

  n_loc <- length(location)
  n_age <- length(age)
  n_coh <- length(cohort)
  n_dos <- length(dose)

  zero_lens <- c("location", "age", "cohort", "dose")[which(
    c(n_loc, n_age, n_coh, n_dos) == 0L
  )]

  stop_fmt_if(
    length(zero_lens) > 0,
    ERR_HELP_VEC_INPUTS_ZERO_LEN,
    args = zero_lens,
    n = 2L
  )
  c(n_loc = n_loc, n_age = n_age, n_coh = n_coh, n_dos = n_dos)
}

#' @title Construct a target grid for prediction
#'
#' @description
#' Builds a target grid, for use with `[predict.imugap_fit()]`, from vectors of
#' locations, ages, cohorts, and doses. This is pure construction and does not
#' reference a fitted model, so it can be called without a fit (e.g. to expand a
#' request into rows before any fit exists). To validate a target against a
#' specific fit -- or to canonicalize a target you built yourself as a
#' `data.frame` -- use `[canonicalize_target()]`; `[predict.imugap_fit()]` does
#' this for you.
#'
#' @param location a vector of location IDs to target.
#' @param age integer or numeric vector of ages for which to predict coverage, consistent with
#'   `[canonicalize_populations()]`.
#' @param cohort integer or numeric vector of cohorts for which to predict coverage, consistent with
#'   `[canonicalize_populations()]`.
#' @param dose integer vector of doses for which to predict coverage, consistent with
#'   `[canonicalize_observations()]`.
#' @param mode character string specifying how vector inputs combine (default: `"error"`).
#'   One of `"error"`, `"enumerate"`, `"recycle"`, or `"snapshot"`:
#'
#'   - `"error"`: all vector inputs must have the same length.
#'   - `"enumerate"`: all combinations of the inputs.
#'   - `"recycle"`: recycle the inputs out to the least-common-multiple length.
#'   - `"snapshot"`: `cohort` must be a single reference value (the oldest
#'     cohort); locations, ages, and doses are enumerated with a cohort for each
#'     age such that `age + cohort` is constant, using the **maximum** value of
#'     `age` to set that constant (`cohort_i = cohort_ref + max(age) - age_i`),
#'     i.e. a snapshot in time.
#'
#' @return a `[data.table()]`, target grid with columns `obs_c_id`, `loc_id`, `age`,
#'   `cohort`, `dose`, and `weight`.
#'
#' @seealso `[canonicalize_target()]`, `[predict.imugap_fit()]`
#'
#' @examples
#' # "error" mode: all vector inputs must have the same length.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2), cohort = c(2, 3), dose = c(1, 1), mode = "error"
#' )
#'
#' # "enumerate": all combinations of the inputs.
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2), cohort = c(2, 3), dose = c(1), mode = "enumerate"
#' )
#'
#' # "snapshot": cohort is a single reference; cohorts are set so age + cohort is
#' # constant, using max(age).
#' create_target(
#'   location = c("Blue Heron School", "Bluebird Learning Center"),
#'   age = c(1, 2, 3), cohort = 5, dose = c(1), mode = "snapshot"
#' )
#'
#' @importFrom data.table as.data.table copy data.table
#' @export
create_target <- function(
  location,
  age,
  cohort,
  dose,
  mode = c("error", "enumerate", "recycle", "snapshot")
) {
  mode <- match.arg(mode)
  lens <- validate_vec_inputs(location, age, cohort, dose)

  if (mode == "error") {
    stop_fmt_if(length(unique(lens)) > 1L, ERR_HELP_ERROR_MODE_LEN)
    target <- data.table::data.table(
      loc_id = location,
      age = age,
      cohort = cohort,
      dose = dose,
      weight = 1.0
    )
  } else if (mode %in% c("enumerate", "snapshot")) {
    stop_fmt_if(
      mode == "snapshot" && length(cohort) != 1L,
      ERR_HELP_SNAP_COHORT_SINGLE
    )
    target <- data.table::as.data.table(expand.grid(
      loc_id = location,
      age = age,
      cohort = cohort,
      dose = dose,
      weight = 1.0,
      stringsAsFactors = FALSE
    ))

    if (mode == "snapshot") {
      ref_cohort <- cohort
      max_age <- max(age)
      target[, cohort := ref_cohort + max_age - age]
    }
  } else if (mode == "recycle") {
    target_len <- compute_recycled_target_len(lens)

    target <- data.table::data.table(
      loc_id = rep_len(location, target_len),
      age = rep_len(age, target_len),
      cohort = rep_len(cohort, target_len),
      dose = rep_len(dose, target_len),
      weight = 1.0
    )
  }
  target[, obs_c_id := seq_len(.N)]
  data.table::setcolorder(
    target,
    c("obs_c_id", "loc_id", "age", "cohort", "dose", "weight")
  )
  target[]
}

#' @title Assemble Multi-Layer Location Hierarchy Data for Stan
#'
#' @description
#' Extracts structural metadata and 1D boundary start indices from a
#' canonicalized locations table for consumption by Stan multi-layer models.
#'
#' @param loc_info a canonicalized locations table (passed to
#'   `[canonicalize_locations()]`).
#'
#' @return a named list, containing:
#'   - `n_locs`: integer total count of locations
#'   - `n_layers`: integer maximum depth / number of layers (>= 2)
#'   - `layer_starts`: integer array of starting location indices for each layer (length `n_layers`)
#'   - `n_parent_locs`: integer count of parent locations that have children
#'   - `parent_loc_id`: integer array (length `n_parent_locs`) of canonical IDs of parent locations
#'   - `parent_child_starts`: integer array (length `n_parent_locs`) of starting child location IDs
#'   - `loc_population`: numeric array (length `n_locs`) of population weights
#'
#' @keywords internal
#' @noRd
#' @autoglobal
assemble_layer_data <- function(loc_info) {
  n_locs <- nrow(loc_info)
  n_layers <- max(loc_info$layer)
  layer_starts <- loc_info[, min(loc_c_id), by = layer]$V1

  # Parent locations metadata (locations having children)
  parent_loc_info <- loc_info[
    loc_id %in% loc_info$parent_id[!is.na(parent_id)],
    .(parent_loc_c_id = loc_c_id, loc_id)
  ]
  data.table::setkeyv(parent_loc_info, "parent_loc_c_id")
  n_parent_locs <- nrow(parent_loc_info)
  parent_loc_id <- as.integer(parent_loc_info$parent_loc_c_id)

  parent_child_starts <- if (n_parent_locs > 0L) {
    child_min <- loc_info[
      parent_id %in% parent_loc_info$loc_id,
      .(min_child = min(loc_c_id)),
      by = parent_id
    ]
    child_min[parent_loc_info, on = .(parent_id = loc_id)]$min_child
  } else {
    integer(0)
  }

  loc_population <- if ("population" %in% names(loc_info)) {
    as.numeric(loc_info$population)
  } else {
    rep(NA_real_, n_locs)
  }

  parent_ids <- unique(loc_info$parent_id[!is.na(loc_info$parent_id)])
  is_leaf <- !(loc_info$loc_id %in% parent_ids)

  # Default outermost leaves with NA or <= 0 population to 1.0
  loc_population[is_leaf & (is.na(loc_population) | loc_population <= 0)] <- 1.0

  # Bottom-up accumulation for parent entities from lowest non-leaf layer to root
  if (n_layers >= 2L) {
    for (lyr in seq(n_layers - 1L, 1L, by = -1L)) {
      parent_rows <- which(loc_info$layer == lyr & !is_leaf)
      for (p in parent_rows) {
        pid <- loc_info$loc_id[p]
        child_rows <- which(loc_info$parent_id == pid)
        child_sum <- sum(loc_population[child_rows], na.rm = TRUE)
        if (is.na(loc_population[p]) || loc_population[p] <= 0) {
          loc_population[p] <- child_sum
        }
      }
    }
  }
  loc_population[is.na(loc_population)] <- 1.0

  # Precompute QR block entries and index bounds for balanced layer offsets
  qr_data <- compute_layer_qr(
    n_parent_locs,
    parent_child_starts,
    n_locs,
    loc_population
  )

  list(
    n_locs = n_locs,
    n_layers = n_layers,
    layer_starts = as.array(as.integer(layer_starts)),
    n_parent_locs = n_parent_locs,
    parent_loc_id = as.array(as.integer(parent_loc_id)),
    parent_child_starts = as.array(as.integer(parent_child_starts)),
    loc_population = as.array(as.numeric(loc_population)),
    n_qr_entries = qr_data$n_qr_entries,
    z_bounds = qr_data$z_bounds,
    qr_bounds = qr_data$qr_bounds,
    qr_entries = as.array(as.numeric(qr_data$qr_entries))
  )
}

#' @title Precompute orthonormal nullspace QR basis for hierarchical layers
#'
#' @description
#' Computes per-parent block QR bases, bounds, and flattened entries for weighted
#' sum-to-zero offsets.
#'
#' @param n_parent_locs integer scalar; number of parent locations.
#' @param parent_child_starts integer vector; starting 1-based child index for each parent.
#' @param n_locs integer scalar; total number of locations.
#' @param loc_population numeric vector; location population sizes.
#'
#' @return a list containing `n_qr_entries`, `z_bounds`, `qr_bounds`, and `qr_entries`.
#' @keywords internal
#' @noRd
compute_layer_qr <- function(
  n_parent_locs,
  parent_child_starts,
  n_locs,
  loc_population
) {
  if (n_parent_locs == 0L) {
    return(list(
      n_qr_entries = 0L,
      z_bounds = matrix(0L, nrow = 2L, ncol = 0L),
      qr_bounds = matrix(0L, nrow = 2L, ncol = 0L),
      qr_entries = numeric(0L)
    ))
  }

  parent_child_bounds <- rbind(
    parent_child_starts,
    c(parent_child_starts[-1L] - 1L, n_locs)
  )

  z_bounds <- matrix(0L, nrow = 2L, ncol = n_parent_locs)
  qr_bounds <- matrix(0L, nrow = 2L, ncol = n_parent_locs)
  qr_list <- vector("list", n_parent_locs)

  cur_z <- 1L
  cur_qr <- 1L
  for (p in seq_len(n_parent_locs)) {
    st <- parent_child_bounds[1L, p]
    en <- parent_child_bounds[2L, p]
    k_len <- en - st + 1L

    z_bounds[1L, p] <- cur_z
    z_bounds[2L, p] <- cur_z + k_len - 2L

    qr_bounds[1L, p] <- cur_qr
    qr_bounds[2L, p] <- cur_qr + k_len * (k_len - 1L) - 1L

    pop_slice <- loc_population[st:en]
    sum_pop <- sum(pop_slice)
    w <- if (sum_pop > 0) pop_slice / sum_pop else rep(1.0 / k_len, k_len)
    w_prime <- sqrt(w)

    v1 <- w_prime / sqrt(sum(w_prime^2))
    mat_m <- matrix(0.0, nrow = k_len, ncol = k_len)
    mat_m[, 1L] <- v1
    for (j in seq_len(k_len - 1L)) {
      mat_m[j, j + 1L] <- 1.0
    }
    mat_q <- qr.Q(qr(mat_m))
    q_star <- mat_q[, 2L:k_len, drop = FALSE]
    for (j in seq_len(ncol(q_star))) {
      nz <- which(abs(q_star[, j]) > 1e-10)[1L]
      if (!is.na(nz) && q_star[nz, j] < 0) {
        q_star[, j] <- -q_star[, j]
      }
    }
    qr_list[[p]] <- as.vector(q_star)

    cur_z <- cur_z + k_len - 1L
    cur_qr <- cur_qr + k_len * (k_len - 1L)
  }

  qr_entries <- unlist(qr_list)

  list(
    n_qr_entries = length(qr_entries),
    z_bounds = z_bounds,
    qr_bounds = qr_bounds,
    qr_entries = qr_entries
  )
}

#' @title Ensure Stan data contains precomputed QR basis entries
#'
#' @description
#' Backfills `n_qr_entries`, `z_bounds`, `qr_bounds`, and `qr_entries` into Stan data
#' if missing.
#'
#' @param dat_stan list of Stan input data.
#'
#' @return a list of Stan input data guaranteed to contain QR basis entries.
#' @keywords internal
#' @noRd
ensure_layer_qr <- function(dat_stan) {
  is_single <- is.null(dat_stan$n_parent_locs) || dat_stan$n_parent_locs == 0L
  has_qr <- !is.null(dat_stan$qr_entries) &&
    !is.null(dat_stan$z_bounds) &&
    !is.null(dat_stan$qr_bounds)
  if (is_single || has_qr) {
    return(dat_stan)
  }
  qr_data <- compute_layer_qr(
    as.integer(dat_stan$n_parent_locs),
    as.integer(dat_stan$parent_child_starts),
    as.integer(dat_stan$n_locs),
    as.numeric(dat_stan$loc_population)
  )
  dat_stan$n_qr_entries <- qr_data$n_qr_entries
  dat_stan$z_bounds <- qr_data$z_bounds
  dat_stan$qr_bounds <- qr_data$qr_bounds
  dat_stan$qr_entries <- as.array(as.numeric(qr_data$qr_entries))
  dat_stan
}

#' @title Validate and subset posterior draws array
#'
#' @description
#' Validates the requested posterior sample size against chain count and available draws,
#' rounding up to a multiple of chains with a warning if needed, and extracts the converged tail.
#'
#' @param draws_array a 3D array of posterior draws (iterations x chains x parameters).
#' @param posterior_size optional integer scalar; how many draws to use from the end of each chain
#'   (default: `NULL`, which returns all draws).
#'
#' @return a 3D array, containing the subsetted posterior draws.
#'
#' @keywords internal
#' @noRd
subset_draws_tail <- function(draws_array, posterior_size = NULL) {
  if (is.null(posterior_size)) {
    return(draws_array)
  }

  n_iter <- dim(draws_array)[1]
  n_chains <- dim(draws_array)[2]
  n_avail <- n_iter * n_chains

  posterior_size <- assert_positive_int(posterior_size, "posterior_size")
  stop_fmt_if(length(posterior_size) != 1L, ERR_POSTERIOR_SIZE_SINGLE)

  rounded <- as.integer(ceiling(posterior_size / n_chains) * n_chains)
  warn_fmt_if(
    posterior_size != rounded,
    MSG_POSTERIOR_SIZE_ROUNDED,
    posterior_size = posterior_size,
    n_chains = n_chains,
    adjusted_size = rounded
  )
  posterior_size <- rounded

  stop_fmt_if(
    posterior_size > n_avail,
    ERR_POSTERIOR_SIZE_EXCEEDS,
    posterior_size = posterior_size,
    n_draws = n_avail
  )

  warn_fmt_if(
    TRUE,
    MSG_POSTERIOR_SUBSAMPLE_WARN,
    posterior_size = posterior_size
  )

  keep <- posterior_size %/% n_chains
  draws_array[seq.int(n_iter - keep + 1L, n_iter), , , drop = FALSE]
}

#' @title Ensure presence of parameter columns in posterior draws
#'
#' @description
#' Reconstructs `z_layer` parameters from `off_layer` and the precomputed block
#' QR basis if `z_layer` was dropped from posterior draws during sampling.
#'
#' @param draws_mat matrix of flattened posterior draws (rows = draws, cols = parameters).
#' @param dat_stan list of Stan input data.
#'
#' @return a matrix containing `draws_mat` and any reconstructed `z_layer` columns.
#' @keywords internal
#' @noRd
ensure_draws_parameters <- function(draws_mat, dat_stan) {
  if (is.null(dat_stan$n_parent_locs) || dat_stan$n_parent_locs == 0L) {
    return(draws_mat)
  }
  cols <- colnames(draws_mat)
  if (
    is.null(cols) ||
      any(grepl("^z_layer(\\[|$)", cols)) ||
      !any(grepl("^off_layer(\\[|$)", cols))
  ) {
    return(draws_mat)
  }

  n_locs <- dat_stan$n_locs
  n_layers <- dat_stan$n_layers
  layer_starts <- as.integer(dat_stan$layer_starts)
  n_parent_locs <- dat_stan$n_parent_locs
  parent_child_starts <- as.integer(dat_stan$parent_child_starts)
  loc_population <- as.numeric(dat_stan$loc_population)

  layer_bounds <- rbind(layer_starts, c(layer_starts[-1L] - 1L, n_locs))
  parent_child_bounds <- rbind(
    parent_child_starts,
    c(parent_child_starts[-1L] - 1L, n_locs)
  )

  loc_layer_idx <- integer(n_locs - 1L)
  for (k in seq_len(n_layers - 1L)) {
    st <- layer_bounds[1L, k + 1L] - 1L
    en <- layer_bounds[2L, k + 1L] - 1L
    loc_layer_idx[st:en] <- k
  }

  loc_pop_scale <- numeric(n_locs - 1L)
  for (k in seq_len(n_layers - 1L)) {
    st <- layer_bounds[1L, k + 1L]
    en <- layer_bounds[2L, k + 1L]
    layer_pop <- loc_population[st:en]
    mean_layer_pop <- mean(layer_pop)
    for (i in st:en) {
      pop_val <- loc_population[i]
      loc_pop_scale[i - 1L] <- if (pop_val > 0 && mean_layer_pop > 0) {
        sqrt(mean_layer_pop / pop_val)
      } else {
        1.0
      }
    }
  }

  dat_stan <- ensure_layer_qr(dat_stan)

  z_cols <- vector("list", n_parent_locs)
  for (p in seq_len(n_parent_locs)) {
    st <- parent_child_bounds[1L, p]
    en <- parent_child_bounds[2L, p]
    k_size <- en - st + 1L

    z_st <- dat_stan$z_bounds[1L, p]
    z_en <- dat_stan$z_bounds[2L, p]
    q_st <- dat_stan$qr_bounds[1L, p]
    q_en <- dat_stan$qr_bounds[2L, p]
    q_star <- matrix(
      dat_stan$qr_entries[q_st:q_en],
      nrow = k_size,
      ncol = k_size - 1L
    )

    l_st <- st - 1L
    l_en <- en - 1L
    layer_idx <- loc_layer_idx[l_st]
    pop_scale <- loc_pop_scale[l_st:l_en]

    off_names <- paste0("off_layer[", l_st:l_en, "]")
    sigma_name <- paste0("sigma_layer[", layer_idx, "]")

    off_sub <- draws_mat[, off_names, drop = FALSE]
    sigma <- draws_mat[, sigma_name]
    raw_off <- sweep(off_sub, 2L, pop_scale, "/") / sigma
    z_sub <- raw_off %*% q_star
    colnames(z_sub) <- paste0("z_layer[", z_st:z_en, "]")
    z_cols[[p]] <- z_sub
  }

  cbind(draws_mat, do.call(cbind, z_cols))
}
