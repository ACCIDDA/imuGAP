skip_if_not_installed("rstan")
#' "functions/lookups.stan" defines
#' `array[] int compute_cdf_lookup(array[] int life_year, array[] int dose, int n_yr, int n_doses)`
#' and `array[] int compute_phi_lookup(array[] int cohort, array[] int location, int n_cohort, int n_locs)`
#' for 1D column-major index flattening with bounds validation.
#' These functions convert observation meta data (e.g. life year and dose) into
#' the index to work with the lookup objects.
#' `vector compute_p_obs(...)` computes segmented weighted observation probabilities.
#' This test synthesizes creation of phi and dose probs to test functions.

target <- "functions/lookups.stan"

skip_if_stan_unchanged(target)

model_lookups <- sprintf(
  "
functions {
  #include %s
}
data {
  int N_cdf;
  array[N_cdf] int life_year;
  array[N_cdf] int dose;
  int n_yr;
  int n_doses;

  int N_phi;
  array[N_phi] int cohort;
  array[N_phi] int location;
  int n_cohort;
  int n_locs;

  int n_obs_p;
  int n_weights_p;
  int n_phi_vec;
  vector[n_phi_vec] phi;
  array[n_weights_p] int phi_lookup;
  int n_unrolled_vec;
  vector[n_unrolled_vec] unrolled_dose_probs;
  array[n_weights_p] int cdf_lookup;
  vector[n_weights_p] weights;
  array[2, n_obs_p] int obs_map;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  array[N_cdf] int out_cdf_lookup = compute_cdf_lookup(life_year, dose, n_yr, n_doses);
  array[N_phi] int out_phi_lookup = compute_phi_lookup(cohort, location, n_cohort, n_locs);
  vector[n_obs_p] out_p_obs = compute_p_obs(
    n_obs_p,
    n_weights_p,
    phi,
    phi_lookup,
    unrolled_dose_probs,
    cdf_lookup,
    weights,
    obs_map
  );
}
",
  target
) |>
  compile_stan_harness()

test_that("compute_cdf_lookup maps (life_year, dose) pairs to 1D column-major indices", {
  life_year <- c(1L, 2L, 5L, 3L)
  dose <- c(1L, 1L, 2L, 3L)
  n_yr <- 10L
  n_doses <- 3L

  cdf_lookup <- run_stan_harness(
    model_lookups,
    data = list(
      N_cdf = length(life_year),
      life_year = life_year,
      dose = dose,
      n_yr = n_yr,
      n_doses = n_doses,
      N_phi = 0L,
      cohort = integer(0),
      location = integer(0),
      n_cohort = 1L,
      n_locs = 1L,
      n_obs_p = 0L,
      n_weights_p = 0L,
      n_phi_vec = 1L,
      phi = as.array(0.2),
      phi_lookup = integer(0),
      n_unrolled_vec = 1L,
      unrolled_dose_probs = as.array(0.8),
      cdf_lookup = integer(0),
      weights = numeric(0),
      obs_map = matrix(integer(0), nrow = 2, ncol = 0)
    ),
    out_cdf_lookup
  )

  expect_equal(
    as.numeric(cdf_lookup),
    life_year + (dose - 1L) * n_yr
  )
})

test_that("compute_cdf_lookup errors on out of bounds life_year", {
  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 2L,
        life_year = c(0L, 2L),
        dose = c(1L, 1L),
        n_yr = 5L,
        n_doses = 3L,
        N_phi = 0L,
        cohort = integer(0),
        location = integer(0),
        n_cohort = 1L,
        n_locs = 1L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_cdf_lookup
    ),
    "out of bounds"
  )

  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 2L,
        life_year = c(1L, 6L),
        dose = c(1L, 1L),
        n_yr = 5L,
        n_doses = 3L,
        N_phi = 0L,
        cohort = integer(0),
        location = integer(0),
        n_cohort = 1L,
        n_locs = 1L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_cdf_lookup
    ),
    "out of bounds"
  )
})

test_that("compute_cdf_lookup errors on out of bounds dose", {
  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 2L,
        life_year = c(1L, 2L),
        dose = c(0L, 1L),
        n_yr = 5L,
        n_doses = 3L,
        N_phi = 0L,
        cohort = integer(0),
        location = integer(0),
        n_cohort = 1L,
        n_locs = 1L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_cdf_lookup
    ),
    "out of bounds"
  )

  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 2L,
        life_year = c(1L, 2L),
        dose = c(1L, 4L),
        n_yr = 5L,
        n_doses = 3L,
        N_phi = 0L,
        cohort = integer(0),
        location = integer(0),
        n_cohort = 1L,
        n_locs = 1L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_cdf_lookup
    ),
    "out of bounds"
  )
})

test_that("compute_cdf_lookup errors if n_yr or n_doses is less than 1", {
  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 1L,
        life_year = 1L,
        dose = 1L,
        n_yr = 0L,
        n_doses = 3L,
        N_phi = 0L,
        cohort = integer(0),
        location = integer(0),
        n_cohort = 1L,
        n_locs = 1L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_cdf_lookup
    ),
    "n_yr must be >= 1"
  )

  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 1L,
        life_year = 1L,
        dose = 1L,
        n_yr = 5L,
        n_doses = 0L,
        N_phi = 0L,
        cohort = integer(0),
        location = integer(0),
        n_cohort = 1L,
        n_locs = 1L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_cdf_lookup
    ),
    "n_doses must be >= 1"
  )
})

test_that("compute_phi_lookup maps (cohort, location) pairs to 1D column-major indices", {
  cohort <- c(1L, 3L, 4L, 2L)
  location <- c(1L, 2L, 5L, 3L)
  n_cohort <- 6L
  n_locs <- 5L

  phi_lookup <- run_stan_harness(
    model_lookups,
    data = list(
      N_cdf = 0L,
      life_year = integer(0),
      dose = integer(0),
      n_yr = 1L,
      n_doses = 1L,
      N_phi = length(cohort),
      cohort = cohort,
      location = location,
      n_cohort = n_cohort,
      n_locs = n_locs,
      n_obs_p = 0L,
      n_weights_p = 0L,
      n_phi_vec = 1L,
      phi = as.array(0.2),
      phi_lookup = integer(0),
      n_unrolled_vec = 1L,
      unrolled_dose_probs = as.array(0.8),
      cdf_lookup = integer(0),
      weights = numeric(0),
      obs_map = matrix(integer(0), nrow = 2, ncol = 0)
    ),
    out_phi_lookup
  )

  expect_equal(
    as.numeric(phi_lookup),
    cohort + (location - 1L) * n_cohort
  )
})

test_that("compute_phi_lookup errors on out of bounds cohort", {
  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 0L,
        life_year = integer(0),
        dose = integer(0),
        n_yr = 1L,
        n_doses = 1L,
        N_phi = 2L,
        cohort = c(0L, 2L),
        location = c(1L, 1L),
        n_cohort = 5L,
        n_locs = 3L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_phi_lookup
    ),
    "out of bounds"
  )

  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 0L,
        life_year = integer(0),
        dose = integer(0),
        n_yr = 1L,
        n_doses = 1L,
        N_phi = 2L,
        cohort = c(1L, 6L),
        location = c(1L, 1L),
        n_cohort = 5L,
        n_locs = 3L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_phi_lookup
    ),
    "out of bounds"
  )
})

test_that("compute_phi_lookup errors on out of bounds location", {
  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 0L,
        life_year = integer(0),
        dose = integer(0),
        n_yr = 1L,
        n_doses = 1L,
        N_phi = 2L,
        cohort = c(1L, 2L),
        location = c(0L, 1L),
        n_cohort = 5L,
        n_locs = 3L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_phi_lookup
    ),
    "out of bounds"
  )

  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 0L,
        life_year = integer(0),
        dose = integer(0),
        n_yr = 1L,
        n_doses = 1L,
        N_phi = 2L,
        cohort = c(1L, 2L),
        location = c(1L, 4L),
        n_cohort = 5L,
        n_locs = 3L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_phi_lookup
    ),
    "out of bounds"
  )
})

test_that("compute_phi_lookup errors if n_cohort or n_locs is less than 1", {
  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 0L,
        life_year = integer(0),
        dose = integer(0),
        n_yr = 1L,
        n_doses = 1L,
        N_phi = 1L,
        cohort = 1L,
        location = 1L,
        n_cohort = 0L,
        n_locs = 3L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_phi_lookup
    ),
    "n_cohort must be >= 1"
  )

  expect_error(
    run_stan_harness(
      model_lookups,
      data = list(
        N_cdf = 0L,
        life_year = integer(0),
        dose = integer(0),
        n_yr = 1L,
        n_doses = 1L,
        N_phi = 1L,
        cohort = 1L,
        location = 1L,
        n_cohort = 5L,
        n_locs = 0L,
        n_obs_p = 0L,
        n_weights_p = 0L,
        n_phi_vec = 1L,
        phi = as.array(0.2),
        phi_lookup = integer(0),
        n_unrolled_vec = 1L,
        unrolled_dose_probs = as.array(0.8),
        cdf_lookup = integer(0),
        weights = numeric(0),
        obs_map = matrix(integer(0), nrow = 2, ncol = 0)
      ),
      out_phi_lookup
    ),
    "n_locs must be >= 1"
  )
})

test_that("compute_p_obs computes segmented weighted observation probabilities across single and multi-element observations", {
  # Mix of single-element (Obs 1: 1..1, Obs 3: 4..4) and multi-element (Obs 2: 2..3, Obs 4: 5..7) spans
  obs_map <- matrix(
    c(
      1L,
      1L,
      2L,
      3L,
      4L,
      4L,
      5L,
      7L
    ),
    nrow = 2
  )

  phi <- c(0.20, 0.40)
  phi_lookup <- c(1L, 2L, 1L, 2L, 1L, 2L, 1L)
  unrolled_probs <- c(0.70, 0.85, 0.90)
  cdf_lookup <- c(1L, 2L, 2L, 3L, 1L, 3L, 2L)
  weights <- c(1.0, 0.4, 0.6, 1.0, 0.2, 0.3, 0.5)

  p_obs <- run_stan_harness(
    model_lookups,
    data = list(
      N_cdf = 0L,
      life_year = integer(0),
      dose = integer(0),
      n_yr = 1L,
      n_doses = 1L,
      N_phi = 0L,
      cohort = integer(0),
      location = integer(0),
      n_cohort = 1L,
      n_locs = 1L,
      n_obs_p = ncol(obs_map),
      n_weights_p = length(weights),
      n_phi_vec = length(phi),
      phi = phi,
      phi_lookup = phi_lookup,
      n_unrolled_vec = length(unrolled_probs),
      unrolled_dose_probs = unrolled_probs,
      cdf_lookup = cdf_lookup,
      weights = weights,
      obs_map = obs_map
    ),
    out_p_obs
  )

  weighted <- (1 - phi[phi_lookup]) * unrolled_probs[cdf_lookup] * weights
  expected_p <- vapply(
    seq_len(ncol(obs_map)),
    function(i) sum(weighted[obs_map[1, i]:obs_map[2, i]]),
    numeric(1)
  )

  expect_equal(as.numeric(p_obs), expected_p)
})

test_that("compute_p_obs handles empty observation segments (n_obs = 0)", {
  p_obs <- run_stan_harness(
    model_lookups,
    data = list(
      N_cdf = 0L,
      life_year = integer(0),
      dose = integer(0),
      n_yr = 1L,
      n_doses = 1L,
      N_phi = 0L,
      cohort = integer(0),
      location = integer(0),
      n_cohort = 1L,
      n_locs = 1L,
      n_obs_p = 0L,
      n_weights_p = 0L,
      n_phi_vec = 1L,
      phi = as.array(0.2),
      phi_lookup = integer(0),
      n_unrolled_vec = 1L,
      unrolled_dose_probs = as.array(0.8),
      cdf_lookup = integer(0),
      weights = numeric(0),
      obs_map = matrix(integer(0), nrow = 2, ncol = 0)
    ),
    out_p_obs
  )

  expect_equal(length(p_obs), 0L)
})
