skip_if_not_installed("rstan")
# ' "model/censored.stan" defines
# ' the observation likelihood evaluating exact binomial log-probabilities for
# ' uncensored data, binomial LCDF for left-censored data, and binomial LCDF for
# ' right-censored failure count observations.

target <- "model/censored.stan"

skip_if_stan_unchanged(c(
  "transformed_data/censoring.stan",
  target
))

model_likelihood <- sprintf(
  "
data {
  int<lower=0> n_obs_uncensored;
  array[n_obs_uncensored] int y_obs_uncensored;
  array[n_obs_uncensored] int y_smp_uncensored;
  vector[n_obs_uncensored] p_obs_uncensored_in;

  int<lower=0> n_obs_left;
  array[n_obs_left] int y_obs_left;
  array[n_obs_left] int y_smp_left;
  vector[n_obs_left] p_obs_left_in;

  int<lower=0> n_obs_right;
  array[n_obs_right] int y_obs_right;
  array[n_obs_right] int y_smp_right;
  vector[n_obs_right] p_obs_right_in;
}
transformed data {
  #include transformed_data/censoring.stan
}
parameters {
  real dummy;
}
transformed parameters {
  vector[n_obs_uncensored] p_obs_uncensored = p_obs_uncensored_in + dummy;
  vector[n_obs_left] p_obs_left = p_obs_left_in + dummy;
  vector[n_obs_right] p_obs_right = p_obs_right_in + dummy;
}
model {
  dummy ~ normal(0, 1);
  #include %s
}
",
  target
) |>
  compile_stan_harness()

unc_data <- list(
  y_obs = c(10L, 20L),
  y_smp = c(20L, 40L),
  p_obs = c(0.4, 0.5)
)

rt_data <- list(
  y_obs = c(15L, 18L),
  y_smp = c(20L, 30L),
  p_obs = c(0.7, 0.6)
)

lt_data <- list(
  y_obs = c(5L, 8L),
  y_smp = c(20L, 25L),
  p_obs = c(0.3, 0.4)
)

make_stream_list <- function(unc = NULL, lt = NULL, rt = NULL) {
  build_stream <- function(stream, suffix) {
    if (is.null(stream) || length(stream$y_obs) == 0L) {
      stats::setNames(
        list(0L, integer(0), integer(0), numeric(0)),
        paste0(
          c("n_obs", "y_obs", "y_smp", "p_obs"),
          "_",
          suffix,
          c("", "", "", "_in")
        )
      )
    } else {
      stats::setNames(
        list(length(stream$y_obs), stream$y_obs, stream$y_smp, stream$p_obs),
        paste0(
          c("n_obs", "y_obs", "y_smp", "p_obs"),
          "_",
          suffix,
          c("", "", "", "_in")
        )
      )
    }
  }

  c(
    build_stream(unc, "uncensored"),
    build_stream(lt, "left"),
    build_stream(rt, "right")
  )
}

calc_expected_lp <- function(unc = NULL, lt = NULL, rt = NULL) {
  lp <- 0
  if (!is.null(unc) && length(unc$y_obs) > 0L) {
    lp <- lp +
      sum(stats::dbinom(
        unc$y_obs,
        size = unc$y_smp,
        prob = unc$p_obs,
        log = TRUE
      ))
  }
  if (!is.null(lt) && length(lt$y_obs) > 0L) {
    lp <- lp +
      sum(stats::pbinom(
        lt$y_obs,
        size = lt$y_smp,
        prob = lt$p_obs,
        log.p = TRUE
      ))
  }
  if (!is.null(rt) && length(rt$y_obs) > 0L) {
    lp <- lp +
      sum(
        stats::pbinom(
          rt$y_smp - rt$y_obs,
          size = rt$y_smp,
          prob = 1 - rt$p_obs,
          log.p = TRUE
        )
      )
  }
  lp
}

test_cases <- list(
  list(name = "uncensored only", unc = unc_data, lt = NULL, rt = NULL),
  list(name = "right-censored only", unc = NULL, lt = NULL, rt = rt_data),
  list(name = "left-censored only", unc = NULL, lt = lt_data, rt = NULL),
  list(
    name = "uncensored and right-censored",
    unc = unc_data,
    lt = NULL,
    rt = rt_data
  ),
  list(
    name = "uncensored and left-censored",
    unc = unc_data,
    lt = lt_data,
    rt = NULL
  ),
  list(
    name = "right-censored and left-censored",
    unc = NULL,
    lt = lt_data,
    rt = rt_data
  ),
  list(
    name = "uncensored, right-censored, and left-censored",
    unc = unc_data,
    lt = lt_data,
    rt = rt_data
  ),
  list(
    name = "no observations (empty streams)",
    unc = NULL,
    lt = NULL,
    rt = NULL
  )
)

for (tc in test_cases) {
  test_that(sprintf("censored.stan computes log-likelihood for %s", tc$name), {
    data_list <- make_stream_list(tc$unc, tc$lt, tc$rt)
    fit <- run_stan_harness(
      model_likelihood,
      data = data_list,
      return_fit = TRUE
    )
    lp <- rstan::log_prob(fit, upars = c(0.0), adjust_transform = FALSE)
    expected_lp <- calc_expected_lp(tc$unc, tc$lt, tc$rt)
    expect_equal(lp, expected_lp, tolerance = 1e-4)
  })
}
