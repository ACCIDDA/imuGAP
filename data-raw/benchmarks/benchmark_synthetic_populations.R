#' Generate Synthetic Populations for imuGAP Inference Benchmarking
#'
#' Usage:
#'   Rscript benchmark_synthetic_populations.R <config_file.yml>
#'
#' Takes a YAML configuration specifying grid dimensions (K, sigma, p0_trend, n_samples, seed)
#' and generates synthetic 2-layer populations with exact ground-truth parameters (mu, delta, p0)
#' and noiseless observation counts. Writes the result to an .rds file of matching base name.

suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
  library(imuGAP)
})

# --- Exact Mu Shift Solver ----------------------------------------------------

solve_exact_mu <- function(
  p0_vec,
  w_vec,
  delta_vec,
  link = c("logit", "probit")
) {
  link <- match.arg(link)
  inv_link <- if (link == "logit") stats::plogis else stats::pnorm
  link_fn <- if (link == "logit") stats::qlogis else stats::qnorm

  vapply(
    p0_vec,
    function(p0) {
      eta0 <- link_fn(p0)
      stats::uniroot(
        function(mu) sum(w_vec * inv_link(eta0 + mu + delta_vec)) - p0,
        interval = c(-10, 10),
        tol = 1e-12
      )$root
    },
    numeric(1)
  )
}

# --- Single Population Simulation ---------------------------------------------

generate_synthetic_dataset <- function(
  K,
  sigma,
  p0_trend,
  n_sample_obs = 1000L,
  pop_per_sub = 5000L,
  obs_age = 2L,
  dose = 1L,
  seed = 42L,
  link = c("logit", "probit")
) {
  link <- match.arg(link)
  set.seed(seed)
  C <- length(p0_trend)

  # 1. Locations Table
  loc_ids <- c("Root", sprintf("Child_%02d", seq_len(K)))
  locations <- data.table(
    loc_id = loc_ids,
    parent_id = c(NA_character_, rep("Root", K)),
    population = as.numeric(c(pop_per_sub * K, rep(pop_per_sub, K)))
  )

  # 2. True Offsets & Population Weights
  w_vec <- rep(1 / K, K)
  delta_raw <- stats::rnorm(K, mean = 0, sd = sigma)
  delta_true <- delta_raw - sum(w_vec * delta_raw)

  # 3. Exact Parent Shift & Subpopulation Probabilities
  mu_true <- solve_exact_mu(p0_trend, w_vec, delta_true, link = link)
  inv_link <- if (link == "logit") stats::plogis else stats::pnorm
  link_fn <- if (link == "logit") stats::qlogis else stats::qnorm

  # 4. Noiseless Observations (expected integer binomial counts)
  obs_list <- list()
  obs_counter <- 1L
  for (c in seq_len(C)) {
    for (k in seq_len(K)) {
      p_sub <- inv_link(link_fn(p0_trend[c]) + mu_true[c] + delta_true[k])
      y_count <- round(n_sample_obs * p_sub)
      obs_list[[length(obs_list) + 1L]] <- data.table(
        obs_id = obs_counter,
        loc_id = sprintf("Child_%02d", k),
        cohort = c,
        age_min = as.integer(obs_age),
        dose = as.integer(dose),
        positive = as.integer(y_count),
        sample_n = as.integer(n_sample_obs),
        censored = NA_real_
      )
      obs_counter <- obs_counter + 1L
    }
  }
  observations <- rbindlist(obs_list)

  # 5. Populations Metadata
  populations <- imuGAP:::create_observation_populations(
    observations,
    mode = "snapshot"
  )

  list(
    locations = locations,
    populations = populations,
    observations = observations,
    p0_true = p0_trend,
    delta_true = delta_true,
    mu_true = mu_true,
    w_vec = w_vec,
    K = K,
    sigma = sigma,
    link = link,
    seed = seed
  )
}

# --- Batch Generator from YAML Config -----------------------------------------

generate_populations_from_config <- function(config_path) {
  stopifnot(file.exists(config_path))
  cfg <- yaml::read_yaml(config_path)

  k_grid <- as.integer(cfg$k_grid)
  sigma_grid <- as.numeric(cfg$sigma_grid)
  p0_trend <- as.numeric(cfg$p0_trend)
  n_samples <- as.integer(cfg$n_samples)
  seed_base <- if (!is.null(cfg$seed)) as.integer(cfg$seed) else 42L
  links <- if (!is.null(cfg$links)) {
    as.character(cfg$links)
  } else {
    c("logit", "probit")
  }
  n_sample_obs <- if (!is.null(cfg$n_sample_obs)) {
    as.integer(cfg$n_sample_obs)
  } else {
    1000L
  }
  pop_per_sub <- if (!is.null(cfg$pop_per_sub)) {
    as.integer(cfg$pop_per_sub)
  } else {
    5000L
  }
  obs_age <- if (!is.null(cfg$obs_age)) as.integer(cfg$obs_age) else 2L
  dose <- if (!is.null(cfg$dose)) as.integer(cfg$dose) else 1L

  total_datasets <- length(links) *
    length(k_grid) *
    length(sigma_grid) *
    n_samples
  cat(sprintf(
    "Generating %d synthetic datasets from '%s'...\n",
    total_datasets,
    config_path
  ))
  flush(stdout())

  datasets <- list()
  idx <- 1L

  for (lnk in links) {
    for (K in k_grid) {
      for (sig in sigma_grid) {
        for (s in seq_len(n_samples)) {
          sim_seed <- seed_base + idx * 1000L + s
          ds <- generate_synthetic_dataset(
            K = K,
            sigma = sig,
            p0_trend = p0_trend,
            link = lnk,
            n_sample_obs = n_sample_obs,
            pop_per_sub = pop_per_sub,
            obs_age = obs_age,
            dose = dose,
            seed = sim_seed
          )
          ds$sample_id <- s
          ds$dataset_id <- idx
          datasets[[idx]] <- ds
          idx <- idx + 1L
        }
      }
    }
  }

  output_path <- sub("\\.ya?ml$", ".rds", config_path)
  result_bundle <- list(
    config = cfg,
    config_path = config_path,
    generated_at = Sys.time(),
    n_datasets = length(datasets),
    datasets = datasets
  )

  saveRDS(result_bundle, output_path)
  cat(sprintf(
    "Saved %d synthetic datasets to '%s'.\n",
    length(datasets),
    output_path
  ))
  invisible(output_path)
}

# --- CLI Entrypoint -----------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0L) {
    cat("Usage: Rscript benchmark_synthetic_populations.R <config.yml>\n")
    cat("Defaulting to data-raw/config_stress_test.yml...\n")
    config_file <- "data-raw/config_stress_test.yml"
  } else {
    config_file <- args[1L]
  }

  generate_populations_from_config(config_file)
}
