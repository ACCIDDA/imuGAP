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
  library(parallel)
  library(pkgload)
})

# Source compilation helpers for config reading & normalization
source(pkgload::package_file(
  "data-raw",
  "benchmarks",
  "benchmark_compilation.R"
))

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
  obs_ages = c(2L, 4L),
  lambda_true = 3.0,
  dose_schedule = 1L,
  dose = 1L,
  seed = 42L,
  link = c("logit", "probit")
) {
  link <- match.arg(link)
  set.seed(seed)
  C <- length(p0_trend)

  # 1. Locations Table
  loc_ids <- c("Root", sprintf("Child_%03d", seq_len(K)))
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

  # 4. Analytical Force-of-Vaccination / Hazard Uptake Factor
  # In imuGAP, cumulative coverage by age a is:
  # C(a) = (1 - phi_sub) * (1 - exp(-lambda * (a - sched_age)))
  # Multi-age sampling breaks colinearity between non-uptake phi and hazard rate lambda.
  sched_age <- min(dose_schedule)
  uptake_factors <- vapply(
    obs_ages,
    function(a) 1.0 - exp(-lambda_true * (a - sched_age)),
    numeric(1)
  )
  names(uptake_factors) <- as.character(obs_ages)

  # 5. Noiseless Observations (expected integer binomial counts)
  obs_list <- list()
  obs_counter <- 1L
  for (c in seq_len(C)) {
    for (k in seq_len(K)) {
      phi_sub <- inv_link(link_fn(p0_trend[c]) + mu_true[c] + delta_true[k])
      for (a in obs_ages) {
        p_vax <- (1.0 - phi_sub) * uptake_factors[as.character(a)]
        y_count <- as.integer(round(n_sample_obs * p_vax))
        obs_list[[length(obs_list) + 1L]] <- data.table(
          obs_id = obs_counter,
          loc_id = sprintf("Child_%03d", k),
          cohort = c,
          age_min = as.integer(a),
          dose = as.integer(dose),
          positive = as.integer(y_count),
          sample_n = as.integer(n_sample_obs),
          censored = NA_real_
        )
        obs_counter <- obs_counter + 1L
      }
    }
  }
  observations <- rbindlist(obs_list)

  # 6. Populations Metadata
  populations <- create_observation_populations(
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
    lambda_true = lambda_true,
    obs_ages = obs_ages,
    dose_schedule = dose_schedule,
    K = K,
    sigma = sigma,
    link = link,
    seed = seed
  )
}

# --- Batch Generator from YAML Config -----------------------------------------

generate_populations_from_config <- function(config_path) {
  cfg <- read_benchmark_config(config_path)

  k_grid <- cfg$k_grid
  sigma_grid <- cfg$sigma_grid
  p0_trend <- cfg$p0_trend
  n_samples <- cfg$n_samples
  seed_base <- cfg$seed
  links <- cfg$links
  n_sample_obs <- cfg$n_sample_obs
  pop_per_sub <- cfg$pop_per_sub
  obs_ages <- cfg$obs_ages
  lambda_true <- cfg$lambda_true
  dose_schedule <- cfg$dose_schedule
  dose <- cfg$dose

  # Determine parallel worker allocation
  n_cores <- as.integer(
    Sys.getenv("SLURM_CPUS_PER_TASK", Sys.getenv("MC_CORES", "1"))
  )
  if (is.na(n_cores) || n_cores < 1L) {
    n_cores <- min(parallel::detectCores(), 4L)
  }
  if (.Platform$OS.type == "windows") {
    n_cores <- 1L
  }

  total_datasets <- length(links) *
    length(k_grid) *
    length(sigma_grid) *
    n_samples
  cat(sprintf(
    "Generating %d synthetic datasets from '%s' using %d core(s)...\n",
    total_datasets,
    config_path,
    n_cores
  ))
  flush(stdout())

  # Construct exact parameter grid matching deterministic nested-loop ordering
  grid_params <- expand.grid(
    sample_id = seq_len(n_samples),
    sigma = sigma_grid,
    K = k_grid,
    link = links,
    stringsAsFactors = FALSE
  )
  grid_params <- grid_params[
    order(
      match(grid_params$link, links),
      match(grid_params$K, k_grid),
      match(grid_params$sigma, sigma_grid),
      grid_params$sample_id
    ),
    ,
    drop = FALSE
  ]
  grid_params$dataset_id <- seq_len(nrow(grid_params))

  t0_gen <- proc.time()

  # Process in chunks to stream progress updates and manage memory
  chunk_size <- if (total_datasets > 1000L) 1000L else total_datasets
  chunk_cuts <- split(
    seq_len(total_datasets),
    ceiling(seq_len(total_datasets) / chunk_size)
  )

  datasets <- vector("list", total_datasets)

  for (chunk in chunk_cuts) {
    chunk_res <- parallel::mclapply(
      chunk,
      function(idx) {
        row <- grid_params[idx, ]
        sim_seed <- seed_base + idx * 1000L + row$sample_id
        ds <- generate_synthetic_dataset(
          K = row$K,
          sigma = row$sigma,
          p0_trend = p0_trend,
          link = row$link,
          n_sample_obs = n_sample_obs,
          pop_per_sub = pop_per_sub,
          obs_ages = obs_ages,
          lambda_true = lambda_true,
          dose_schedule = dose_schedule,
          dose = dose,
          seed = sim_seed
        )
        ds$sample_id <- row$sample_id
        ds$dataset_id <- idx
        ds
      },
      mc.cores = n_cores
    )

    for (i in seq_along(chunk)) {
      res <- chunk_res[[i]]
      if (inherits(res, "try-error")) {
        stop(sprintf(
          "Error generating dataset %d: %s",
          chunk[i],
          as.character(res)
        ))
      }
      datasets[[chunk[i]]] <- res
    }

    done <- max(chunk)
    elapsed_min <- (proc.time() - t0_gen)[["elapsed"]] / 60
    eta_min <- if (done < total_datasets) {
      (elapsed_min / done) * (total_datasets - done)
    } else {
      0.0
    }
    cat(sprintf(
      "  Progress: %d / %d datasets (%.1f%%) [elapsed: %.1f min, eta: ~%.1f min]\n",
      done,
      total_datasets,
      (done / total_datasets) * 100,
      elapsed_min,
      eta_min
    ))
    flush(stdout())
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

# --- CLI Entrypoint (Invoked by Make) -----------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  config_file <- args[grepl("\\.ya?ml$", args)][1L]
  generate_populations_from_config(config_file)
}
