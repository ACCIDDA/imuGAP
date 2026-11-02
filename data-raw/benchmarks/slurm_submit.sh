#!/usr/bin/env bash
# Two-stage SLURM orchestrator for imuGAP benchmarks
# 0. Installs/updates the current development version of imuGAP into the active R library
# 1. Prepares data & pre-compiles threaded Stan models on head node
# 2. Submits 55-task array job (CHAINS x THREADS_PER_CHAIN cores per task)
# 3. Submits dependent merge/consolidation job (afterok)

set -euo pipefail

CONFIG_NAME="${1:-strenuous}"
CHAINS="${2:-4}"
THREADS_PER_CHAIN="${3:-1}"
CORES_PER_TASK=$(( CHAINS * THREADS_PER_CHAIN ))
ITER="${4:-300}"
WARMUP="${5:-150}"

CONFIG_BASE=$(echo "${CONFIG_NAME}" | tr '-' '_')
CONFIG_TARGET=$(echo "${CONFIG_NAME}" | tr '_' '-')
CONFIG_RDS="config_${CONFIG_BASE}.rds"

DEFAULT_TIME_LIMIT="01:00:00"
if [[ "${CONFIG_BASE}" == "full" || "${CONFIG_BASE}" == "full_grid" ]]; then
  DEFAULT_TIME_LIMIT="24:00:00"
elif [[ "${CONFIG_BASE}" == "strenuous" ]]; then
  DEFAULT_TIME_LIMIT="02:00:00"
fi
TIME_LIMIT="${6:-${DEFAULT_TIME_LIMIT}}"

echo "================================================================="
echo " imuGAP Distributed Benchmark Pipeline Orchestrator"
echo "================================================================="
echo "Configuration     : ${CONFIG_BASE} (${CONFIG_RDS})"
echo "Parallel Chains   : ${CHAINS} chains"
echo "Threads per Chain : ${THREADS_PER_CHAIN} threads (STAN_NUM_THREADS)"
echo "CPUs per Task     : ${CORES_PER_TASK} cores (${CHAINS} x ${THREADS_PER_CHAIN})"
echo "Iterations        : ${ITER} (warmup: ${WARMUP})"
echo "Walltime Limit    : ${TIME_LIMIT}"
echo "================================================================="

# Stage 0: Guarantee dependencies and current development package are installed in R library
echo ">>> [Stage 0] Synchronizing and installing imuGAP package in R library..."
Rscript -e '
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(user_lib)) {
    r_ver <- sprintf("%s.%s", R.version$major, sub("\\..*", "", R.version$minor))
    user_lib <- file.path(Sys.getenv("HOME"), "R", paste0(R.version$platform, "-library"), r_ver)
  }
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_lib, .libPaths()))

  pkgs <- c("yaml", "data.table", "rstan", "withr", "flexstanr")
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing) > 0L) {
    msg <- sprintf("Installing missing CRAN packages into %s: %s",
      user_lib, paste(missing, collapse = ", "))
    message(msg)
    install.packages(missing, lib = user_lib, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
'
R CMD INSTALL --no-multiarch --with-keep.source ../..

# Stage 1: Build populations and pre-compile unified Stan models
echo ">>> [Stage 1] Pre-flight compilation & population generation..."
if [[ ! -f "${CONFIG_RDS}" ]] && [[ "$(hostname)" =~ (login|ll-login) ]]; then
  echo "NOTICE: Building '${CONFIG_RDS}' on an HPC login node may be throttled."
  echo "        Pre-generating via 'srun -p interact ... make ${CONFIG_TARGET}-pop' is recommended."
fi
make "${CONFIG_TARGET}-pop"
make compile

mkdir -p results_parts logs

# Derive total number of model combinations dynamically
N_MODELS=$(Rscript -e '
  source("benchmark_compilation.R")
  cfg <- read_benchmark_config("'"${CONFIG_RDS}"'")
  cat(nrow(get_valid_model_combinations(cfg)))
')

# Stage 2: Submit SLURM Array Job with exact CPU allocation
echo ">>> [Stage 2] Submitting ${N_MODELS}-task SLURM job array..."
echo "    Allocation: ${CORES_PER_TASK} CPUs/task, walltime ${TIME_LIMIT}"
ARRAY_JOB_ID=$(sbatch \
  ${SLURM_EXTRA_ARGS:-} \
  --parsable \
  --array=1-"${N_MODELS}" \
  --cpus-per-task="${CORES_PER_TASK}" \
  --mem-per-cpu="${MEM_PER_CPU:-2G}" \
  --time="${TIME_LIMIT}" \
  slurm_benchmark_array.sh \
    "${CONFIG_RDS}" \
    "${CHAINS}" \
    "${THREADS_PER_CHAIN}" \
    "${CORES_PER_TASK}" \
    "${ITER}" \
    "${WARMUP}")

echo ">>> Successfully submitted Array Job ID: ${ARRAY_JOB_ID}"

# Stage 3: Submit Dependent Merge Job
echo ">>> [Stage 3] Submitting dependent merge job (afterok:${ARRAY_JOB_ID})..."
MERGE_JOB_ID=$(sbatch \
  ${SLURM_EXTRA_ARGS:-} \
  --parsable \
  --dependency=afterok:"${ARRAY_JOB_ID}" \
  slurm_merge.sh "${CONFIG_BASE}")

echo ">>> Successfully submitted Merge Job ID: ${MERGE_JOB_ID}"
echo "================================================================="
echo "Pipeline submitted to SLURM:"
echo "  - Array Tasks : ${ARRAY_JOB_ID}_[1-${N_MODELS}] (${CORES_PER_TASK} cores each)"
echo "  - Merge Job   : ${MERGE_JOB_ID} (will run automatically when array finishes)"
echo "  - Monitor with: squeue -u \$USER"
echo "================================================================="
