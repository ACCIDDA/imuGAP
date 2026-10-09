#!/usr/bin/env bash
#SBATCH --job-name=imugap_bench
#SBATCH --array=1-55
#SBATCH --mem=4G
#SBATCH --time=01:00:00
#SBATCH --output=logs/bench_%A_%a.log
#SBATCH --error=logs/bench_%A_%a.err

# Benchmark Array Worker Task for imuGAP
# Executes 1 model across all matching link datasets using multicore chains + within-chain threads.

set -euo pipefail

CONFIG_ARG="${1:-config_strenuous.rds}"
CHAINS="${2:-4}"
THREADS_PER_CHAIN="${3:-1}"
CORES="${4:-4}"
ITER="${5:-300}"
WARMUP="${6:-150}"

mkdir -p results_parts logs

echo "=== [SLURM Task ${SLURM_ARRAY_TASK_ID:-1}] Starting Worker ==="
echo "Host: $(hostname)"
echo "Config: ${CONFIG_ARG}"
echo "Model Index: ${SLURM_ARRAY_TASK_ID:-1}"
echo "MCMC: ${CHAINS} chains, ${THREADS_PER_CHAIN} threads/chain across ${CORES} cores, iter=${ITER}, warmup=${WARMUP}"

Rscript benchmark_inference_runner.R \
  "${CONFIG_ARG}" \
  --model-idx="${SLURM_ARRAY_TASK_ID:-1}" \
  --chains="${CHAINS}" \
  --threads-per-chain="${THREADS_PER_CHAIN}" \
  --cores="${CORES}" \
  --iter="${ITER}" \
  --warmup="${WARMUP}"

echo "=== [SLURM Task ${SLURM_ARRAY_TASK_ID:-1}] Completed Successfully ==="
