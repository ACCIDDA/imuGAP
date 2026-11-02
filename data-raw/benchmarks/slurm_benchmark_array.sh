#!/usr/bin/env bash
#SBATCH --job-name=imugap_bench
# Default array range; slurm_submit.sh dynamically passes --array=1-N matching the config
#SBATCH --array=1-10
#SBATCH --mem-per-cpu=2G
#SBATCH --time=01:00:00
#SBATCH --output=logs/bench_%A_%a.log
#SBATCH --error=logs/bench_%A_%a.err

# Benchmark Array Worker Task for imuGAP
# Executes 1 model across all matching link datasets using multicore chains + within-chain threads.

set -euo pipefail

# Fallback module loading on HPC clusters (e.g. Longleaf / Lmod) if Rscript not in PATH
if ! command -v Rscript &>/dev/null; then
  if [ -f /etc/profile.d/modules.sh ]; then
    source /etc/profile.d/modules.sh
  fi
  module load r 2>/dev/null || module load R 2>/dev/null || true
fi

cd "${SLURM_SUBMIT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"

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
echo "MCMC: ${CHAINS} chains, ${THREADS_PER_CHAIN} threads/chain across ${CORES} cores"
echo "Sampling: iter=${ITER}, warmup=${WARMUP}"

Rscript benchmark_inference_runner.R \
  "${CONFIG_ARG}" \
  --model-idx="${SLURM_ARRAY_TASK_ID:-1}" \
  --chains="${CHAINS}" \
  --threads-per-chain="${THREADS_PER_CHAIN}" \
  --cores="${CORES}" \
  --iter="${ITER}" \
  --warmup="${WARMUP}"

echo "=== [SLURM Task ${SLURM_ARRAY_TASK_ID:-1}] Completed Successfully ==="
