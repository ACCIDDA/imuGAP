#!/usr/bin/env bash
#SBATCH --job-name=imugap_merge
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=00:15:00
#SBATCH --output=logs/merge_%j.log
#SBATCH --error=logs/merge_%j.err

# Benchmark Reducer / Consolidator Task for imuGAP
# Collects all partition files, audits completeness, and computes summary matrices.

set -euo pipefail

# Fallback module loading on HPC clusters (e.g. Longleaf / Lmod) if Rscript not in PATH
if ! command -v Rscript &>/dev/null; then
  if [ -f /etc/profile.d/modules.sh ]; then
    source /etc/profile.d/modules.sh
  fi
  module load r/4.5.0 2>/dev/null || module load R/4.5.0 2>/dev/null || \
    module load r 2>/dev/null || module load R 2>/dev/null || true
fi

cd "${SLURM_SUBMIT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"

CONFIG_NAME="${1:-strenuous}"
PARTS_DIR="${2:-results_parts}"

mkdir -p logs

echo "=== [SLURM Merge Reducer] Starting Consolidation for '${CONFIG_NAME}' ==="
echo "Host: $(hostname)"
echo "Parts Directory: ${PARTS_DIR}"

Rscript benchmark_merge.R "${CONFIG_NAME}" "${PARTS_DIR}"

echo "=== [SLURM Merge Reducer] Completed Successfully ==="
