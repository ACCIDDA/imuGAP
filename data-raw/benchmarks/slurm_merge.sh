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

CONFIG_NAME="${1:-strenuous}"
PARTS_DIR="${2:-results_parts}"

mkdir -p logs

echo "=== [SLURM Merge Reducer] Starting Consolidation for '${CONFIG_NAME}' ==="
echo "Host: $(hostname)"
echo "Parts Directory: ${PARTS_DIR}"

Rscript benchmark_merge.R "${CONFIG_NAME}" "${PARTS_DIR}"

echo "=== [SLURM Merge Reducer] Completed Successfully ==="
