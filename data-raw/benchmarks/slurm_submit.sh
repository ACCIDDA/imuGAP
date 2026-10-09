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

CONFIG_RDS="config_${CONFIG_NAME}.rds"

echo "================================================================="
echo " imuGAP Distributed Benchmark Pipeline Orchestrator"
echo "================================================================="
echo "Configuration     : ${CONFIG_NAME} (${CONFIG_RDS})"
echo "Parallel Chains   : ${CHAINS} chains"
echo "Threads per Chain : ${THREADS_PER_CHAIN} threads (STAN_NUM_THREADS)"
echo "CPUs per Task     : ${CORES_PER_TASK} cores (${CHAINS} x ${THREADS_PER_CHAIN})"
echo "Iterations        : ${ITER} (warmup: ${WARMUP})"
echo "================================================================="

# Stage 0: Guarantee current development package is installed in R library
echo ">>> [Stage 0] Synchronizing and installing imuGAP package in R library..."
R CMD INSTALL --no-multiarch --with-keep.source ../..

# Stage 1: Build populations and pre-compile unified Stan models
echo ">>> [Stage 1] Pre-flight compilation & population generation..."
make "${CONFIG_NAME}-pop"
make "${CONFIG_NAME}-compile"

mkdir -p results_parts logs

# Stage 2: Submit SLURM Array Job with exact CPU allocation
echo ">>> [Stage 2] Submitting 55-task SLURM job array (${CORES_PER_TASK} CPUs/task)..."
ARRAY_JOB_ID=$(sbatch \
  --parsable \
  --cpus-per-task="${CORES_PER_TASK}" \
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
MERGE_JOB_ID=$(sbatch --parsable --dependency=afterok:"${ARRAY_JOB_ID}" slurm_merge.sh "${CONFIG_NAME}")

echo ">>> Successfully submitted Merge Job ID: ${MERGE_JOB_ID}"
echo "================================================================="
echo "Pipeline submitted to SLURM:"
echo "  - Array Tasks : ${ARRAY_JOB_ID}_[1-55] (${CORES_PER_TASK} cores each)"
echo "  - Merge Job   : ${MERGE_JOB_ID} (will run automatically when array finishes)"
echo "  - Monitor with: squeue -u \$USER"
echo "================================================================="
