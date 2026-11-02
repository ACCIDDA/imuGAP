#!/usr/bin/env bash
# HPC Remote Orchestrator for imuGAP Benchmarks
# Allows local terminal interaction with the remote HPC cluster (e.g. Longleaf)
# to sync code, prepare data, submit SLURM jobs, monitor status, and fetch results.

set -euo pipefail

# --- Configuration & Defaults -------------------------------------------------

HPC_HOST="${HPC_HOST:-longleaf}"
HPC_DIR="${HPC_DIR:-~/imuGAP}"
R_MODULE="${R_MODULE:-r/4.4.0}"
SYNC_HOST="${HPC_DM_HOST:-longleaf-dm}"

# Determine repository root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Verify connection host
get_transfer_host() {
  if ssh -q -o BatchMode=yes -o ConnectTimeout=3 "${SYNC_HOST}" exit 2>/dev/null; then
    echo "${SYNC_HOST}"
  else
    echo "${HPC_HOST}"
  fi
}

check_hpc_connection() {
  local timeout="${1:-5}"
  if ! ssh -q -o BatchMode=yes -o ConnectTimeout="${timeout}" "${HPC_HOST}" exit 2>/dev/null; then
    echo "Error: Cannot reach host '${HPC_HOST}' (connection timed out or refused)." >&2
    echo "" >&2
    echo "Checklist for UNC Longleaf:" >&2
    echo "  1. Are you connected to the UNC VPN (vpn.unc.edu via Cisco Secure Client)?" >&2
    echo "     Longleaf port 22 is only accessible from within the UNC campus network." >&2
    echo "  2. If using WSL2, test if Windows can reach Longleaf in PowerShell:" >&2
    echo "       ssh ${HPC_HOST}" >&2
    echo "  3. Verify SSH key and host entry in ~/.ssh/config:" >&2
    echo "       ssh -v ${HPC_HOST}" >&2
    exit 1
  fi
}

usage() {
  cat <<EOF
Usage: $(basename "$0") <command> [arguments]

Commands:
  sync                 Sync local imuGAP repository to remote HPC (${HPC_HOST}:${HPC_DIR})
  prep <config>        Run data prep & model compilation via a SLURM compute job
  submit <config> [C] [T] [I] [W]
                       Sync local files and submit SLURM benchmark array + merge job
                         C: chains (default: 4)
                         T: threads per chain (default: 1)
                         I: sampling iterations (default: 300)
                         W: warmup iterations (default: 150)
  status               Display active SLURM benchmark jobs (squeue)
  logs [filter]        Display latest SLURM logs from remote cluster
  fetch <config>       Download consolidated benchmark results (.rds and .csv) to local
  cancel               Cancel active imuGAP SLURM jobs on remote cluster
  ssh                  Open interactive shell on remote HPC cluster

Environment variables:
  HPC_HOST             Remote SSH host alias (default: 'longleaf')
  HPC_DIR              Target directory on remote host (default: '~/imuGAP')
  R_MODULE             R environment module on remote host (default: 'r/4.4.0')
  SLURM_EXTRA_ARGS     Additional sbatch arguments (e.g. '-p jlessler' or '--qos=...')

Examples:
  ./hpc_remote.sh sync
  ./hpc_remote.sh prep full
  ./hpc_remote.sh submit full 4 1 300 150
  ./hpc_remote.sh status
  ./hpc_remote.sh fetch full
EOF
}

# --- Subcommand Implementations -----------------------------------------------

cmd_sync() {
  check_hpc_connection
  local target_host
  target_host=$(get_transfer_host)
  echo ">>> Syncing local repository to ${target_host}:${HPC_DIR}..."

  # Ensure remote parent directory exists
  ssh "${HPC_HOST}" "mkdir -p '${HPC_DIR}'"

  # Sync repository excluding build artifacts and large results
  rsync -avz --delete \
    --exclude=".git/" \
    --exclude=".Rproj.user/" \
    --exclude="src/*.o" \
    --exclude="src/*.so" \
    --exclude="src/*.dll" \
    --exclude="*.tar.gz" \
    --exclude="data-raw/benchmarks/logs/" \
    --exclude="data-raw/benchmarks/results_parts/" \
    --exclude="data-raw/benchmarks/results_*.rds" \
    --exclude="data-raw/benchmarks/results_*.csv" \
    "${REPO_ROOT}/" "${target_host}:${HPC_DIR}/"

  echo ">>> Code synchronization complete."
}

cmd_prep() {
  check_hpc_connection
  local config="${1:-full}"
  local config_target
  config_target=$(echo "${config}" | tr '_' '-')

  echo ">>> Submitting data prep & compilation job on ${HPC_HOST} for '${config}'..."
  ssh "${HPC_HOST}" "bash -l -c '
    set -euo pipefail
    module load ${R_MODULE} 2>/dev/null || true
    cd \"${HPC_DIR}/data-raw/benchmarks\"
    mkdir -p logs benchobjs
    sbatch \
      ${SLURM_EXTRA_ARGS:-} \
      --job-name=imugap_prep \
      --cpus-per-task=4 \
      --mem=8G \
      --time=01:00:00 \
      --output=logs/prep_%j.log \
      --error=logs/prep_%j.err \
      --wrap=\"make ${config_target}-pop && make compile\"
  '"
  echo ">>> Prep job submitted. Monitor progress with: ./hpc_remote.sh status"
}

cmd_submit() {
  local config="${1:-full}"
  shift || true
  local chains="${1:-4}"
  local threads="${2:-1}"
  local iter="${3:-300}"
  local warmup="${4:-150}"

  # Step 1: Sync local changes first
  cmd_sync

  # Step 2: Submit via remote slurm_submit.sh
  echo ">>> Triggering SLURM benchmark pipeline on ${HPC_HOST}..."
  ssh -t "${HPC_HOST}" "bash -l -c '
    set -euo pipefail
    module load ${R_MODULE} 2>/dev/null || true
    cd \"${HPC_DIR}/data-raw/benchmarks\"
    SLURM_EXTRA_ARGS=\"${SLURM_EXTRA_ARGS:-}\" \
      ./slurm_submit.sh \"${config}\" \"${chains}\" \"${threads}\" \"${iter}\" \"${warmup}\"
  '"
}

cmd_status() {
  check_hpc_connection
  echo ">>> Checking SLURM queue on ${HPC_HOST}..."
  ssh "${HPC_HOST}" "squeue -u \$USER --format=\"%.18i %.9P %.20j %.8u %.2t %.10M %.6D %R\""
}

cmd_logs() {
  check_hpc_connection
  local filter="${1:-}"
  echo ">>> Fetching latest logs from ${HPC_HOST}..."
  if [ -n "${filter}" ]; then
    ssh "${HPC_HOST}" "bash -l -c '
      cd \"${HPC_DIR}/data-raw/benchmarks/logs\" 2>/dev/null || exit 0
      ls -lt *${filter}* 2>/dev/null | head -n 10
    '"
  else
    ssh "${HPC_HOST}" "bash -l -c '
      cd \"${HPC_DIR}/data-raw/benchmarks/logs\" 2>/dev/null || exit 0
      echo \"=== Recent Log Files ===\"
      ls -lt *.log *.err 2>/dev/null | head -n 15
      echo \"\"
      echo \"=== Tail of Most Recent Log ===\"
      latest=\$(ls -t *.log 2>/dev/null | head -n 1)
      if [ -n \"\${latest}\" ]; then
        echo \"--- \${latest} ---\"
        tail -n 25 \"\${latest}\"
      fi
    '"
  fi
}

cmd_fetch() {
  check_hpc_connection
  local config="${1:-full}"
  local config_base
  config_base=$(echo "${config}" | tr '-' '_')
  local target_host
  target_host=$(get_transfer_host)

  echo ">>> Downloading benchmark results for '${config_base}' from ${target_host}..."
  rsync -avz --progress \
    "${target_host}:${HPC_DIR}/data-raw/benchmarks/results_${config_base}*" \
    "${SCRIPT_DIR}/"

  echo ">>> Results fetch complete. Downloaded artifacts:"
  ls -lh "${SCRIPT_DIR}/results_${config_base}"* 2>/dev/null || true
}

cmd_cancel() {
  check_hpc_connection
  echo ">>> Cancelling active imuGAP jobs on ${HPC_HOST}..."
  ssh "${HPC_HOST}" "scancel -u \$USER --name=imugap_bench,imugap_merge,imugap_prep"
  echo ">>> Cancel command issued."
}

cmd_ssh() {
  echo ">>> Connecting interactive SSH session to ${HPC_HOST}..."
  ssh -t "${HPC_HOST}" "cd '${HPC_DIR}/data-raw/benchmarks' && exec bash -l"
}

# --- Main Dispatcher ----------------------------------------------------------

CMD="${1:-help}"
shift || true

case "${CMD}" in
  sync) cmd_sync ;;
  prep) cmd_prep "$@" ;;
  submit) cmd_submit "$@" ;;
  status) cmd_status ;;
  logs) cmd_logs "$@" ;;
  fetch) cmd_fetch "$@" ;;
  cancel) cmd_cancel ;;
  ssh) cmd_ssh ;;
  help|--help|-h) usage ;;
  *)
    echo "Error: Unknown command '${CMD}'" >&2
    usage >&2
    exit 1
    ;;
esac

