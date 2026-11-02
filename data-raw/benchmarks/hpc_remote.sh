#!/usr/bin/env bash
# HPC Remote Orchestrator for imuGAP Benchmarks
# Allows local terminal interaction with the remote HPC cluster (e.g. Longleaf)
# to bootstrap environment, sync code, prepare data, submit SLURM jobs, and fetch results.

set -euo pipefail

# --- Configuration & Defaults -------------------------------------------------

HPC_HOST="${HPC_HOST:-longleaf}"
HPC_DIR="${HPC_DIR:-imuGAP}"
# Normalize remote path: strip any leading ~/ for safe remote shell expansion
HPC_REL_DIR="${HPC_DIR#~/}"
R_MODULE="${R_MODULE:-r/4.5.0}"
SYNC_HOST="${HPC_DM_HOST:-longleaf-dm}"

# Determine repository root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Verify connection host
get_transfer_host() {
  local ssh_opts=(-q -o BatchMode=yes -o StrictHostKeyChecking=accept-new -o ConnectTimeout=3)
  if ssh "${ssh_opts[@]}" "${SYNC_HOST}" exit 2>/dev/null; then
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
  setup                Bootstrap remote HPC: clone/init repo, sync code & R dependencies
  sync                 Sync local imuGAP repository to remote HPC (~/${HPC_REL_DIR})
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
  HPC_DIR              Target directory on remote host (default: 'imuGAP')
  R_MODULE             R environment module on remote host (default: 'r/4.5.0')
  SLURM_EXTRA_ARGS     Additional sbatch arguments (e.g. '-p jlessler' or '--qos=...')

Examples:
  ./hpc_remote.sh setup
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
  echo ">>> Syncing local repository to ${target_host}:~/${HPC_REL_DIR}..."

  # Ensure remote target directory exists
  ssh "${HPC_HOST}" "mkdir -p \"\${HOME}/${HPC_REL_DIR}\""

  # Sync repository excluding build artifacts and large results
  rsync -avz --delete \
    -e "ssh -o BatchMode=yes -o StrictHostKeyChecking=accept-new" \
    --exclude=".git/" \
    --exclude=".Rproj.user/" \
    --exclude="*.Rcheck/" \
    --exclude="imuGAP.Rcheck/" \
    --exclude="src/*.o" \
    --exclude="src/*.so" \
    --exclude="src/*.dll" \
    --exclude="src/*.gcno" \
    --exclude="src/*.gcda" \
    --exclude="*.tar.gz" \
    --exclude="data-raw/benchmarks/logs/" \
    --exclude="data-raw/benchmarks/results_parts/" \
    --exclude="data-raw/benchmarks/results_*.rds" \
    --exclude="data-raw/benchmarks/results_*.csv" \
    --exclude="data-raw/benchmarks/config_*.rds" \
    --exclude="data-raw/benchmarks/benchobjs/" \
    "${REPO_ROOT}/" "${target_host}:${HPC_REL_DIR}/"

  echo ">>> Code synchronization complete."
}

cmd_setup() {
  check_hpc_connection
  local branch
  branch=$(git -C "${REPO_ROOT}" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "main")
  local remote_url
  remote_url=$(git -C "${REPO_ROOT}" config --get remote.origin.url 2>/dev/null || \
    echo "git@github.com:ACCIDDA/imuGAP.git")

  echo "================================================================="
  echo " imuGAP HPC Remote Environment Initializer"
  echo "================================================================="
  echo "Target Host   : ${HPC_HOST}"
  echo "Remote Path   : ~/${HPC_REL_DIR}"
  echo "Active Branch : ${branch}"
  echo "R Module      : ${R_MODULE}"
  echo "================================================================="

  # Step 1: Ensure directory or git clone on remote
  echo ">>> [1/4] Checking remote repository on ${HPC_HOST}..."
  ssh "${HPC_HOST}" "bash -l -c '
    set -euo pipefail
    TARGET=\"\${HOME}/${HPC_REL_DIR}\"
    if [ ! -d \"\${TARGET}\" ]; then
      echo \">>> Directory \${TARGET} does not exist. Initializing...\"
      if git clone -b \"${branch}\" \"${remote_url}\" \"\${TARGET}\" 2>/dev/null; then
        echo \">>> Successfully cloned repository (branch: ${branch}).\"
      else
        echo \">>> Git clone not reachable; initializing directory for rsync.\"
        mkdir -p \"\${TARGET}\"
      fi
    else
      echo \">>> Found existing directory at \${TARGET}.\"
    fi
  '"

  # Step 2: Sync workspace code via rsync
  echo ">>> [2/4] Synchronizing active workspace files to ${HPC_HOST}..."
  cmd_sync

  # Step 3: Verify R environment, load module, check/install dependencies
  echo ">>> [3/4] Verifying R environment and dependencies on ${HPC_HOST}..."
  ssh "${HPC_HOST}" "bash -l -c '
    set -euo pipefail
    TARGET=\"\${HOME}/${HPC_REL_DIR}\"
    module load ${R_MODULE} 2>/dev/null || module load r 2>/dev/null || true
    if ! command -v Rscript &>/dev/null; then
      echo \"Error: Rscript not found on ${HPC_HOST}. Verify module ${R_MODULE}.\" >&2
      exit 1
    fi
    mkdir -p \"\${TARGET}/data-raw/benchmarks/logs\" \"\${TARGET}/data-raw/benchmarks/benchobjs\"

    Rscript -e \"
      user_lib <- Sys.getenv(\\\"R_LIBS_USER\\\")
      if (!nzchar(user_lib)) {
        r_ver <- sprintf(\\\"%s.%s\\\", R.version\\\$major,
          sub(\\\"\\\\\\\\..*\\\", \\\"\\\", R.version\\\$minor))
        user_lib <- file.path(Sys.getenv(\\\"HOME\\\"), \\\"R\\\",
          paste0(R.version\\\$platform, \\\"-library\\\"), r_ver)
      }
      dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
      .libPaths(c(user_lib, .libPaths()))

      pkgs <- c(\\\"yaml\\\", \\\"data.table\\\", \\\"rstan\\\", \\\"withr\\\", \\\"flexstanr\\\")
      missing <- setdiff(pkgs, rownames(installed.packages()))
      if (length(missing) > 0L) {
        msg <- sprintf(\\\"Installing missing CRAN packages into %s: %s\\\",
          user_lib, paste(missing, collapse = \\\", \\\"))
        message(msg)
        install.packages(
          missing,
          repos = \\\"https://cloud.r-project.org\\\",
          lib = user_lib,
          quiet = TRUE
        )
      }
    \"
  '"

  # Step 4: Install/update imuGAP into remote R library
  echo ">>> [4/4] Installing active development version of imuGAP on ${HPC_HOST}..."
  ssh "${HPC_HOST}" "bash -l -c '
    set -euo pipefail
    TARGET=\"\${HOME}/${HPC_REL_DIR}\"
    module load ${R_MODULE} 2>/dev/null || module load r 2>/dev/null || true
    R CMD INSTALL --no-multiarch --with-keep.source \"\${TARGET}\"
  '"

  echo "================================================================="
  echo ">>> HPC remote environment initialized successfully!"
  echo "================================================================="
}

ensure_remote_setup() {
  local check_result
  check_result=$(ssh -o BatchMode=yes -o ConnectTimeout=5 "${HPC_HOST}" "bash -l -c '
    TARGET=\"\${HOME}/${HPC_REL_DIR}\"
    if [ -d \"\${TARGET}/data-raw/benchmarks\" ]; then
      echo \"OK\"
    else
      echo \"MISSING\"
    fi
  '" 2>/dev/null || echo "MISSING")

  if [ "${check_result}" != "OK" ]; then
    echo ">>> Remote directory ~/${HPC_REL_DIR} is not initialized. Running setup..."
    cmd_setup
  else
    # Directory exists; sync workspace changes to remote
    cmd_sync
  fi
}

cmd_prep() {
  check_hpc_connection
  ensure_remote_setup

  local config="${1:-full}"
  local config_target
  config_target=$(echo "${config}" | tr '_' '-')
  local prep_cpus="${PREP_CPUS:-4}"
  local prep_time="${PREP_TIME_LIMIT:-02:00:00}"

  echo ">>> Submitting data prep & compilation job on ${HPC_HOST} for '${config}'..."
  echo "    Allocation: ${prep_cpus} CPUs, walltime ${prep_time}"
  ssh "${HPC_HOST}" "bash -l -c '
    set -euo pipefail
    TARGET=\"\${HOME}/${HPC_REL_DIR}\"
    module load ${R_MODULE} 2>/dev/null || module load r 2>/dev/null || true
    cd \"\${TARGET}/data-raw/benchmarks\"
    mkdir -p logs benchobjs
    sbatch \
      ${SLURM_EXTRA_ARGS:-} \
      --job-name=imugap_prep \
      --cpus-per-task=${prep_cpus} \
      --mem=8G \
      --time=${prep_time} \
      --output=logs/prep_%j.log \
      --error=logs/prep_%j.err \
      --wrap=\"make ${config_target}-pop && make compile\"
  '"
  echo ">>> Prep job submitted. Monitor progress with: ./hpc_remote.sh status"
}

cmd_submit() {
  check_hpc_connection
  ensure_remote_setup

  local config="${1:-full}"
  shift || true
  local chains="${1:-4}"
  local threads="${2:-1}"
  local iter="${3:-300}"
  local warmup="${4:-150}"

  echo ">>> Triggering SLURM benchmark pipeline on ${HPC_HOST}..."
  ssh -t "${HPC_HOST}" "bash -l -c '
    set -euo pipefail
    TARGET=\"\${HOME}/${HPC_REL_DIR}\"
    module load ${R_MODULE} 2>/dev/null || module load r 2>/dev/null || true
    cd \"\${TARGET}/data-raw/benchmarks\"
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
      TARGET=\"\${HOME}/${HPC_REL_DIR}\"
      cd \"\${TARGET}/data-raw/benchmarks/logs\" 2>/dev/null || exit 0
      ls -lt *${filter}* 2>/dev/null | head -n 10
    '"
  else
    ssh "${HPC_HOST}" "bash -l -c '
      TARGET=\"\${HOME}/${HPC_REL_DIR}\"
      cd \"\${TARGET}/data-raw/benchmarks/logs\" 2>/dev/null || exit 0
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
    "${target_host}:${HPC_REL_DIR}/data-raw/benchmarks/results_${config_base}*" \
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
  ssh -t "${HPC_HOST}" "bash -l -c '
    TARGET=\"\${HOME}/${HPC_REL_DIR}\"
    cd \"\${TARGET}/data-raw/benchmarks\" 2>/dev/null || true
    exec bash -l
  '"
}

# --- Main Dispatcher ----------------------------------------------------------

CMD="${1:-help}"
shift || true

case "${CMD}" in
  setup) cmd_setup ;;
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
