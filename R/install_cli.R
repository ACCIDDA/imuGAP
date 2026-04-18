#' @title Install imuGAP CLI to PATH
#'
#' @description Creates a symlink to the bundled CLI script so \code{imugap} is
#' available as a shell command.
#'
#' @param path character; directory to install the symlink into.
#'   Defaults to \code{"~/.local/bin"}.
#'
#' @return Invisible \code{TRUE} on success, errors on failure.
#'
#' @export
install_cli <- function(path = "~/.local/bin") {
  if (.Platform$OS.type == "windows") {
    stop("install_cli() is not supported on Windows.", call. = FALSE)
  }

  script <- system.file("scripts", "imugap.R", package = "imuGAP")
  if (!nzchar(script)) {
    stop("Cannot find bundled CLI script. Is imuGAP installed?", call. = FALSE)
  }

  path <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path, call. = FALSE)
  }

  link <- file.path(path, "imugap")

  if (interactive()) {
    ans <- readline(paste0("Install symlink at ", link, "? [Y/n] "))
    if (!tolower(ans) %in% c("", "y", "yes")) {
      message("Aborted.")
      return(invisible(FALSE))
    }
  }

  unlink(link)
  ok <- file.symlink(script, link)
  if (!ok) {
    stop("Failed to create symlink at ", link, call. = FALSE)
  }
  message("Installed: ", link, " -> ", script)
  message("Ensure ", path, " is on your PATH.")
  return(invisible(TRUE))
}
