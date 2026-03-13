#' @importFrom utils flush.console
NULL


#' Validate and normalize fapply arguments
#'
#' Internal helper to check and coerce inputs for `fapply()`.
#'
#' @keywords internal
#' @noRd
.check_fapply_args <- function(.x, .f, ncores, pb) {
  # check .x is atomic or list-like
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)

  # empty input warning
  if (length(.x) == 0L) {
    warning("Input `.x` is empty. Returning empty list.")
    return(list(result = list(), .x = .x))
  }

  # check .f
  .f <- match.fun(.f)

  # check ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) || ncores < 1 || is.na(ncores)) {
      warning("`ncores` must be a positive integer. Defaulting to sequential.")
      ncores <- 1
    }
  } else {
    ncores <- 1
  }

  # windows fallback
  if (.Platform$OS.type == "windows" && ncores > 1) {
    warning("Parallel execution on Windows uses clusters. Falling back to sequential for consistency.")
    ncores <- 1
  }

  # check pb
  if (!is.logical(pb) || length(pb) != 1) {
    warning("`pb` must be TRUE or FALSE. Disabling progress bar.")
    pb <- FALSE
  }

  list(result = NULL, .x = .x, .f = .f, ncores = ncores, pb = pb)
}


#' Create a custom console progress bar
#'
#' Internal utility to build a simple console-based progress bar with estimated time remaining.
#' Used inside `fapply()` when `pb = TRUE`.
#'
#' @keywords internal
#' @noRd
functionals_progress_bar <- function(min = 0, max = 1, style = 1, width = NA, char = "=") {
  start_time <- proc.time()[["elapsed"]]
  last_draw <- -Inf
  i <- min
  if (is.na(width)) width <- getOption("width")

  get_time_str <- function(seconds) {
    seconds <- max(0, round(seconds))
    minutes <- seconds %/% 60
    sec <- seconds %% 60
    sprintf("%02d:%02d", minutes, sec)
  }

  draw <- function(value, now) {
    total <- max - min
    percent <- if (total > 0) (value - min) / total else 1
    percent <- max(0, min(1, percent))
    completed <- value - min
    total_steps <- max(total, 0)
    elapsed <- now - start_time
    eta <- if (percent > 0 && elapsed >= 3) elapsed * (1 - percent) / percent else NA
    status <- sprintf("%3d%% %d/%d %s", round(percent * 100), completed, total_steps, get_time_str(elapsed))
    if (!is.na(eta)) status <- sprintf("%s eta %s", status, get_time_str(eta))

    bar_width <- max(10, min(30, width - nchar(status) - 6))
    done <- floor(bar_width * percent)
    left <- bar_width - done
    cat(sprintf("\r[%s%s] %s", strrep(char, done), strrep(" ", left), status))
    flush.console()
    last_draw <<- now
  }

  update <- function(value) {
    if (!is.finite(value) || value < min || value > max) return()
    i <<- value
    now <- proc.time()[["elapsed"]]
    is_final <- isTRUE(value >= max)
    if ((now - last_draw) >= 0.1 || is_final) {
      draw(value, now)
    }
  }

  clear <- function() {
    cat("\r", strrep(" ", width), "\r", sep = "")
  }

  emit <- function(text) {
    if (!length(text)) return()
    clear()
    cat(paste(text, collapse = "\n"))
    if (!grepl("\n$", text[[length(text)]])) cat("\n")
    draw(i, proc.time()[["elapsed"]])
  }

  kill <- function() cat("\n")
  update(i)
  list(up = update, kill = kill, emit = emit, clear = clear)
}


#' Split indices for chunked parallel processing
#'
#' Helper function to divide a sequence of indices into balanced chunks for parallel processing.
#' Used internally by `fapply()` to manage load balancing.
#'
#' @param nx Integer. Total number of elements to split.
#' @param ncl Integer. Number of cores (chunks).
#' @param nout Approximate number of output groups for progress bar feedback.
#'
#' @return A list of integer vectors, each representing a chunk of indices.
#'
#' @keywords internal
#' @noRd
splitpb <- function(nx, ncl, nout = 100) {
  i <- seq_len(nx)
  if (ncl == 0L) return(list())
  k <- max(1L, ceiling(ceiling(nx / ncl) / nout))
  g <- 1L + (i - 1L) %/% as.integer(ncl * k)
  structure(split(i, g), names = NULL)
}
