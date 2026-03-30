#' @importFrom utils flush.console capture.output
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
  last_value <- min
  i <- min
  if (is.na(width)) width <- getOption("width")
  total <- max - min
  redraw_step <- max(1L, ceiling(max(total, 1) / 100))
  tick_interval <- 0.25

  get_time_str <- function(seconds) {
    seconds <- max(0, round(seconds))
    minutes <- seconds %/% 60
    sec <- seconds %% 60
    sprintf("%02d:%02d", minutes, sec)
  }

  draw <- function(value, now) {
    percent <- if (total > 0) (value - min) / total else 1
    percent <- max(0, min(1, percent))
    completed <- value - min
    total_steps <- max(total, 0)
    elapsed <- now - start_time
    eta <- if (completed > 0 && percent > 0) elapsed * (1 - percent) / percent else NA
    status <- sprintf(
      "%3d%% %d/%d elapsed %s",
      round(percent * 100),
      completed,
      total_steps,
      get_time_str(elapsed)
    )
    if (!is.na(eta)) status <- sprintf("%s eta %s", status, get_time_str(eta))

    bar_width <- max(10, min(30, width - nchar(status) - 6))
    done <- floor(bar_width * percent)
    left <- bar_width - done
    clear()
    cat(sprintf("\r[%s%s] %s", strrep(char, done), strrep(" ", left), status))
    flush.console()
    last_draw <<- now
    last_value <<- value
  }

  update <- function(value) {
    if (!is.finite(value) || value < min || value > max) return()
    i <<- value
    now <- proc.time()[["elapsed"]]
    is_final <- isTRUE(value >= max)
    advanced_enough <- (value - last_value) >= redraw_step
    is_initial <- !is.finite(last_draw)
    if (is_initial || ((now - last_draw) >= 0.1 && advanced_enough) || is_final) {
      draw(value, now)
    }
  }

  tick <- function() {
    if (i >= max) return()
    now <- proc.time()[["elapsed"]]
    if ((now - last_draw) >= tick_interval) {
      draw(i, now)
    }
  }

  clear <- function() {
    cat("\r", strrep(" ", width), "\r", sep = "")
  }

  emit <- function(text, redraw = TRUE) {
    if (!length(text)) return()
    clear()
    cat(paste(text, collapse = "\n"))
    if (!grepl("\n$", text[[length(text)]])) cat("\n")
    if (redraw) draw(i, proc.time()[["elapsed"]])
  }

  kill <- function() cat("\n")
  update(i)
  list(up = update, tick = tick, kill = kill, emit = emit, clear = clear)
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


#' Unpack a task result emitted by a worker wrapper
#'
#' @keywords internal
#' @noRd
.functionals_unwrap_task_result <- function(result) {
  if (is.list(result) && identical(result$ok, TRUE)) {
    return(result$value)
  }

  if (is.list(result) && identical(result$ok, FALSE)) {
    stop(result$message, call. = FALSE)
  }

  if (inherits(result, "try-error")) {
    stop(as.character(result), call. = FALSE)
  }

  result
}


#' Execute tasks on a cluster with exact completion-driven progress
#'
#' @keywords internal
#' @noRd
.functionals_cluster_queue <- function(cl, .x, task_fun, pb_bar = NULL) {
  if (!length(.x)) return(list())

  parallel_ns <- asNamespace("parallel")
  send_call <- get("sendCall", envir = parallel_ns)
  recv_one_result <- get("recvOneResult", envir = parallel_ns)
  is_sock_cluster <- inherits(cl, "SOCKcluster") || all(vapply(cl, inherits, logical(1), what = "SOCKnode"))

  out <- vector("list", length(.x))
  next_idx <- 1L
  n_workers <- min(length(cl), length(.x))
  socklist <- if (is_sock_cluster) lapply(cl, function(x) x$con) else NULL

  for (worker_idx in seq_len(n_workers)) {
    send_call(cl[[worker_idx]], task_fun, list(.x[[next_idx]]), tag = next_idx)
    next_idx <- next_idx + 1L
  }

  completed <- 0L
  while (completed < length(.x)) {
    res <- if (is_sock_cluster) {
      ready <- socketSelect(socklist, timeout = 0.25)
      if (!any(ready)) {
        if (!is.null(pb_bar)) pb_bar$tick()
        next
      }
      node <- which.max(ready)
      value <- unserialize(socklist[[node]])
      list(value = value$value, node = node, tag = value$tag)
    } else {
      recv_one_result(cl)
    }

    out[[res$tag]] <- .functionals_unwrap_task_result(res$value)
    completed <- completed + 1L
    if (!is.null(pb_bar)) pb_bar$up(completed)

    if (next_idx <= length(.x)) {
      send_call(cl[[res$node]], task_fun, list(.x[[next_idx]]), tag = next_idx)
      next_idx <- next_idx + 1L
    }
  }

  out
}


#' Execute tasks with mcparallel and exact completion-driven progress
#'
#' @keywords internal
#' @noRd
.functionals_multicore_queue <- function(.x, task_fun, ncores, pb_bar = NULL) {
  if (!length(.x)) return(list())

  parallel_ns <- asNamespace("parallel")
  cleanup <- get("cleanup", envir = parallel_ns)
  on.exit(cleanup(kill = TRUE, detach = FALSE), add = TRUE)

  out <- vector("list", length(.x))
  jobs <- list()
  next_idx <- 1L
  max_jobs <- min(ncores, length(.x))

  launch_job <- function(idx) {
    parallel::mcparallel(task_fun(.x[[idx]]), name = as.character(idx), silent = TRUE)
  }

  while (length(jobs) < max_jobs && next_idx <= length(.x)) {
    jobs[[as.character(next_idx)]] <- launch_job(next_idx)
    next_idx <- next_idx + 1L
  }

  completed <- 0L
  while (completed < length(.x)) {
    res <- parallel::mccollect(jobs, wait = FALSE, timeout = 0.1)
    if (is.null(res)) {
      if (!is.null(pb_bar)) pb_bar$tick()
      next
    }

    for (name in names(res)) {
      out[[as.integer(name)]] <- .functionals_unwrap_task_result(res[[name]])
      jobs[[name]] <- NULL
      completed <- completed + 1L
      if (!is.null(pb_bar)) pb_bar$up(completed)

      if (next_idx <= length(.x)) {
        jobs[[as.character(next_idx)]] <- launch_job(next_idx)
        next_idx <- next_idx + 1L
      }
    }
  }

  out
}
