#' Apply a function over a list or vector with optional parallelism and progress
#'
#' A lightweight and fast version of `lapply()` with support for multicore (Unix) and snow-style
#' clusters via `parallel`, with internal progress bar tracking and message suppression.
#' When `pb = TRUE`, progress advances when individual tasks complete, including in the
#' parallel paths, so counts reflect completed jobs rather than internal chunk boundaries.
#' Progress rendering is throttled for large workloads to keep console redraw overhead bounded,
#' while status output reports elapsed time and estimated time remaining.
#'
#' @param .x A list or atomic vector.
#' @param .f Function to apply.
#' @param ncores Number of cores to use (default: 1 = sequential).
#' @param pb Show progress bar? (default: FALSE). When enabled, progress is driven by
#' completed tasks. In parallel mode, updates occur as workers return results.
#' @param cl A cluster object (from parallel::makeCluster), or integer for core count.
#' @param load_balancing Logical. Use `parLapplyLB` if `TRUE` (default: `FALSE`).
#' @param .on_error How to handle an error thrown by `.f`. `"stop"` (default)
#'   aborts the whole run, matching base `lapply()`. `"pass"` keeps going and
#'   stores an object of class `"functionals_error"` in the failed position
#'   (inspect with `conditionMessage()`), then emits a one-line summary
#'   warning. `"fill"` replaces each failed result with `.fill`. The behaviour
#'   is identical whether execution is sequential or parallel.
#' @param .fill Replacement value used for failed elements when
#'   `.on_error = "fill"`. Default `NULL`.
#' @param .seed Optional single number. When supplied, task `i` is run with its
#'   own L'Ecuyer-CMRG random-number stream, so results are reproducible and
#'   independent of `ncores` or task-completion order. The caller's global RNG
#'   state is restored on exit.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results.
#'
#' @examples
#' # Basic usage (sequential)
#' fapply(1:5, sqrt)
#'
#' # With progress bar (sequential)
#' fapply(1:5, function(x) { Sys.sleep(0.1); x^2 }, pb = TRUE)
#'
#' # Multicore on Unix (if available)
#' \donttest{
#' if (.Platform$OS.type != "windows") {
#'   fapply(1:10, sqrt, ncores = 2)
#' }
#' }
#'
#' # With user-created cluster (portable across platforms)
#' \donttest{
#' cl <- parallel::makeCluster(2)
#' fapply(1:10, sqrt, cl = cl)
#' parallel::stopCluster(cl)
#' }
#'
#' # Heavy computation example with exact completion-driven progress
#' \donttest{
#' heavy_fn <- function(x) { Sys.sleep(0.05); x^2 }
#' fapply(1:20, heavy_fn, ncores = 2, pb = TRUE)
#' }
#'
#' @export

fapply <- function(.x, .f, ncores = 1, pb = FALSE, cl = NULL, load_balancing = TRUE,
                   .on_error = c("stop", "pass", "fill"), .fill = NULL, .seed = NULL, ...) {
  .f <- match.fun(.f)
  .on_error <- match.arg(.on_error)
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)
  if (!length(.x)) return(list())

  ncores <- .norm_ncores(ncores)

  # per-task RNG streams (rewrites .x -> seq_along, .f -> stream-installing wrapper)
  if (!is.null(.seed)) {
    .rng0 <- .rng_snapshot()
    on.exit(.rng_restore(.rng0), add = TRUE)
  }
  sw <- .seed_wrap(.x, .f, .seed)
  .x <- sw$.x
  .f <- sw$.f
  orig_names <- sw$nm
  # capture .f errors uniformly across every back end
  .f <- .error_wrap(.f, .on_error, .fill)

  is_windows <- .Platform$OS.type == "windows"
  use_parallel <- isTRUE(ncores > 1L)

  # disable crashpad messages
  Sys.setenv(CHROME_CRASHPAD_PIPE_NAME = "disable")

  out <- .fapply_run(
    .x, .f, ncores = ncores, pb = pb, cl = cl,
    load_balancing = load_balancing, is_windows = is_windows,
    use_parallel = use_parallel, ...
  )

  if (!is.null(orig_names)) names(out) <- orig_names
  if (identical(.on_error, "pass")) .warn_failures(out)
  out
}

#' Internal dispatch for fapply's execution back ends
#' @keywords internal
#' @noRd
.fapply_run <- function(.x, .f, ncores, pb, cl, load_balancing, is_windows, use_parallel, ...) {

  # sequential fallback
  if (!use_parallel && is.null(cl)) {
    if (pb) {
      pb_bar <- functionals_progress_bar(min = 0, max = length(.x))
      on.exit(pb_bar$kill(), add = TRUE)
      out <- vector("list", length(.x))
      for (i in seq_along(.x)) {
        captured_messages <- character()
        result <- withCallingHandlers(
          capture.output(
            out[[i]] <- .f(.x[[i]], ...),
            type = "output"
          ),
          message = function(m) {
            captured_messages <<- c(captured_messages, conditionMessage(m))
            invokeRestart("muffleMessage")
          }
        )
        emitted_text <- c(captured_messages, result)
        if (length(emitted_text)) {
          if (i < length(.x)) {
            pb_bar$up(i)
            pb_bar$emit(emitted_text)
          } else {
            pb_bar$emit(emitted_text, redraw = FALSE)
            pb_bar$up(i)
          }
        } else {
          pb_bar$up(i)
        }
      }
      return(out)
    } else {
      return(lapply(.x, .f, ...))
    }
  }

  # if user passed a cluster
  if (inherits(cl, "cluster")) {
    PAR_FUN <- if (load_balancing) parallel::parLapplyLB else parallel::parLapply
    if (pb) {
      pb_bar <- functionals_progress_bar(min = 0, max = length(.x))
      on.exit(pb_bar$kill(), add = TRUE)
      task_fun <- function(item) {
        tryCatch(
          list(ok = TRUE, value = .f(item, ...)),
          error = function(e) list(ok = FALSE, message = conditionMessage(e), call = conditionCall(e))
        )
      }
      return(.functionals_cluster_queue(cl, .x, task_fun, pb_bar = pb_bar))
    } else {
      return(PAR_FUN(cl, .x, .f, ...))
    }
  }

  # if user requested multicore (Unix only)
  if (!is_windows && is.null(cl)) {
    if (pb) {
      pb_bar <- functionals_progress_bar(min = 0, max = length(.x))
      on.exit(pb_bar$kill(), add = TRUE)
      task_fun <- function(item) {
        tryCatch(
          list(
            ok = TRUE,
            value = suppressWarnings(suppressMessages(.f(item, ...)))
          ),
          error = function(e) list(ok = FALSE, message = conditionMessage(e), call = conditionCall(e))
        )
      }
      return(.functionals_multicore_queue(.x, task_fun, ncores = ncores, pb_bar = pb_bar))
    } else {
      return(
        suppressWarnings(suppressMessages(
          parallel::mclapply(.x, .f, ..., mc.cores = ncores, mc.silent = TRUE)
        ))
      )
    }
  }

  # Windows or fallback with no cluster -> create PSOCK cluster
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  PAR_FUN <- if (load_balancing) parallel::parLapplyLB else parallel::parLapply
  if (pb) {
    pb_bar <- functionals_progress_bar(min = 0, max = length(.x))
    on.exit(pb_bar$kill(), add = TRUE)
    task_fun <- function(item) {
      tryCatch(
        list(ok = TRUE, value = .f(item, ...)),
        error = function(e) list(ok = FALSE, message = conditionMessage(e), call = conditionCall(e))
      )
    }
    return(.functionals_cluster_queue(cl, .x, task_fun, pb_bar = pb_bar))
  } else {
    return(PAR_FUN(cl, .x, .f, ...))
  }
}
