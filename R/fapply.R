#' Apply a function over a list or vector with optional parallelism and progress
#'
#' A lightweight and fast version of `lapply()` with support for multicore (Unix) and snow-style
#' clusters via `parallel`, with internal progress bar tracking and message suppression.
#'
#' @param .x A list or atomic vector.
#' @param .f Function to apply.
#' @param ncores Number of cores to use (default: 1 = sequential).
#' @param pb Show progress bar? (default: FALSE).
#' @param cl A cluster object (from parallel::makeCluster), or integer for core count.
#' @param load_balancing Logical. Use `parLapplyLB` if `TRUE` (default: `FALSE`).
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
#' # Heavy computation example with chunked parallelism
#' \donttest{
#' heavy_fn <- function(x) { Sys.sleep(0.05); x^2 }
#' fapply(1:20, heavy_fn, ncores = 2, pb = TRUE)
#' }
#'
#' @export

fapply <- function(.x, .f, ncores = 1, pb = FALSE, cl = NULL, load_balancing = TRUE, ...) {
  .f <- match.fun(.f)
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)
  if (!length(.x)) return(list())

  is_windows <- .Platform$OS.type == "windows"
  use_parallel <- isTRUE(ncores > 1L)

  # disable crashpad messages
  Sys.setenv(CHROME_CRASHPAD_PIPE_NAME = "disable")

  # sequential fallback
  if (!use_parallel && is.null(cl)) {
    if (pb) {
      pb_bar <- functionals_progress_bar(min = 0, max = length(.x))
      on.exit(pb_bar$kill(), add = TRUE)
      out <- vector("list", length(.x))
      for (i in seq_along(.x)) {
        out[[i]] <- .f(.x[[i]], ...)
        pb_bar$up(i)
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
      Split <- splitpb(length(.x), length(cl), nout = 100)
      B <- length(Split)
      pb_bar <- functionals_progress_bar(min = 0, max = B)
      on.exit(pb_bar$kill(), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[[i]] <- PAR_FUN(cl, .x[Split[[i]]], .f, ...)
        pb_bar$up(i)
      }
      return(do.call(c, rval))
    } else {
      return(PAR_FUN(cl, .x, .f, ...))
    }
  }

  # if user requested multicore (Unix only)
  if (!is_windows && is.null(cl)) {
    if (pb) {
      Split <- splitpb(length(.x), ncores, nout = 100)
      B <- length(Split)
      pb_bar <- functionals_progress_bar(min = 0, max = B)
      on.exit(pb_bar$kill(), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[[i]] <- suppressWarnings(suppressMessages(
          parallel::mclapply(.x[Split[[i]]], .f, ..., mc.cores = ncores, mc.silent = TRUE)
        ))
        pb_bar$up(i)
      }
      return(do.call(c, rval))
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
    Split <- splitpb(length(.x), ncores, nout = 100)
    B <- length(Split)
    pb_bar <- functionals_progress_bar(min = 0, max = B)
    on.exit(pb_bar$kill(), add = TRUE)
    rval <- vector("list", B)
    for (i in seq_len(B)) {
      rval[[i]] <- PAR_FUN(cl, .x[Split[[i]]], .f, ...)
      pb_bar$up(i)
    }
    return(do.call(c, rval))
  } else {
    return(PAR_FUN(cl, .x, .f, ...))
  }
}
