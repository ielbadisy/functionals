#' Apply a function over a list or vector with optional parallelism and progress
#'
#' Applies a function `.f` to each element of `.x`, using optional parallel computation
#' and progress bar tracking. Designed as a parallel-compatible replacement for `lapply()`.
#'
#' @param .x A list or atomic vector of elements to iterate over. If not a list, it will be coerced.
#' @param .f A function to apply to each element of `.x`. Can be a function or a string naming a function.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `1` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param cl Optional. A cluster object or "future" to use as backend. Default is NULL.
#' @param load_balancing Logical. Use load balancing (parLapplyLB). Default is FALSE.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results obtained by applying `.f` to each element of `.x`. The result is always a list.
#'
#' @export

fapply <- function(.x, .f, ncores = 1, pb = FALSE, cl = NULL, load_balancing = FALSE, ...) {

  ## match function
  .f <- match.fun(.f)
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)
  if (!length(.x)) return(list())

  ## sequential fallback
  if ((is.null(cl) && ncores < 2) || length(.x) < 2) {
    if (pb) {
      pb_bar <- funr_progress_bar(min = 0, max = length(.x))
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

  ## detect future backend
  use_future <- is.character(cl) && cl == "future"

  ## setup progress and chunking
  Split <- splitpb(length(.x), ncores, nout = 100)
  B <- length(Split)
  if (pb) {
    pb_bar <- funr_progress_bar(min = 0, max = B)
    on.exit(pb_bar$kill(), add = TRUE)
  }

  rval <- vector("list", B)

  ## windows special case
  if (.Platform$OS.type == "windows" && !use_future) {
    if (is.null(cl)) cl <- parallel::makeCluster(ncores)
    on.exit(if (inherits(cl, "cluster")) parallel::stopCluster(cl), add = TRUE)

    PAR_FUN <- if (load_balancing) parallel::parLapplyLB else parallel::parLapply
    for (i in seq_len(B)) {
      rval[[i]] <- PAR_FUN(cl, .x[Split[[i]]], .f, ...)
      if (pb) pb_bar$up(i)
    }

    ## use future
  } else if (use_future) {
    if (!requireNamespace("future.apply", quietly = TRUE)) stop("future.apply required for cl='future'")
    for (i in seq_len(B)) {
      rval[[i]] <- future.apply::future_lapply(.x[Split[[i]]], .f, ..., future.stdout = FALSE)
      if (pb) pb_bar$up(i)
    }

    ## Unix multicore
  } else {
    for (i in seq_len(B)) {
      rval[[i]] <- parallel::mclapply(.x[Split[[i]]], .f, ..., mc.cores = ncores)
      if (pb) pb_bar$up(i)
    }
  }

  return(do.call(c, rval))
}
