#' Apply a function over a list or vector with optional parallelism and progress
#'
#' Applies a function `.f` to each element of `.x`, using optional parallel computation
#' and progress bar tracking. Uses batched `future_lapply()` for high-performance parallelism.
#'
#' @param .x A list or atomic vector of elements to iterate over. If not a list, it will be coerced.
#' @param .f A function to apply to each element of `.x`. Can be a function or a string naming a function.
#' @param ncores Integer. Number of CPU cores to use for parallel processing. Default is `1` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param cl Optional. A parallel backend to use. If `"future"`, uses `future::multisession`.
#'   If a cluster object is provided, falls back to `parallel::parLapply()` or `parLapplyLB()`.
#' @param load_balancing Logical. If using a cluster backend, whether to use `parLapplyLB`.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results obtained by applying `.f` to each element of `.x`.
#'
#' @export
fapply <- function(.x, .f, ncores = 1, pb = FALSE, cl = NULL, load_balancing = FALSE, ...) {
  .f <- match.fun(.f)
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)
  if (!length(.x)) return(list())

  use_future <- is.character(cl) && cl == "future"
  use_parallel <- isTRUE(ncores > 1)

  # sequential fallback
  if (!use_parallel) {
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

  # batched future (recommended)
  if (is.null(cl) || use_future) {
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' are required for parallel execution.")
    }

    future::plan(future::multisession, workers = ncores)
    on.exit(future::plan(future::sequential), add = TRUE)

    # batch .x into ncores chunks
    Split <- split(.x, cut(seq_along(.x), breaks = ncores, labels = FALSE))
    if (pb) {
      pb_bar <- funr_progress_bar(min = 0, max = length(Split))
      on.exit(pb_bar$kill(), add = TRUE)

      results <- future.apply::future_lapply(seq_along(Split), function(i) {
        res <- lapply(Split[[i]], .f, ...)
        pb_bar$up(i)
        res
      }, future.scheduling = 1)
    } else {
      results <- future.apply::future_lapply(Split, function(chunk) {
        lapply(chunk, .f, ...)
      }, future.scheduling = 1)
    }

    return(unlist(results, recursive = FALSE))
  }

  # legacy cluster fallback
  if (.Platform$OS.type == "windows" && is.null(cl)) {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  PAR_FUN <- if (load_balancing) parallel::parLapplyLB else parallel::parLapply

  if (pb) {
    Split <- splitpb(length(.x), ncores, nout = 100)
    B <- length(Split)
    pb_bar <- funr_progress_bar(min = 0, max = B)
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
