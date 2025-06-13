#' Apply a function over a list or vector with optional parallelism and progress
#'
#' Applies a function `.f` to each element of `.x`, using optional parallel computation
#' and progress bar tracking. Designed as a parallel-compatible replacement for `lapply()`.
#'
#' @param .x A list or atomic vector of elements to iterate over. If not a list, it will be coerced.
#' @param .f A function to apply to each element of `.x`. Can be a function or a string naming a function.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `1` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results obtained by applying `.f` to each element of `.x`. The result is always a list.
#'
#' @examples
#' slow_fn <- function(x) { Sys.sleep(0.01); x^2 }
#' x <- 1:100
#'
#' # Sequential without progress
#' fapply(x, slow_fn)
#'
#' # Sequential with progress
#' fapply(x, slow_fn, pb = TRUE)
#'
#' # Parallel execution (Linux/macOS)
#' \dontrun{
#' fapply(x, slow_fn, ncores = 4)
#' fapply(x, slow_fn, ncores = 4, pb = TRUE)
#' }
#'
#' @export

fapply <- function(.x, .f, ncores = 1, pb = FALSE, ...) {

  ## check and normalize arguments
  args <- .check_fapply_args(.x, .f, ncores, pb)
  if (!is.null(args$result)) return(args$result)
  .x <- args$.x; .f <- args$.f; ncores <- args$ncores; pb <- args$pb

  .f <- match.fun(.f)
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)
  if (!length(.x)) return(list())


  if (pb) {
    Split <- splitpb(length(.x), ncores, nout = 100)
    B <- length(Split)
    pb_bar <- funr_progress_bar(min = 0, max = B)
    on.exit(pb_bar$kill(), add = TRUE)
    rval <- vector("list", B)
    for (i in seq_len(B)) {
      if (.Platform$OS.type == "windows") {
        cl <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)
        rval[[i]] <- parallel::parLapply(cl, .x[Split[[i]]], .f, ...)
        parallel::stopCluster(cl)
      } else {
        rval[[i]] <- parallel::mclapply(.x[Split[[i]]], .f, ..., mc.cores = ncores)
      }
      pb_bar$up(i)
    }
    return(unlist(rval, recursive = FALSE))
  } else {
    if (!is.null(ncores) && ncores > 1) {
      if (.Platform$OS.type == "windows") {
        cl <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::parLapply(cl, .x, .f, ...)
      } else {
        parallel::mclapply(.x, .f, ..., mc.cores = ncores)
      }
    } else {
      lapply(.x, .f, ...)
    }
  }
}
