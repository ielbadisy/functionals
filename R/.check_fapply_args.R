.check_fapply_args <- function(X, FUN, ncores, pb) {
  # check X is atomic or list-like
  if (!is.vector(X) || is.object(X)) X <- as.list(X)

  # empty input warning
  if (length(X) == 0L) {
    warning("Input `X` is empty. Returning empty list.")
    return(list(result = list(), X = X))
  }

  # check FUN
  FUN <- match.fun(FUN)

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

  list(result = NULL, X = X, FUN = FUN, ncores = ncores, pb = pb)
}


