# Test script to validate funr_progress_bar with fapply

# Dummy slow function
slow_fn <- function(x) {
  Sys.sleep(0.2)
  x^2
}

# Generate test input
x <- as.list(1:100)

# Define fapply using funr_progress_bar
fapply <- function(X, FUN, ncores = NULL, pb = FALSE, ...) {
  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X)) X <- as.list(X)
  if (!length(X)) return(list())

  show_pb <- isTRUE(pb)
  total <- length(X)
  pb_bar <- if (show_pb) funr_progress_bar(min = 0, max = total) else NULL

  wrapper <- function(i) {
    res <- FUN(X[[i]], ...)
    if (!is.null(pb_bar)) pb_bar$up(i)
    res
  }

  result <- if (is.null(ncores)) {
    lapply(seq_along(X), wrapper)
  } else if (.Platform$OS.type == "windows") {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::parLapply(cl, seq_along(X), wrapper)
  } else {
    parallel::mclapply(seq_along(X), wrapper, mc.cores = ncores)
  }

  if (!is.null(pb_bar)) pb_bar$kill()
  result
}

# Run test
res <- fapply(x, slow_fn, ncores = NULL, pb = TRUE)
stopifnot(length(res) == length(x))
