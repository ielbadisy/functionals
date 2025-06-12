# core functional mapping tools in funr
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



# A slow function for testing
slow_fn <- function(x) {
  Sys.sleep(0.01)
  x^2
}

# Test data
x <- 1:100

# sequential without progress
res1 <- fapply(x, slow_fn)
stopifnot(identical(res1, as.list(x^2)))

# sequential with progress
res2 <- fapply(x, slow_fn, pb = TRUE)
stopifnot(identical(res2, as.list(x^2)))

# parallel without progress
res3 <- fapply(x, slow_fn, ncores = 4)
stopifnot(identical(res3, as.list(x^2)))

# parallel with progress
res4 <- fapply(x, slow_fn, ncores = 12, pb = TRUE)
stopifnot(identical(res4, as.list(x^2)))

