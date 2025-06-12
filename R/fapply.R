# core functional mapping tools in funr
fapply <- function(.x, .f, ncores = 1, pb = FALSE, ...) {

  ## check and normalize arguments
  args <- .check_fapply_args(.x, .f, ncores, pb)
  if (!is.null(args$result)) return(args$result)
  .x <- args$.x; .f <- args$.f; ncores <- args$ncores; pb <- args$pb


  .f <- match.fun(.f)
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)
  if (!length(.x)) return(list())

  # custom progress bar function
  funr_progress_bar <- function(min = 0, max = 1, style = 1, width = NA, char = "=") {
    start_time <- proc.time()[["elapsed"]]
    i <- min
    if (is.na(width)) width <- getOption("width")
    get_time_str <- function(seconds) {
      sec <- round(seconds %% 60)
      minutes <- floor(seconds / 60) %% 60
      hours <- floor(seconds / 3600)
      sprintf("%02ih %02im %02is", hours, minutes, sec)
    }
    update <- function(value) {
      if (!is.finite(value) || value < min || value > max) return()
      i <<- value
      elapsed <- proc.time()[["elapsed"]] - start_time
      total <- max - min
      percent <- (i - min) / total
      remaining <- if (percent > 0) elapsed * (1 - percent) / percent else NA
      bar_width <- width - 30
      done <- floor(bar_width * percent)
      left <- bar_width - done
      cat(sprintf("\r |%s%s| %3d%% elapsed=%s, remaining~%s",
                  strrep(char, done), strrep(" ", left),
                  round(percent * 100),
                  get_time_str(elapsed),
                  if (!is.na(remaining)) get_time_str(remaining) else "..."))
      flush.console()
    }
    kill <- function() cat("\n")
    update(i)
    list(up = update, kill = kill)
  }

  if (pb) {
    splitpb <- function(nx, ncl, nout = 100) {
      i <- seq_len(nx)
      if (ncl == 0L) return(list())
      k <- max(1L, ceiling(ceiling(nx / ncl) / nout))
      g <- 1L + (i - 1L) %/% as.integer(ncl * k)
      structure(split(i, g), names = NULL)
    }
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

