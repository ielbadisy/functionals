
#-------------------- arguments check
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


#-------------------- internal progression bars

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



splitpb <- function(nx, ncl, nout = 100) {
  i <- seq_len(nx)
  if (ncl == 0L) return(list())
  k <- max(1L, ceiling(ceiling(nx / ncl) / nout))
  g <- 1L + (i - 1L) %/% as.integer(ncl * k)
  structure(split(i, g), names = NULL)
}
