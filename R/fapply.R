# core functional mapping tools in funr

fapply <- function(X, FUN, ncores = 1, pb = FALSE, ...) {
  ## function check logic
  args <- .check_fapply_args(X, FUN, ncores, pb)
  if (!is.null(args$result)) return(args$result)
  X <- args$X; FUN <- args$FUN; ncores <- args$ncores; pb <- args$pb

  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X)) X <- as.list(X)
  if (!length(X)) return(list())

  if (pb) {
    # internal PB setup -> remove to helpers ?
    pboptions <- function(...) {
      opar <- getOption("pboptions")
      args <- list(...)
      if (length(args)) {
        if (length(args) == 1 && is.list(args[[1]])) {
          npar <- args[[1]]
        } else {
          npar <- opar
          npar[match(names(args), names(npar))] <- args
        }
        options("pboptions" = npar)
      }
      invisible(opar)
    }

    pbtypes <- function() {
      TYPES <- c("timer", "txt", "tk", "none", "shiny")
      if (.Platform$OS.type == "windows")
        c(TYPES, "win") else TYPES
    }

    dopb <- function() {
      progress.bar <- getOption("pboptions")$type
      if (!is.null(progress.bar)) {
        progress.bar <- match.arg(progress.bar, pbtypes())
        if (progress.bar == "none")
          progress.bar <- NULL
        if (!is.null(getOption("knitr.in.progress")))
          progress.bar <- NULL
      }
      !is.null(progress.bar)
    }

    splitpb <- function(nx, ncl, nout = NULL) {
      i <- seq_len(nx)
      if (ncl == 0L) return(list())
      if (is.null(nout)) {
        k <- 1L
      } else {
        if (nout < 1L) stop("nout must be > 0")
        k <- max(1L, ceiling(ceiling(nx / ncl) / nout))
      }
      g <- 1L + (i - 1L) %/% as.integer(ncl * k)
      structure(split(i, g), names = NULL)
    }

    getTimeAsString <- function(time) {
      if (length(time) > 1L) stop("length of input must be 1")
      if (is.null(time)) return("calculating")
      if (is.infinite(time)) return("Inf")
      sec <- round(time %% 60)
      time <- floor(time / 60)
      minutes <- floor(time %% 60)
      time <- floor(time / 60)
      days <- floor(time / 24)
      time <- floor(time %% 24)
      hours <- floor(time %% 60)
      resTime <- ""
      if (days > 0) resTime <- sprintf("%02id ", days)
      if (hours > 0 || days > 0) resTime <- paste(resTime, sprintf("%02ih ", hours), sep = "")
      if (minutes > 0 || hours > 0 || days > 0) resTime <- paste(resTime, sprintf("%02im ", minutes), sep = "")
      paste0(resTime, sprintf("%02is", sec))
    }

    # remove to helpers ?
    timerProgressBar <- function(min = 0, max = 1, initial = 0, char = "=", width = NA, style = 1, file = "", min_time = 0) {
      .start <- proc.time()[["elapsed"]]
      .min <- min
      .max <- max
      .i <- initial
      .killed <- FALSE
      .showpb <- if (min_time > 0) FALSE else TRUE
      getVal <- function() .i
      if (is.na(width)) width <- options("width")[[1]]
      up <- function(value) {
        if (!is.finite(value) || value < min || value > max) return()
        time0 <- proc.time()[["elapsed"]] - .start
        .i <<- value
        i <- .i - .min
        n <- .max - .min
        time <- time0 / (i / n) - time0
        if (.i > .min && sum(time0, time, na.rm = TRUE) > min_time) .showpb <<- TRUE
        if (.showpb) {
          spentTime <- paste0(" elapsed=", getTimeAsString(time0))
          leftTime <- if (i == 0) "" else paste0(", remaining~", getTimeAsString(time))
          minLetters <- nchar("%%%% ~00h 00m 00s", "w")
          txtWidth <- max(width, width - minLetters - 4)
          text <- paste0(sprintf("%-2.0f%%", 100 * i / n), spentTime, leftTime)
          if (nchar(text, "w") < minLetters)
            text <- paste(text, paste(rep(" ", minLetters - nchar(text, "w")), collapse = ""))
          if (txtWidth <= 0) {
            cat("\r ", text, file = file)
          } else {
            done <- ceiling(txtWidth * i / n)
            bb <- strrep(char, done)
            empty <- strrep(" ", txtWidth - done)
            bar <- paste(" |", bb, empty, "|", sep = "")
            cat("\r", bar, text, file = file)
          }
          flush.console()
        }
      }
      ## remove to helpers
      kill <- function() if (!.killed) {
        if (.showpb) cat("\n", file = file)
        .killed <<- TRUE
      }
      up(initial)
      structure(list(getVal = getVal, up = up, kill = kill), class = c("timerProgressBar", "txtProgressBar"))
    }

    ## remove to helpers
    setpb <- function(pb, value) {
      if (inherits(pb, "timerProgressBar")) {
        pb$up(value)
      } else if (inherits(pb, "txtProgressBar")) {
        utils::setTxtProgressBar(pb, value)
      }
    }
    ## remove to helpers
    closepb <- function(pb) {
      if (inherits(pb, "timerProgressBar")) {
        pb$kill()
      } else if (inherits(pb, "txtProgressBar")) {
        close(pb)
      }
    }
    ## remove helpers
    startpb <- function(min = 0, max = 1) {
      if (dopb()) {
        control <- getOption("pboptions")
        pb <- switch(control$type,
                     timer = timerProgressBar(min = min, max = max, initial = 0,
                                              style = control$style, width = control$txt.width,
                                              char = control$char, min_time = control$min_time),
                     txt = txtProgressBar(min = min, max = max, initial = 0,
                                          style = control$style, width = control$txt.width,
                                          char = control$char))
      } else {
        pb <- NULL
      }
      invisible(pb)
    }

    pboptions(type = "timer", style = 1, txt.width = 60, char = "=", min_time = 0.1, nout = 100)
    Split <- splitpb(length(X), ncores, nout = 100)
    B <- length(Split)
    pb_bar <- startpb(0, B)
    on.exit(closepb(pb_bar), add = TRUE)
    rval <- vector("list", B)
    for (i in seq_len(B)) {
      if (.Platform$OS.type == "windows") {
        cl <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)
        rval[[i]] <- parallel::parLapply(cl, X[Split[[i]]], FUN, ...)
        parallel::stopCluster(cl)
      } else {
        rval[[i]] <- parallel::mclapply(X[Split[[i]]], FUN, ..., mc.cores = ncores)
      }
      setpb(pb_bar, i)
    }
    return(unlist(rval, recursive = FALSE))
  } else {
    if (!is.null(ncores) && ncores > 1) {
      if (.Platform$OS.type == "windows") {
        cl <- parallel::makeCluster(ncores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::parLapply(cl, X, FUN, ...)
      } else {
        parallel::mclapply(X, FUN, ..., mc.cores = ncores)
      }
    } else {
      lapply(X, FUN, ...)
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
res4 <- fapply(x, slow_fn, ncores = 4, pb = TRUE)
stopifnot(identical(res4, as.list(x^2)))

