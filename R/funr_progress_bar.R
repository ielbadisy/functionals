
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
