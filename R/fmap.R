
fmap <- function(.x, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  fapply(.x, .f, ncores = ncores, pb = pb, ...)
}



library(furrr)
library(bench)
library(future)

slow_fn <- function(x) {
  Sys.sleep(0.01)
  x^2
}

x <- 1:100

ncores <- 12

plan(multisession, workers = ncores)

results <- bench::mark(
  funr_fmap      = fmap(x, slow_fn, ncores = ncores, pb = TRUE),
  furrr_future   = furrr::future_map(x, slow_fn, .options = furrr::furrr_options(seed = NULL)),
  iterations = 10,
  check = TRUE,
  memory = FALSE
)

print(results)
