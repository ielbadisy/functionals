# Single-threaded benchmark: functionals vs purrr vs base
# ------------------------------------------------------------------
# Fairness rules
#   * every engine runs sequentially: ncores = NULL, pb = FALSE,
#     options(mc.cores = 1), no future plan.
#   * identical `.f` for all engines (a plain closure, not a purrr
#     formula, so no lambda-compilation asymmetry).
#   * bench::mark(check = FALSE) because return containers differ in
#     type across engines; correctness is covered by the test suite.
#   * warm-up call before timing; fixed min_iterations.
#
# Run from the package root with:
#   Rscript inst/benchmarks/bench-purrr.R
# It writes inst/benchmarks/bench-purrr-results.rds, which the
# "functionals-programming" vignette reads (it never runs this file).

stopifnot(requireNamespace("functionals", quietly = TRUE))
suppressPackageStartupMessages({
  library(functionals)
  library(purrr)
  library(bench)
})

options(mc.cores = 1L)
set.seed(1)

# moderate-payload runs are capped at a smaller n so the script stays
# under a minute; trivial-payload runs go an order of magnitude higher.
ns_trivial  <- c(100L, 1000L, 10000L, 50000L)
ns_moderate <- c(100L, 1000L, 5000L)

# trivial: pure arithmetic. moderate: ~a few us of real work per call.
work <- function(kind) {
  if (kind == "trivial") {
    function(x) x * 2 + 1
  } else {
    function(x) {
      v <- rnorm(20) + x
      sum(sort(v)) / length(v)
    }
  }
}

bench_one <- function(n, kind) {
  x  <- seq_len(n)
  f  <- work(kind)
  x2 <- list(seq_len(n), seq_len(n))
  g2 <- function(a, b) a + b

  # warm up
  invisible(fmap(x, f)); invisible(map(x, f))

  mark(
    `functionals::fmap`  = fmap(x, f),
    `purrr::map`         = map(x, f),
    `base::lapply`       = lapply(x, f),
    `functionals::fmapn` = fmapn(x2, g2),
    `purrr::pmap`        = pmap(x2, g2),
    `base::Map`          = Map(g2, x2[[1]], x2[[2]]),
    `functionals::freduce` = freduce(x, `+`),
    `purrr::reduce`        = reduce(x, `+`),
    check         = FALSE,
    min_iterations = 10,
    filter_gc     = FALSE
  ) |>
    transform(n = n, payload = kind) |>
    subset(select = c(expression, n, payload, min, median, `itr/sec`, mem_alloc))
}

grid <- rbind(
  data.frame(n = ns_trivial,  kind = "trivial",  stringsAsFactors = FALSE),
  data.frame(n = ns_moderate, kind = "moderate", stringsAsFactors = FALSE)
)
results <- do.call(rbind, Map(bench_one, grid$n, grid$kind))
results$expression <- as.character(results$expression)
results$median_ms  <- as.numeric(results$median) * 1000
results$mem_kb     <- as.numeric(results$mem_alloc) / 1024

out <- file.path("inst", "benchmarks", "bench-purrr-results.rds")
saveRDS(
  list(
    results   = results,
    sysname   = Sys.info()[["sysname"]],
    r_version = R.version.string,
    versions  = c(
      functionals = as.character(packageVersion("functionals")),
      purrr       = as.character(packageVersion("purrr")),
      bench       = as.character(packageVersion("bench"))
    ),
    date = Sys.Date()
  ),
  out,
  version = 2
)
cat("wrote", out, "\n")
print(results[c("expression", "n", "payload", "median_ms", "mem_kb")], row.names = FALSE)
