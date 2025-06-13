#' Repeat an expression or function call multiple times with parallelism
#'
#' Repeats an expression or function evaluation `times` times. If `expr` is a function, it is invoked
#' with optional input `.x` and additional arguments. If `expr` is a quoted expression, it is evaluated
#' in the parent environment. Supports parallel processing and optional simplification of results.
#'
#' @param .x Optional input passed to `expr` if `expr` is a function. Default is `NULL`.
#' @param times Integer. Number of repetitions.
#' @param expr A function or an unevaluated expression to repeat. If a function, it will be called `times` times.
#' @param simplify Logical. If `TRUE`, attempts to simplify the result using `simplify2array()`. Default is `FALSE`.
#' @param ncores Integer. Number of cores to use for parallel execution. Default is `NULL` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param ... Additional arguments passed to the function `expr` if it is callable.
#'
#' @return A list of outputs (or a simplified array if `simplify = TRUE`) from evaluating `expr` multiple times.
#'
#' @examples
#' # Repeat a pure function call
#' frepeat(times = 3, expr = function() rnorm(1))
#'
#' # Repeat a function with input `.x`
#' frepeat(.x = 10, times = 3, expr = function(x) rnorm(1, mean = x))
#'
#' # Repeat an unevaluated expression (evaluated with `eval()`)
#' frepeat(times = 2, expr = quote(rnorm(1)))
#'
#' # Simplify the output to an array
#' frepeat(times = 3, expr = function() rnorm(1), simplify = TRUE)
#'
#' # Monte Carlo simulation: estimate coverage of a 95% CI for sample mean
#' mc_result <- frepeat(times = 1000, simplify = TRUE, pb = TRUE, ncores = 1,expr = function() {
#'   sample <- rnorm(30, mean = 0, sd = 1)
#'   ci <- t.test(sample)$conf.int
#'   mean(ci[1] <= 0 & 0 <= ci[2])  # check if true mean is inside the interval
#' })
#' mean(mc_result)  # estimated coverage
#'
#' @note If `expr` is passed as a function call (not a function or quoted expression),
#' it will be evaluated immediately, not repeated. Use `function(...) \\{ ... \\}` or `quote(...)` instead..
#'
#' @export

frepeat <- function(.x = NULL, times, expr, simplify = FALSE, ncores = NULL, pb = FALSE, ...) {
  out <- if (is.function(expr)) {
    fapply(
      seq_len(times),
      function(i) {
        if (is.null(.x)) expr(...) else expr(.x, ...)
      },
      ncores = ncores,
      pb = pb
    )
  } else {
    fapply(seq_len(times), function(i) eval(expr), ncores = ncores, pb = pb)
  }
  if (simplify) simplify2array(out) else out
}
