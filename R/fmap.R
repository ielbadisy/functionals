#' Functional mapping with optional parallelism and progress bars
#'
#' Applies a function `.f` to each element of `.x`, with optional parallel processing and progress bar support.
#'
#' @param .x A list or atomic vector of elements to iterate over.
#' @param .f A function to apply to each element of `.x`. Can be a function or a string naming a function.
#' @param ncores Integer. Number of CPU cores to use for parallel processing. Default is `NULL` (sequential).
#' @param pb Logical. Whether to show a progress bar. Default is `FALSE`.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results, one for each element of `.x`.
#'
#' @examples
#' slow_fn <- function(x) { Sys.sleep(0.01); x^2 }
#' x <- 1:100
#'
#' # Basic usage
#' fmap(x, slow_fn)
#'
#' # With progress bar
#' fmap(x, slow_fn, pb = TRUE)
#'
#' # With parallel execution (non-Windows)
#' \donttest{
#' if (.Platform$OS.type != "windows") {
#'   fmap(x, slow_fn, ncores = 2, pb = TRUE)
#' }
#' }
#'
#' @export
fmap <- function(.x, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  fapply(.x, .f, ncores = ncores %||% 1, pb = pb, ...)
}


