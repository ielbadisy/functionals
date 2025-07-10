#' Walk over a vector or list with side effects
#'
#' Applies a function `.f` to each element of `.x`, typically for its side effects (e.g., printing, writing files).
#' This function is the side-effect-friendly equivalent of `fmap()`. Supports parallel execution and progress bar display.
#'
#' @param .x A list or atomic vector of elements to iterate over.
#' @param .f A function to apply to each element of `.x`. Should be called primarily for side effects.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `NULL` (sequential).
#' @param pb Logical. Whether to show a progress bar. Default is `FALSE`.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return Invisibly returns `.x`, like `purrr::walk()`.
#'
#' @examples
#' # Print each element
#' fwalk(1:3, print)
#'
#' # Simulate writing files in parallel
#' \donttest{
#' fwalk(1:3, function(i) {
#'   cat(paste("Processing item", i, "\n"))
#'   Sys.sleep(0.5)
#' }, ncores = 2, pb = TRUE)
#' }
#'
#' @export

fwalk <- function(.x, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  tmp <- fapply(.x, function(x) {.f(x, ...); NULL}, ncores = ncores, pb = pb)
  invisible(.x)
}

