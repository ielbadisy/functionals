#' Functional loop with optional parallelism and progress bar
#'
#' `floop()` applies a function `.f` to each element of `.x`, optionally in parallel, and with an optional progress bar.
#' Unlike `fwalk()`, it can return results or be used purely for side effects (like a for-loop).
#'
#' @param .x A vector or list of elements to iterate over.
#' @param .f A function to apply to each element of `.x`.
#' @param ncores Integer. Number of cores to use. Default is 1 (sequential).
#' @param pb Logical. Show a progress bar? Default is `FALSE`.
#' @param .capture Logical. Should results of `.f` be captured and returned? If `FALSE`, acts like a side-effect loop.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results if `.capture = TRUE`, otherwise returns `.x` invisibly.
#'
#' @examples
#' # Functional loop that collects output
#' floop(1:3, function(i) i^2)
#'
#' # Side-effect only loop (like for-loop with cat)
#' \dontrun{
#' floop(1:5, function(i) cat(" Processing", i, "\n"), pb = TRUE, .capture = FALSE)
#' }
#'
#' @export

floop <- function(.x, .f, ncores = 1, pb = FALSE, .capture = TRUE, ...) {
  .f <- match.fun(.f)
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)
  fwrap <- function(x) {.f(x, ...)}
  result <- fapply(.x, fwrap, ncores = ncores, pb = pb)
  if (.capture) result else invisible(.x)
}
