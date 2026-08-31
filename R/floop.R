#' Functional loop with optional parallelism and progress bar
#'
#' @description
#' **Deprecated in 0.6.0.** `floop()` never did anything that [fmap()] and
#' [fwalk()] do not: `floop(.x, .f)` is [fmap()], and
#' `floop(.x, .f, .capture = FALSE)` is [fwalk()]. Use those directly. `floop()`
#' will be removed in a future release.
#'
#' @param .x A vector or list of elements to iterate over.
#' @param .f A function to apply to each element of `.x`.
#' @param ncores Integer. Number of cores to use. Default is 1 (sequential).
#' @param pb Logical. Show a progress bar? Default is `FALSE`.
#' @param .capture Logical. If `TRUE` (default) results are returned (like
#'   [fmap()]); if `FALSE`, `.x` is returned invisibly (like [fwalk()]).
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results if `.capture = TRUE`, otherwise `.x` invisibly.
#'
#' @seealso [fmap()], [fwalk()]
#'
#' @examples
#' # Prefer fmap() / fwalk():
#' fmap(1:3, function(i) i^2)
#'
#' @export
floop <- function(.x, .f, ncores = 1, pb = FALSE, .capture = TRUE, ...) {
  .Deprecated(
    new = if (isTRUE(.capture)) "fmap" else "fwalk",
    package = "functionals",
    msg = paste0(
      "floop() is deprecated as of functionals 0.6.0.\n",
      "Use fmap() to collect results, or fwalk() for side effects."
    )
  )
  .f <- match.fun(.f)
  if (isTRUE(.capture)) {
    fmap(.x, .f, ncores = ncores, pb = pb, ...)
  } else {
    fwalk(.x, .f, ncores = ncores, pb = pb, ...)
  }
}
