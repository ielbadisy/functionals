#' Reduce a list using a binary function
#'
#' Applies a binary function `.f` cumulatively to the elements of `.x`, reducing it to a single value.
#'
#' @param .x A list or atomic vector.
#' @param .f A binary function to reduce over elements of `.x`.
#' @param .init Optional initial value. Passed to `Reduce()` as `init`.
#' @param .right Logical. If `TRUE`, reduction proceeds right-to-left. Default is `FALSE`.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A single value obtained by cumulatively applying `.f`.
#'
#' @examples
#' freduce(1:5, `+`)           # 1 + 2 + 3 + 4 + 5 = 15
#' freduce(1:5, paste0)        # "12345"
#' freduce(list(1, 2, 3), function(x, y) x + y)  # 6
#'
#' @export

freduce <- function(.x, .f, .init, .right = FALSE, ...) {
  .f <- match.fun(.f)
  if (missing(.init)) {
    Reduce(.f, .x, right = .right, ...)
  } else {
    Reduce(.f, .x, init = .init, right = .right, ...)
  }
}
