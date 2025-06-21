#' Compose multiple functions
#'
#' Create a new function by composing several functions, applied from right to left.
#' Equivalent to `f1(f2(f3(...)))`.
#'
#' @param ... Functions to compose. Each must take a single argument and return an object
#' compatible with the next function in the chain.
#'
#' @return A new function equivalent to nested application of the input functions.
#' @examples
#' square <- function(x) x^2
#' add1 <- function(x) x + 1
#'
#' f <- fcompose(sqrt, square, add1)  # => sqrt(square(x + 1))
#' f(4)  # => sqrt((4 + 1)^2) = sqrt(25) = 5
#'
#' # More compact
#' fcompose(log, exp)(2)  # log(exp(2)) = 2
#'
#' @export
fcompose <- function(...) {
  funcs <- list(...)
  if (length(funcs) == 0) stop("At least one function must be provided.")
  function(x) {
    for (f in rev(funcs)) {
      f <- match.fun(f)
      x <- f(x)
    }
    x
  }
}
