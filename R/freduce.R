#' Functional Reduce
#'
#' Apply a binary function iteratively over a list or vector, reducing it to a single value or a sequence of intermediate results.
#' This is a wrapper around [Reduce()] that supports optional initial values, right-to-left evaluation,
#' accumulation of intermediate steps, and output simplification.
#'
#' @param .x A vector or list to reduce.
#' @param .f A binary function to apply. Can be given as a function or quoted (e.g., `\`+\``).
#' @param .init Optional initial value passed to [Reduce()]. If `NULL`, reduction starts from the first two elements.
#' @param .right Logical. If `TRUE`, reduction is performed from right to left.
#' @param .accumulate Logical. If `TRUE`, returns a list of intermediate results (like a scan).
#' @param .simplify Logical. If `TRUE` and all intermediate results are length 1, the output is simplified to a vector.
#'
#' @return A single value (default) or a list/vector of intermediate results if `.accumulate = TRUE`.
#'
#' @examples
#' freduce(1:5, `+`)                          # => 15
#' freduce(letters[1:4], paste0)             # => "abcd"
#' freduce(list(1, 2, 3), `*`)               # => 6
#' freduce(1:3, `+`, .init = 10)             # => 16
#' freduce(1:3, paste0, .right = TRUE)       # => "321"
#' freduce(1:4, `+`, .accumulate = TRUE)     # => c(1, 3, 6, 10)
#'
#' @export
freduce <- function(.x, .f, .init = NULL, .right = FALSE, .accumulate = FALSE, .simplify = TRUE) {
  .f <- match.fun(.f)
  if (!is.list(.x)) .x <- as.list(.x)
  if (is.null(.init)) {
    Reduce(.f, .x, right = .right, accumulate = .accumulate, simplify = .simplify)
  } else {
    Reduce(.f, .x, init = .init, right = .right, accumulate = .accumulate, simplify = .simplify)
  }
}
