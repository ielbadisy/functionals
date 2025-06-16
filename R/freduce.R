#' Functional reduce
#'
#' Apply a binary function iteratively over a list or vector, reducing it to a single value.
#' This is a functional programming wrapper around [Reduce()] with support for left-to-right
#' and right-to-left reduction and an optional initial value.
#'
#' @param .x A vector or list of elements to reduce.
#' @param .f A binary function to apply. Can be specified as a function or a function name (e.g., `\`+\``).
#' @param .init An optional initial value to start the reduction. If `NULL`, reduction begins with the first two elements of `.x`.
#' @param .right Logical. If `FALSE` (default), reduction proceeds left-to-right. If `TRUE`, right-to-left reduction is used.
#'
#' @return The result of reducing `.x` using `.f`, optionally starting from `.init`.
#'
#' @examples
#' freduce(1:5, `+`)                          # Sum: 1 + 2 + 3 + 4 + 5 = 15
#' freduce(letters[1:4], paste0)             # Concatenate: "abcd"
#' freduce(list(1, 2, 3), `*`)               # Product: 1 * 2 * 3 = 6
#' freduce(1:3, `+`, .init = 10)             # Sum with initial value: 10 + 1 + 2 + 3 = 16
#' freduce(1:3, paste0, .right = TRUE)       # Right-to-left: paste0(1, paste0(2, 3)) = "123"
#'
#' @export
freduce <- function(.x, .f, .init = NULL, .right = FALSE) {
  .f <- match.fun(.f)
  if (!is.list(.x)) .x <- as.list(.x)
  if (is.null(.init)) {
    Reduce(.f, .x, right = .right)
  } else {
    Reduce(.f, .x, init = .init, right = .right)
  }
}
