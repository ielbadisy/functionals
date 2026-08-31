#' Apply a function column-wise with name access and parallelism
#'
#' Applies a function `.f` to each column of a data frame `.df`. Each call receives both
#' the column vector and its name, enabling name-aware column processing. Supports parallel execution and progress display.
#'
#' @param .df A data frame whose columns will be iterated over.
#' @param .f A function that takes two arguments: the column vector and its name.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `NULL` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param .on_error How errors thrown by `.f` are handled: `"stop"` (default),
#'   `"pass"`, or `"fill"`. See [fapply()].
#' @param .fill Replacement for failed columns when `.on_error = "fill"`.
#' @param .seed Optional single number for reproducible per-column RNG streams.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results obtained by applying `.f` to each column of `.df`.
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6)
#'
#' # Apply a function that returns column mean and name
#' fmapc(df, function(x, name) {
#'   list(mean = mean(x), var = var(x), name = name)
#' })
#'
#' # With progress and parallel execution
#' \donttest{
#' fmapc(df, function(x, name) mean(x), ncores = 2, pb = TRUE)
#' }
#'
#' @export

fmapc <- function(.df, .f, ncores = NULL, pb = FALSE,
                  .on_error = c("stop", "pass", "fill"), .fill = NULL, .seed = NULL, ...) {
  .f <- match.fun(.f)
  cols <- as.list(.df)
  names(cols) <- names(.df)
  col_pairs <- mapply(function(x, n) list(x, n), cols, names(cols), SIMPLIFY = FALSE)
  fapply(col_pairs, function(pair) .f(pair[[1]], pair[[2]], ...), ncores = ncores, pb = pb,
         .on_error = .on_error, .fill = .fill, .seed = .seed)
}
