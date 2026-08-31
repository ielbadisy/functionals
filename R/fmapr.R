#' Apply a function row-wise on a data frame with parallelism
#'
#' Applies a function `.f` to each row of a data frame `.df`, with optional parallelism and progress bar.
#' Each row is converted to a named list before being passed to `.f`, enabling flexible access to variables by name.
#'
#' @param .df A data frame whose rows will be iterated over.
#' @param .f A function applied to each row, which receives a named list.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `NULL` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param .on_error How errors thrown by `.f` are handled: `"stop"` (default),
#'   `"pass"`, or `"fill"`. See [fapply()].
#' @param .fill Replacement for failed rows when `.on_error = "fill"`.
#' @param .seed Optional single number for reproducible per-row RNG streams.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results returned by applying `.f` to each row as a list.
#'
#' @examples
#' df <- data.frame(name = c("Mister", "Hipster"), age = c(30, 25))
#'
#' # Create personalized messages
#' fmapr(df, function(row) paste(row$name, "is", row$age, "years old"))
#'
#' # Row-wise model formulas
#' formulas <- data.frame(
#'   response = c("y1", "y2"),
#'   predictor = c("x1", "x2"),
#'   stringsAsFactors = FALSE
#' )
#'
#' fmapr(formulas, function(row) {
#'   reformulate(row$predictor, row$response)
#' })
#'
#' @export

fmapr <- function(.df, .f, ncores = NULL, pb = FALSE,
                  .on_error = c("stop", "pass", "fill"), .fill = NULL, .seed = NULL, ...) {
  .f <- match.fun(.f)
  rows <- split(.df, seq_len(nrow(.df)))
  fapply(rows, function(row) .f(as.list(row), ...), ncores = ncores, pb = pb,
         .on_error = .on_error, .fill = .fill, .seed = .seed)
}
