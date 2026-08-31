#' Apply a function over multiple argument lists in parallel
#'
#' Applies a function `.f` over multiple aligned lists in `.l`.
#' Each element of `.l` should be a list or vector of the same length. Each call to `.f` receives one element from each list.
#' Supports parallel execution and progress display.
#'
#' @param .l A list of vectors or lists. All elements must be of equal length.
#' @param .f A function to apply. It must accept as many arguments as there are elements in `.l`.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `NULL` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param .on_error How errors thrown by `.f` are handled: `"stop"` (default),
#'   `"pass"`, or `"fill"`. See [fapply()].
#' @param .fill Replacement for failed tuples when `.on_error = "fill"`.
#' @param .seed Optional single number for reproducible per-tuple RNG streams.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results obtained by applying `.f` to each tuple from `.l`.
#'
#' @examples
#' # Fit a linear model for each response variable using the same predictor
#' df <- data.frame(
#'   y1 = rnorm(100),
#'   y2 = rnorm(100),
#'   x = rnorm(100)
#' )
#'
#' # List of formulas and data
#' formulas <- list(y1 ~ x, y2 ~ x)
#' data_list <- list(df, df)
#'
#' fmapn(list(formula = formulas, data = data_list),
#'       function(formula, data) lm(formula, data = data))
#'
#' # Extract model summaries in parallel
#' models <- fmapn(
#'   list(formula = formulas, data = data_list),
#'   function(formula, data) summary(lm(formula, data = data))$r.squared
#' )
#'
#' @export

fmapn <- function(.l, .f, ncores = NULL, pb = FALSE,
                  .on_error = c("stop", "pass", "fill"), .fill = NULL, .seed = NULL, ...) {
  .f <- match.fun(.f)
  dots <- list(...)
  args <- do.call(Map, c(f = list(function(...) list(...)), .l))
  fapply(args, function(x) do.call(.f, c(x, dots)), ncores = ncores, pb = pb,
         .on_error = .on_error, .fill = .fill, .seed = .seed)
}
