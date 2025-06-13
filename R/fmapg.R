#' Apply a function to groups of a data frame in parallel
#'
#' Applies a function `.f` to each group of rows in a data frame `.df`, where grouping is defined by one or more variables in `by`.
#' Each group is passed as a data frame to `.f`. Supports parallelism and optional progress display.
#'
#' @param .df A data frame to group and apply the function over.
#' @param .f A function to apply to each group. The function should accept a data frame (a group).
#' @param by A character vector of column names in `.df` used for grouping.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `NULL` (sequential).
#' @param pb Logical. Whether to show a progress bar. Default is `FALSE`.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results, one for each group defined by `by`.
#'
#' @examples
#' # Group-wise mean of Sepal.Length in iris dataset
#' fmapg(iris, function(df) mean(df$Sepal.Length), by = "Species")
#'
#' # Group-wise model fitting with progress and parallelism
#' \dontrun{
#' fmapg(mtcars, function(df) lm(mpg ~ wt, data = df), by = "cyl", ncores = 2, pb = TRUE)
#' }
#'
#' @export


fmapg <- function(.df, .f, by, ncores = NULL, pb = FALSE, ...) {
  stopifnot(is.data.frame(.df), all(by %in% names(.df)))
  groups <- split(.df, .df[by], drop = TRUE)
  .f <- match.fun(.f)
  fapply(groups, .f, ncores = ncores, pb = pb, ...)
}
