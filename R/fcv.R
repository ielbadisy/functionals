#' Functional Cross-Validation mapping with parallelism support
#'
#' Applies a user-defined function `.f` to each element of `.splits`, typically from
#' cross-validation objects such as `rsample::vfold_cv()`.
#'
#' @param .splits A list of resample splits (e.g., from `rsample::vfold_cv()`).
#' @param .f A function to apply to each split. Typically expects a single `split` object.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `NULL` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results returned by applying `.f` to each element of `.splits`.
#'
#' @examples
#' library(rsample)
#' set.seed(123)
#' cv_splits <- vfold_cv(mtcars, v = 5)
#'
#' # Apply summary over training sets
#' fcv(cv_splits$splits, function(split) {
#'   summary(analysis(split))
#' })
#'
#' # With progress and parallel execution
#' \dontrun{
#' fcv(cv_splits$splits, function(split) {
#'   summary(analysis(split))
#' }, ncores = 2, pb = TRUE)
#' }
#'
#' @export

fcv <- function(.splits, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  fapply(.splits, .f, ncores = ncores, pb = pb, ...)
}


