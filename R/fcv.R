#' Functional Cross-Validation mapping
#'
#' Applies a user-defined function `.f` to each element of `.splits`, typically from
#' cross-validation objects such as `rsample::vfold_cv()`.
#'
#' `fcv()` is a thin, intention-revealing alias for [fapply()] specialised to a
#' list of resample splits: `fcv(splits, f)` is exactly
#' `fapply(splits, f)`. Use it when a call site reads better as
#' "cross-validate" than as "apply".
#'
#' @param .splits A list of resample splits (e.g., from `rsample::vfold_cv()`).
#' @param .f A function to apply to each split. Typically expects a single `split` object.
#' @param ncores Integer. Number of cores to use for parallel processing. Default is `NULL` (sequential).
#' @param pb Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param .on_error How errors thrown by `.f` are handled: `"stop"` (default),
#'   `"pass"`, or `"fill"`. See [fapply()].
#' @param .fill Replacement for failed folds when `.on_error = "fill"`.
#' @param .seed Optional single number for reproducible per-fold RNG streams.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A list of results returned by applying `.f` to each element of `.splits`.
#'
#' @examples
#' if (requireNamespace("rsample", quietly = TRUE)) {
#'   set.seed(123)
#'   cv_splits <- rsample::vfold_cv(mtcars, v = 5)
#'
#'   # Apply summary over training sets
#'   fcv(cv_splits$splits, function(split) {
#'     summary(rsample::analysis(split))
#'   })
#'
#'   # With progress and parallel execution
#'   \donttest{
#'     fcv(cv_splits$splits, function(split) {
#'       summary(rsample::analysis(split))
#'     }, ncores = 2, pb = TRUE)
#'   }
#' }
#'
#' @export

fcv <- function(.splits, .f, ncores = NULL, pb = FALSE,
                .on_error = c("stop", "pass", "fill"), .fill = NULL, .seed = NULL, ...) {
  .f <- match.fun(.f)
  fapply(.splits, .f, ncores = ncores, pb = pb,
         .on_error = .on_error, .fill = .fill, .seed = .seed, ...)
}


