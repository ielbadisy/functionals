#' Multicore row-binding of a list of data.frames using funr
#'
#' @param x A list of data.frames.
#' @param ncores Number of cores to use. Default is 1 (sequential).
#' @param chunk_size Optional chunk size.
#' @param pb Show progress bar? Default FALSE.
#'
#' @return A data.frame resulting from row-binding all elements in `x`.
#' @export
frbindlist <- function(x, ncores = 1, chunk_size = NULL, pb = FALSE) {
  if (!is.list(x)) stop("x must be a list")
  x <- Filter(Negate(is.null), x)
  if (length(x) == 0) return(data.frame())

  n <- length(x)
  k <- if (!is.null(chunk_size)) ceiling(n / chunk_size) else min(n, ncores)
  if (k <= 1) return(do.call(rbind, x))

  idx <- split(seq_along(x), cut(seq_along(x), breaks = k, labels = FALSE))
  chunks <- lapply(idx, function(i) x[i])

  part_results <- floop(
    .x = chunks,
    .f = function(chunk) do.call(rbind, chunk),
    ncores = ncores,
    pb = pb
  )

  do.call(rbind, part_results)
}
