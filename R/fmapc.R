
fmapc <- function(.df, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  cols <- as.list(.df)
  names(cols) <- names(.df)
  col_pairs <- mapply(function(x, n) list(x, n), cols, names(cols), SIMPLIFY = FALSE)
  fapply(col_pairs, function(pair) .f(pair[[1]], pair[[2]], ...), ncores = ncores, pb = pb)
}
