




fmapr <- function(.df, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  rows <- split(.df, seq_len(nrow(.df)))
  fapply(rows, function(row) .f(as.list(row), ...), ncores = ncores, pb = pb)
}
