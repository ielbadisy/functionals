
fmap <- function(.x, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  fapply(.x, .f, ncores = ncores, pb = pb, ...)
}

