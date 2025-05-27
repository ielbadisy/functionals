
fmapn <- function(.l, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  args <- do.call(Map, c(f = list(function(...) list(...)), .l))
  fapply(args, function(x) do.call(.f, x), ncores = ncores, pb = pb, ...)
}
