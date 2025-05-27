fwalk <- function(.x, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  fapply(.x, function(x) {.f(x, ...); NULL}, ncores = ncores, pb = pb)
  invisible(.x)
}
