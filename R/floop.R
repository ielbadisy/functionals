
floop <- function(.x, .f, ncores = 1, pb = FALSE, .capture = TRUE, ...) {
  .f <- match.fun(.f)
  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)
  fwrap <- function(x) {.f(x, ...)}
  result <- fapply(.x, fwrap, ncores = ncores, pb = pb)
  if (.capture) result else invisible(.x)
}
