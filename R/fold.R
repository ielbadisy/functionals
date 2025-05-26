## freduce
#reduce a list/vector to a single value by applying a binary function

fold <- function(.x, .f, ...) {
  Reduce(.f, .x, ...)
}
