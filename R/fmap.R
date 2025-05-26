## fmap ---------------
fmap <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}
