#accumulate results across elements using a binary function
faccumulate <- function(.x, .f, ...) {
  Reduce(.f, .x, accumulate = TRUE, ...)
}
