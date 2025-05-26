
fdrop <- function(.x, .p, ...) {
  Filter(Negate(.p), .x)
}
