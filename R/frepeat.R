
frepeat <- function(times, expr, simplify = FALSE, ncores = NULL, pb = FALSE, ...) {
  out <- if (is.function(expr)) {
    fapply(seq_len(times), function(i) expr(...), ncores = ncores, pb = pb)
  } else {
    fapply(seq_len(times), function(i) eval(expr), ncores = ncores, pb = pb)
  }
  if (simplify) simplify2array(out) else out
}

