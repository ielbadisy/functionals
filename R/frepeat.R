
#frepeat <- function(times, expr, simplify = FALSE, ncores = NULL, pb = FALSE, ...) {
  #out <- if (is.function(expr)) {
    #fapply(seq_len(times), function(i) expr(...), ncores = ncores, pb = pb)
  #} else {
    #fapply(seq_len(times), function(i) eval(expr), ncores = ncores, pb = pb)
  #}
  #if (simplify) simplify2array(out) else out
#}

frepeat <- function(.x = NULL, times, expr, simplify = FALSE, ncores = NULL, pb = FALSE, ...) {
  out <- if (is.function(expr)) {
    fapply(
      seq_len(times),
      function(i) {
        if (is.null(.x)) expr(...) else expr(.x, ...)
      },
      ncores = ncores,
      pb = pb
    )
  } else {
    fapply(seq_len(times), function(i) eval(expr), ncores = ncores, pb = pb)
  }
  if (simplify) simplify2array(out) else out
}



##The expr argument is passed as a value, so if we write expr = fcv(x, evaluate_split, ...), the function fcv(...) gets evaluated immediately, before frepeat() is even called!!!
