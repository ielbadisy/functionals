safe <- function(.f) {
  function(...) {
    result <- tryCatch(list(result = .f(...), error = NULL),
                       error = function(e) list(result = NULL, error = e))
    return(result)
  }
}
