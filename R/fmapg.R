

fmapg <- function(.df, .f, by, ncores = NULL, pb = FALSE, ...) {
  stopifnot(is.data.frame(.df), all(by %in% names(.df)))
  groups <- split(.df, .df[by], drop = TRUE)
  .f <- match.fun(.f)
  fapply(groups, .f, ncores = ncores, pb = pb, ...)
}

#fmapg(iris, function(df) mean(df$Sepal.Length), by = "Species")
