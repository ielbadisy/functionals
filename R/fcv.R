
fcv <- function(.splits, .f, ncores = NULL, pb = FALSE, ...) {
  .f <- match.fun(.f)
  fapply(.splits, .f, ncores = ncores, pb = pb, ...)
}



library(rsample)
data_split <- vfold_cv(mtcars, v = 5)

fcv(data_split$splits, function(split) {
  analysis(split) |> summary()
}, ncores = 2, pb = TRUE)

