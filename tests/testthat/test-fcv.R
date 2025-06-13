test_that("fcv works with rsample::vfold_cv splits", {
  library(rsample)
  set.seed(123)
  cv <- vfold_cv(mtcars, v = 5)

  results <- fcv(cv$splits, function(split) {
    dat <- analysis(split)
    mean(dat$mpg)
  })

  expect_length(results, 5)
  expect_true(all(sapply(results, is.numeric)))
})

test_that("fcv supports parallel execution with progress", {
  skip_on_cran()
  library(rsample)
  cv <- vfold_cv(mtcars, v = 4)

  results <- fcv(cv$splits, function(split) {
    nrow(analysis(split))
  }, ncores = 2, pb = TRUE)

  expect_length(results, 4)
  expect_true(all(sapply(results, is.numeric)))
})
