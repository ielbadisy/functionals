test_that("fapply works sequentially", {
  x <- 1:5
  expect_equal(unname(fapply(x, function(x) x^2)), as.list(x^2))
})

test_that("fapply works with progress bar and parallel", {
  skip_on_cran()
  x <- 1:10
  res <- fapply(x, function(x) x + 1, ncores = 2, pb = TRUE)
  expect_equal(unname(res), as.list(x + 1))
})

test_that("fapply handles empty input", {
  expect_equal(fapply(list(), identity), list())
})
