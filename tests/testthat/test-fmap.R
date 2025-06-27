test_that("fmap works like fapply", {
  x <- 1:5
  expect_equal(unname(fmap(x, function(x) x + 1)), as.list(2:6))
})

test_that("fmap works with progress and parallel", {
  skip_on_cran()
  x <- 1:5
  res <- fmap(x, function(x) x * 10, ncores = 2, pb = TRUE)
  expect_equal(unname(res), as.list(x * 10))
})

test_that("fmap returns empty list on empty input", {
  expect_equal(fmap(list(), identity), list())
})
