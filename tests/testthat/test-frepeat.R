test_that("frepeat evaluates an expression repeatedly", {
  out <- frepeat(times = 3, expr = quote(rnorm(1)))
  expect_length(out, 3)
  expect_true(all(sapply(out, is.numeric)))
})

test_that("frepeat works with a function and optional .x", {
  out1 <- frepeat(times = 3, expr = function() 1)
  expect_equal(out1, list(1, 1, 1))

  out2 <- frepeat(.x = 10, times = 3, expr = function(x) x + 1)
  expect_equal(out2, list(11, 11, 11))
})

test_that("frepeat can simplify the result", {
  out <- frepeat(times = 3, expr = function() 1, simplify = TRUE)
  expect_equal(out, rep(1, 3))
})

test_that("frepeat works in parallel with progress bar", {
  skip_on_cran()
  out <- frepeat(times = 5, expr = function() 1, ncores = 2, pb = TRUE)
  expect_equal(out, rep(list(1), 5))
})
