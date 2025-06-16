test_that("fcompose works with numeric functions", {
  f <- fcompose(sqrt, function(x) x^2, function(x) x + 1)
  expect_equal(f(4), 5)
})

test_that("fcompose fails with no input functions", {
  expect_error(fcompose(), "At least one function must be provided.")
})


test_that("fcompose returns original function if one is passed", {
  f <- fcompose(function(x) x + 1)
  expect_equal(f(3), 4)
})
