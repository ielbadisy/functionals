test_that("floop returns expected results", {
  out <- floop(1:3, function(x) x + 1)
  expect_equal(unname(out), list(2, 3, 4))
})

test_that("floop respects .capture = FALSE", {
  calls <- character()
  invisible(floop(1:3, function(i) calls <<- c(calls, paste0("log", i)), .capture = FALSE))
  expect_equal(calls, c("log1", "log2", "log3"))
})

test_that("floop works with ncores > 1", {
  out <- floop(1:4, function(x) x^2, ncores = 2)
  expect_equal(unname(out), list(1, 4, 9, 16))
})
