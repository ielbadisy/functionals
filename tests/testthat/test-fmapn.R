test_that("fmapn applies function over multiple lists", {
  x <- list(1:3, 4:6)
  out <- fmapn(x, function(a, b) a + b)

  expect_equal(out, as.list(c(5, 7, 9)))
})

test_that("fmapn handles character and numeric lists", {
  first <- letters[1:3]
  second <- 1:3
  out <- fmapn(list(first, second), function(a, b) paste0(a, b))

  expect_equal(unname(out), as.list(c("a1", "b2", "c3")))
})


test_that("fmapn supports parallel execution with progress", {
  skip_on_cran()
  a <- 1:10
  b <- 11:20

  out <- fmapn(list(a, b), function(x, y) x * y, ncores = 2, pb = TRUE)

  expect_equal(out, as.list(a * b))
})
