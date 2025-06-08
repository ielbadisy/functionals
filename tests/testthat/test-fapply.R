test_that("fapply works sequentially without progress bar", {
  x <- 1:10
  expect_equal(fapply(x, function(i) i^2), as.list(x^2))
})

test_that("fapply works sequentially with progress bar", {
  x <- 1:10
  expect_equal(fapply(x, function(i) i^2, pb = TRUE), as.list(x^2))
})

test_that("fapply works in parallel without progress bar", {
  skip_if(.Platform$OS.type == "windows", "mclapply not supported on Windows")
  x <- 1:10
  expect_equal(fapply(x, function(i) i^2, ncores = 2), as.list(x^2))
})

test_that("fapply works in parallel with progress bar", {
  skip_if(.Platform$OS.type == "windows", "mclapply not supported on Windows")
  x <- 1:10
  expect_equal(fapply(x, function(i) i^2, ncores = 2, pb = TRUE), as.list(x^2))
})

test_that("fapply returns empty list when input is empty", {
  expect_equal(fapply(numeric(0), identity), list())
})

test_that("fapply warns and defaults on invalid ncores", {
  expect_warning(
    res <- fapply(1:5, identity, ncores = "bad"),
    regexp = "ncores.*positive integer"
  )
  expect_equal(res, as.list(1:5))
})

test_that("fapply warns and disables progress on invalid pb", {
  expect_warning(
    res <- fapply(1:5, identity, pb = "yes"),
    regexp = "pb.*TRUE or FALSE"
  )
  expect_equal(res, as.list(1:5))
})

test_that("fapply handles character input and side effects", {
  expect_equal(
    fapply(letters[1:3], toupper),
    as.list(c("A", "B", "C"))
  )
})

test_that("fapply works with additional arguments", {
  x <- 1:3
  square_add <- function(i, add) (i^2) + add
  expect_equal(fapply(x, square_add, add = 1), as.list(x^2 + 1))
})
