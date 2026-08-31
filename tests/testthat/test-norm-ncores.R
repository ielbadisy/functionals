test_that(".norm_ncores canonicalises NULL / 1 / 1L to 1L", {
  expect_identical(functionals:::.norm_ncores(NULL), 1L)
  expect_identical(functionals:::.norm_ncores(1), 1L)
  expect_identical(functionals:::.norm_ncores(1L), 1L)
  expect_identical(functionals:::.norm_ncores(4), 4L)
})

test_that(".norm_ncores warns and falls back on nonsense", {
  expect_warning(expect_identical(functionals:::.norm_ncores(0), 1L))
  expect_warning(expect_identical(functionals:::.norm_ncores(-2), 1L))
  expect_warning(expect_identical(functionals:::.norm_ncores(NA), 1L))
  expect_warning(expect_identical(functionals:::.norm_ncores(c(2, 3)), 1L))
})

test_that("every wrapper accepts ncores = NULL identically", {
  f <- function(x) x + 1
  expect_equal(fmap(1:3, f, ncores = NULL), fmap(1:3, f, ncores = 1))
  expect_equal(fmapn(list(1:3, 4:6), `+`, ncores = NULL), fmapn(list(1:3, 4:6), `+`))
})

test_that("fmapn forwards ... to .f", {
  out <- fmapn(list(x = 1:3, y = 4:6), function(x, y, k) x + y + k, k = 10)
  expect_equal(as.numeric(unlist(out)), c(15, 17, 19))
})
