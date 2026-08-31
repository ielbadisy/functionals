test_that(".seed makes a sequential run reproducible", {
  a <- fmap(1:6, function(i) rnorm(1), .seed = 123)
  b <- fmap(1:6, function(i) rnorm(1), .seed = 123)
  expect_identical(a, b)
})

test_that(".seed is invariant to ncores", {
  skip_on_cran()
  if (.Platform$OS.type == "windows") skip("fork path")
  seq_run <- fmap(1:10, function(i) rnorm(1), .seed = 7)
  par_run <- fmap(1:10, function(i) rnorm(1), .seed = 7, ncores = 2)
  expect_equal(seq_run, par_run)
})

test_that(".seed restores the caller's global RNG state", {
  set.seed(42)
  target <- runif(3)
  set.seed(42)
  invisible(frepeat(times = 5, expr = function() rnorm(1), .seed = 99))
  expect_identical(runif(3), target)
  expect_identical(RNGkind()[1], "Mersenne-Twister")
})

test_that("frepeat + .seed reproduces a Monte Carlo estimate", {
  est <- function() {
    x <- rnorm(20)
    mean(x)
  }
  r1 <- frepeat(times = 50, expr = est, .seed = 2024, simplify = TRUE)
  r2 <- frepeat(times = 50, expr = est, .seed = 2024, simplify = TRUE)
  expect_identical(r1, r2)
})

test_that(".seed keeps names", {
  out <- fmap(c(a = 1, b = 2, c = 3), function(x) rnorm(1), .seed = 1)
  expect_identical(names(out), c("a", "b", "c"))
})
