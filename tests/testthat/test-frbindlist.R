test_that("frbindlist binds correctly with base R only", {
  x <- lapply(1:100, function(i) data.frame(id = i, val = rnorm(2)))
  out <- frbindlist(x, ncores = 2)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 200)
  expect_named(out, c("id", "val"))
})

test_that("frbindlist handles empty or NULL elements", {
  x <- list(NULL, data.frame(id = 1, val = 2), NULL)
  out <- frbindlist(x)
  expect_equal(nrow(out), 1)
  expect_named(out, c("id", "val"))
})
