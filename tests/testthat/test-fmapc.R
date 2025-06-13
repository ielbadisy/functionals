test_that("fmapc applies function to each column with column names", {
  df <- data.frame(a = 1:3, b = 4:6)
  out <- fmapc(df, function(x, name) paste0(name, ":", sum(x)))

  expect_length(out, ncol(df))
  expect_true(all(sapply(out, is.character)))
})

test_that("fmapc supports numeric summaries with different output", {
  df <- data.frame(a = 1:10, b = rnorm(10))
  out <- fmapc(df, function(x, name) mean(x))

  expect_length(out, 2)
  expect_true(all(sapply(out, is.numeric)))
})

test_that("fmapc supports parallel execution", {
  skip_on_cran()
  df <- data.frame(a = 1:100, b = 101:200)
  out <- fmapc(df, function(x, name) mean(x), ncores = 2, pb = TRUE)

  expect_equal(length(out), 2)
  expect_true(all(sapply(out, is.numeric)))
})
