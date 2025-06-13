test_that("fmapg applies function to grouped data", {
  df <- data.frame(
    id = rep(1:3, each = 2),
    value = c(10, 20, 30, 40, 50, 60)
  )

  out <- fmapg(df, function(group) sum(group$value), by = "id")

  expect_length(out, 3)
  expect_true(all(sapply(out, is.numeric)))
})

test_that("fmapg supports parallel execution with progress", {
  skip_on_cran()
  df <- data.frame(
    g = rep(letters[1:4], each = 5),
    x = rnorm(20)
  )

  out <- fmapg(df, function(gr) mean(gr$x), by = "g", ncores = 2, pb = TRUE)

  expect_equal(length(out), 4)
  expect_true(all(sapply(out, is.numeric)))
})

test_that("fmapg errors with missing group variables", {
  df <- data.frame(a = 1:3)
  expect_error(fmapg(df, identity, by = "missing_column"))
})
