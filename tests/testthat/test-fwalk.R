test_that("fwalk executes function for side effects", {
  calls <- character()
  fwalk(1:3, function(x) calls <<- c(calls, paste0("call", x)))
  expect_equal(calls, c("call1", "call2", "call3"))
})

test_that("fwalk returns input invisibly", {
  expect_invisible(fwalk(1:3, function(x) NULL))
})


test_that("fwalk returns input and does so invisibly", {
  out <- fwalk(1:3, function(x) NULL)
  expect_equal(out, 1:3)
  expect_invisible(fwalk(1:3, function(x) NULL))
})


test_that("fwalk supports parallel execution with progress", {
  skip_on_cran()
  log <- tempfile()
  fwalk(1:4, function(i) {
    cat(paste("Task", i), file = log, append = TRUE, sep = "\n")
  }, ncores = 2, pb = TRUE)

  lines <- readLines(log)
  expect_length(lines, 4)
  expect_true(all(grepl("Task", lines)))
})
