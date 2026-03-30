library(functionals)

capture_progress_output <- function(expr) {
  paste(capture.output(force(expr), type = "output"), collapse = "\n")
}

progress_lines <- function(output) {
  lines <- strsplit(gsub("\r", "\n", output, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  lines[nzchar(trimws(lines))]
}

test_that("fapply stays silent when progress is disabled", {
  output <- capture_progress_output({
    out <- fapply(1:3, identity, pb = FALSE)
  })

  expect_identical(output, "")
  expect_equal(unname(out), as.list(1:3))
})

test_that("sequential progress uses task totals and reports ETA from completed work", {
  skip_on_cran()

  output <- capture_progress_output({
    out <- fapply(1:4, function(x) {
      Sys.sleep(1.05)
      x
    }, pb = TRUE)
  })

  lines <- progress_lines(output)
  counts <- c("0/4", "1/4", "2/4", "3/4", "4/4")

  expect_true(all(vapply(counts, function(count) any(grepl(count, lines, fixed = TRUE)), logical(1))))
  expect_true(any(grepl("elapsed", lines, fixed = TRUE)))
  expect_true(any(grepl("eta", lines[grepl("1/4", lines, fixed = TRUE)], fixed = TRUE)))
  expect_true(any(grepl("eta", lines, fixed = TRUE)))
})

test_that("cluster-backed progress counts completed tasks rather than chunks", {
  skip_on_cran()

  cl <- parallel::makeCluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  output <- capture_progress_output({
    out <- fapply(as.list(1:4), function(x) {
      Sys.sleep(c(0.12, 0.24, 0.36, 0.48)[[x]])
      x
    }, cl = cl, pb = TRUE)
  })

  lines <- progress_lines(output)
  counts <- c("1/4", "2/4", "3/4", "4/4")

  expect_true(all(vapply(counts, function(count) any(grepl(count, lines, fixed = TRUE)), logical(1))))
  expect_false(any(grepl("2/2", lines, fixed = TRUE)))
  expect_equal(unname(out), as.list(1:4))
})

test_that("multicore progress counts completed tasks rather than chunks", {
  skip_on_cran()
  skip_on_os("windows")

  output <- capture_progress_output({
    out <- fapply(as.list(1:4), function(x) {
      Sys.sleep(c(0.12, 0.24, 0.36, 0.48)[[x]])
      x
    }, ncores = 2, pb = TRUE)
  })

  lines <- progress_lines(output)
  counts <- c("1/4", "2/4", "3/4", "4/4")

  expect_true(all(vapply(counts, function(count) any(grepl(count, lines, fixed = TRUE)), logical(1))))
  expect_false(any(grepl("2/2", lines, fixed = TRUE)))
  expect_equal(unname(out), as.list(1:4))
})

test_that("messages do not produce duplicate completed progress lines", {
  skip_on_cran()

  output <- capture_progress_output({
    out <- fapply(1:2, function(x) {
      message(sprintf("msg-%s", x))
      x
    }, pb = TRUE)
  })

  lines <- progress_lines(output)
  completed_lines <- lines[grepl("100% 2/2", lines, fixed = TRUE)]

  expect_lte(length(completed_lines), 1)
  expect_equal(unname(out), as.list(1:2))
})

test_that("cluster-backed progress propagates task errors", {
  skip_on_cran()

  cl <- parallel::makeCluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  expect_error(
    fapply(as.list(1:4), function(x) {
      if (x == 3) stop("cluster-boom")
      x
    }, cl = cl, pb = TRUE),
    "cluster-boom"
  )
})

test_that("cluster-backed progress refreshes elapsed time while waiting", {
  skip_on_cran()

  cl <- parallel::makeCluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  output <- capture_progress_output({
    out <- fapply(as.list(1:2), function(x) {
      Sys.sleep(c(1.2, 1.6)[[x]])
      x
    }, cl = cl, pb = TRUE)
  })

  lines <- progress_lines(output)

  expect_true(any(grepl("0/2 elapsed 00:01", lines, fixed = TRUE)))
  expect_equal(unname(out), as.list(1:2))
})

test_that("multicore progress propagates task errors", {
  skip_on_cran()
  skip_on_os("windows")

  expect_error(
    fapply(as.list(1:4), function(x) {
      if (x == 3) stop("multicore-boom")
      x
    }, ncores = 2, pb = TRUE),
    "multicore-boom"
  )
})

test_that("multicore progress refreshes elapsed time while waiting", {
  skip_on_cran()
  skip_on_os("windows")

  output <- capture_progress_output({
    out <- fapply(as.list(1:2), function(x) {
      Sys.sleep(c(1.2, 1.6)[[x]])
      x
    }, ncores = 2, pb = TRUE)
  })

  lines <- progress_lines(output)

  expect_true(any(grepl("0/2 elapsed 00:01", lines, fixed = TRUE)))
  expect_equal(unname(out), as.list(1:2))
})
