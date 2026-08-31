test_that("default .on_error = 'stop' aborts like lapply", {
  expect_error(fmap(list(1, "a", 3), function(x) x * 2))
})

test_that(".on_error = 'pass' keeps going and returns error sentinels", {
  expect_warning(
    fmap(list(1, "a", 3), function(x) x * 2, .on_error = "pass"),
    "1 of 3"
  )
  res <- suppressWarnings(fmap(list(1, "a", 3), function(x) x * 2, .on_error = "pass"))
  expect_equal(res[[1]], 2)
  expect_equal(res[[3]], 6)
  expect_s3_class(res[[2]], "functionals_error")
  expect_type(conditionMessage(res[[2]]), "character")
})

test_that(".on_error = 'fill' substitutes .fill", {
  res <- fmap(list(1, "a", 3), function(x) x * 2, .on_error = "fill", .fill = NA_real_)
  expect_identical(res, list(2, NA_real_, 6))
})

test_that(".on_error behaves identically in parallel", {
  skip_on_cran()
  if (.Platform$OS.type == "windows") skip("fork/queue path")
  res <- suppressWarnings(
    fmap(list(1, "a", 3), function(x) x * 2, .on_error = "pass", ncores = 2)
  )
  expect_s3_class(res[[2]], "functionals_error")
  expect_equal(res[[c(3)]], 6)
})

test_that(".on_error threads through every wrapper", {
  expect_s3_class(
    suppressWarnings(fcv(list(1, "x", 3), function(s) s + 1, .on_error = "pass"))[[2]],
    "functionals_error"
  )
  expect_s3_class(
    suppressWarnings(fmapg(
      data.frame(g = c("a", "a", "b"), v = c(1, 2, 3)),
      function(d) if (d$g[1] == "b") stop("boom") else sum(d$v),
      by = "g", .on_error = "pass"
    ))[["b"]],
    "functionals_error"
  )
})
