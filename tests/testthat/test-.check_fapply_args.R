test_that("check_fapply_args handles invalid ncores and pb", {
  expect_warning(res <- funr:::.check_fapply_args(1:3, sum, ncores = -1, pb = "maybe"))
  expect_equal(res$ncores, 1)
  expect_false(res$pb)
})
