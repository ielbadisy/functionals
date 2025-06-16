test_that("freduce sums numbers correctly", {
  expect_equal(freduce(1:5, `+`), 15)
})

test_that("freduce concatenates strings", {
  expect_equal(freduce(letters[1:4], paste0), "abcd")
})

test_that("freduce works with a custom binary function", {
  out <- freduce(list(1, 2, 3), function(x, y) x * y)
  expect_equal(out, 6)
})

test_that("freduce handles init argument", {
  expect_equal(freduce(1:3, `+`, .init = 10), 16)
})


test_that("freduce supports right-to-left reduction", {
  expect_equal(freduce(1:3, function(x, y) paste0(x, y), .right = TRUE), "123")
})
