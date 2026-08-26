test_that("supported plate sizes return correct dimensions", {
  p96 <- platesize_check(96)
  expect_equal(p96$rowmax, 8)
  expect_equal(p96$colmax, 12)
  expect_equal(p96$rows, LETTERS[1:8])
  expect_equal(p96$columns, 1:12)

  p384 <- platesize_check(384)
  expect_equal(p384$rowmax, 16)
  expect_equal(p384$colmax, 24)
  expect_equal(p384$rows, LETTERS[1:16])
})

test_that("1536-well rows use multi-letter labels with no NAs", {
  p <- platesize_check(1536)
  expect_equal(p$rowmax, 32)
  expect_equal(p$colmax, 48)
  expect_length(p$rows, 32)
  expect_false(anyNA(p$rows))
  expect_equal(p$rows[27], "AA")
  expect_equal(p$rows[32], "AF")
})

test_that("unsupported plate sizes error", {
  expect_error(platesize_check(100), "Unsupported plate size")
  expect_error(platesize_check(0), "Unsupported plate size")
})
