test_that("well IDs are split into numeric Row and Column", {
  df <- data.frame(WellID = c("A1", "B12", "H1", "AA10"))
  out <- to_rows_columns(df, "WellID")
  expect_equal(out$Row, c(1, 2, 8, 27))
  expect_equal(out$Column, c(1, 12, 1, 10))
})

test_that("lowercase well IDs are accepted", {
  out <- to_rows_columns(data.frame(w = c("a1", "c5")), "w")
  expect_equal(out$Row, c(1, 3))
  expect_equal(out$Column, c(1, 5))
})

test_that("malformed input is rejected", {
  expect_error(to_rows_columns(data.frame(w = "1A"), "w"), "format")
  expect_error(to_rows_columns(data.frame(w = "A"), "w"), "format")
  expect_error(to_rows_columns(list(w = "A1"), "w"), "dataframe")
  expect_error(to_rows_columns(data.frame(w = "A1"), c("w", "x")), "single character string")
})
