test_that("add_plates wraps well indices across plates", {
  out <- add_plates(data.frame(pos = c(1, 96, 97, 192, 193)), "pos", 96)
  expect_equal(out$Plate.Index, c(1, 1, 2, 2, 3))
  expect_equal(out$Well.Index, c(1, 96, 1, 96, 1))
})

test_that("a single full plate stays on plate 1", {
  out <- add_plates(data.frame(Well.Index = 1:96), "Well.Index", 96)
  expect_true(all(out$Plate.Index == 1))
  expect_equal(out$Well.Index, 1:96)
})

test_that("missing column errors", {
  expect_error(add_plates(data.frame(a = 1), "pos", 96), "does not exist")
  expect_error(add_plates(data.frame(a = 1), c("a", "b"), 96), "single character string")
})
