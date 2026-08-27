test_that("to_well / to_num round-trip for common plates and both directions", {
  for (plate in c(96, 384, 1536)) {
    n <- plate
    for (wise in c("row", "col")) {
      labels <- convert_well(seq_len(n), "to_well", plate = plate, wise = wise)
      back <- convert_well(labels, "to_num", plate = plate, wise = wise)
      expect_equal(back, seq_len(n), info = paste(plate, wise))
    }
  }
})

test_that("row-wise and column-wise indices differ as expected on a 96-well plate", {
  expect_equal(convert_well("A01", "to_num", plate = 96, wise = "row"), 1)
  expect_equal(convert_well("B01", "to_num", plate = 96, wise = "row"), 13)
  expect_equal(convert_well("A02", "to_num", plate = 96, wise = "row"), 2)

  expect_equal(convert_well("A01", "to_num", plate = 96, wise = "col"), 1)
  expect_equal(convert_well("B01", "to_num", plate = 96, wise = "col"), 2)
  expect_equal(convert_well("A02", "to_num", plate = 96, wise = "col"), 9)
})

test_that("multi-letter rows work for 1536-well plates", {
  # "AA" is the 27th row; row-wise index = (27 - 1) * 48 + 1
  expect_equal(convert_well("AA01", "to_num", plate = 1536, wise = "row"), 26 * 48 + 1)
  expect_equal(convert_well(26 * 48 + 1, "to_well", plate = 1536, wise = "row"), "AA01")
})

test_that("invalid input is rejected", {
  expect_error(convert_well("Z99", "to_num", plate = 96), "invalid")
  expect_error(convert_well(1000, "to_well", plate = 96), "out of bounds")
  expect_error(convert_well(1, "to_num", plate = 96), "character")
  expect_error(convert_well("A01", "to_well", plate = 96), "numeric")
  expect_error(convert_well("A01", "to_num", plate = 96, wise = "diagonal"), "row.*col")
  expect_error(convert_well("A01", "to_num", plate = 100), "Unsupported plate size")
})
