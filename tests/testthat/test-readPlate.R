sample_plate <- readRDS(
  system.file("extdata", "SamplePlateShapedData.rds", package = "mtpR")
)

test_that("plate-shaped data is reshaped to long well/value form", {
  out <- readPlate(sample_plate, plate.size = 96)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("well", "value"))
  expect_equal(nrow(out), 96)
  expect_true(all(c("A1", "H12") %in% out$well))
  expect_type(out$value, "double")
  # spot-check a known cell (row A, column 1)
  expect_equal(out$value[out$well == "A1"], sample_plate[[2]][1])
})

test_that("value column can be renamed", {
  out <- readPlate(sample_plate, plate.size = 96, value_name = "absorbance")
  expect_named(out, c("well", "absorbance"))
})

test_that("a plate-size mismatch errors", {
  expect_error(readPlate(sample_plate, plate.size = 384), "384-well plate has 24 columns")
  expect_error(readPlate(sample_plate[, 1:5], plate.size = 96), "96-well plate has 12 columns")
})
