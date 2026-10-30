sample_plate <- readRDS(
  system.file("extdata", "SamplePlateShapedData.rds", package = "mtpR")
)

test_that("plate-shaped data is reshaped to long well/value form", {
  out <- read_plate(sample_plate, plate_size = 96)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("well", "value"))
  expect_equal(nrow(out), 96)
  expect_true(all(c("A1", "H12") %in% out$well))
  expect_type(out$value, "double")
  # spot-check a known cell (row A, column 1)
  expect_equal(out$value[out$well == "A1"], sample_plate[[2]][1])
})

test_that("value column can be renamed", {
  out <- read_plate(sample_plate, plate_size = 96, value_name = "absorbance")
  expect_named(out, c("well", "absorbance"))
})

test_that("a plate-size mismatch errors", {
  expect_error(read_plate(sample_plate, plate_size = 384), "384-well plate has 24 columns")
  expect_error(read_plate(sample_plate[, 1:5], plate_size = 96), "96-well plate has 12 columns")
})

test_that("cells that map to one well are an error", {
  numbered <- sample_plate
  names(numbered)[-1] <- paste0("Plate1_col", 1:12)
  expect_error(read_plate(numbered), "same well")

  repeated <- sample_plate
  repeated[2, 1] <- repeated[1, 1]
  expect_error(read_plate(repeated), "same well")
})

test_that("value_name cannot replace the well column", {
  expect_error(read_plate(sample_plate, value_name = "well"), "cannot be \"well\"")
})

test_that("readPlate() is a deprecated alias", {
  rlang::reset_warning_verbosity("mtpR_readPlate")
  expect_warning(out <- readPlate(sample_plate, plate.size = 96), "read_plate")
  expect_equal(out, read_plate(sample_plate, plate_size = 96))
})
