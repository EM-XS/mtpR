test_that("strict accepts only the standard form", {
  expect_equal(clean_wells(c("A01", "H12"), mode = "strict"), c("A01", "H12"))
  expect_equal(clean_wells("AF48", plate = 1536, mode = "strict"), "AF48")
  for (bad in c("A1", "a01", "A001", " A01", "A-1")) {
    expect_error(clean_wells(bad, mode = "strict"), "standard form")
  }
})

test_that("common accepts case, padding, whitespace and one separator", {
  input <- c("A1", "a1", "a01", "A001", " A1", "A1 ", "A 1", "A-1", "A_1", "A:1")
  expect_equal(clean_wells(input), rep("A01", length(input)))
  for (bad in c("A1B2", "1A", "R1C1", "Well A1", "A--1", "A1.0", "A", "1", "")) {
    expect_error(clean_wells(bad), "not in the form")
  }
})

test_that("two-letter rows are kept whole", {
  expect_equal(clean_wells(c("AA1", "aa01", "A1"), plate = 1536), c("AA01", "AA01", "A01"))
  for (mode in c("strict", "common", "guess")) {
    expect_error(clean_wells("AA01", plate = 96, mode = mode), "plate")
    expect_error(clean_wells("AA01", plate = 384, mode = mode), "plate")
  }
  expect_false(identical(clean_wells("AA01", plate = 1536), clean_wells("A01", plate = 1536)))
})

test_that("wells off the plate and missing labels are errors in every mode", {
  for (mode in c("strict", "common", "guess")) {
    expect_error(clean_wells("I01", plate = 96, mode = mode), "plate")
    expect_error(clean_wells("A13", plate = 96, mode = mode), "plate")
    expect_error(clean_wells("A00", plate = 96, mode = mode), "plate")
    expect_error(clean_wells(c("A01", NA), mode = mode), "missing")
    expect_error(clean_wells(1:3, mode = mode), "character vector")
  }
})

test_that("bare numbers are not read, even by guess", {
  expect_error(clean_wells(5, mode = "guess"), "character vector")
  expect_error(clean_wells("5", mode = "guess"), "one format")
})

test_that("every well on every plate survives each mode unchanged", {
  for (plate in c(6, 12, 24, 48, 96, 384, 1536)) {
    wells <- convert_well(seq_len(plate), "to_well", plate = plate)
    expect_equal(clean_wells(wells, plate = plate, mode = "strict"), wells)
    expect_equal(clean_wells(wells, plate = plate), wells)
    expect_equal(as.vector(clean_wells(wells, plate = plate, mode = "guess")), wells)
    expect_equal(clean_wells(sub("^([A-Z]+)0", "\\1", wells), plate = plate), wells)
  }
})

test_that("factors, repeats and empty input are handled", {
  expect_equal(clean_wells(factor(c("B2", "A1"))), c("B02", "A01"))
  expect_equal(clean_wells(c("A1", "A1", "A01")), rep("A01", 3))
  expect_equal(clean_wells(character()), character())
  expect_equal(clean_wells(character(), mode = "guess"), character())
})

test_that("guess reads each format", {
  guess <- function(x, ...) {
    as.vector(suppressMessages(clean_wells(x, mode = "guess", ...)))
  }
  expect_equal(guess(c("1A", "12H")), c("A01", "H12"))
  expect_equal(guess(c("R1C1", "r02c12", "R8-C12")), c("A01", "B12", "H12"))
  expect_equal(guess(c("1,1", "2, 12", "(8,12)", "3-4")), c("A01", "B12", "H12", "C04"))
  expect_equal(guess(c("Well A1", "Plate1_A02", "B3 sample")), c("A01", "A02", "B03"))
  expect_equal(guess(c("A1", "Well A2")), c("A01", "A02"))
})

test_that("guess never reads a label two ways or from a fragment", {
  for (bad in c("A1B2", "A1.5", "Sample", "A1 B2", "A", "1A2B")) {
    expect_error(clean_wells(bad, mode = "guess"), "one format", info = bad)
  }
  # P is a row on a 384-well plate, so there are two wells in the text
  expect_equal(suppressMessages(as.vector(clean_wells("P1_A01", mode = "guess"))), "A01")
  expect_error(clean_wells("P1_A01", plate = 384, mode = "guess"), "one format")
  # read as a well in one format and as another well in a second
  expect_error(clean_wells("R1-C2", mode = "guess"), "more than one way")
})

test_that("guess needs one format for all of x", {
  expect_error(clean_wells(c("A1", "1A"), mode = "guess"), "one format")
  expect_error(clean_wells(c("R1C1", "2,2"), mode = "guess"), "one format")
})

test_that("guess reports what it changed", {
  expect_message(out <- clean_wells(c("R1C1", "R1C1", "R2C2"), mode = "guess"),
                 "3 label(s) using format \"R1C1\": R1C1 -> A01, ...", fixed = TRUE)
  expect_equal(attr(out, "mapping"),
               data.frame(input = c("R1C1", "R2C2"), well = c("A01", "B02")),
               ignore_attr = "row.names")
  expect_no_message(clean_wells("A01", mode = "guess"))
})

test_that("functions across the package agree on labels", {
  plate <- place_controls(96, wells = "A1", name = "ctl")
  expect_error(plate_roles(plate, "a01"), "reserved")
  expect_equal(well_quadrant("A 1"), well_quadrant("A01"))
  expect_equal(plate_picker_value(list(well = "A 1", label = "x"))$well, "A01")
  expect_equal(read_plate(readRDS(system.file("extdata", "SamplePlateShapedData.rds",
                                              package = "mtpR")))$well[1],
               convert_well(1, "to_well"))
})
