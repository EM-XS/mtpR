# Focused tests for 1536-well plates, whose multi-letter row labels (A..Z, AA..AF)
# and 48-column geometry exercise code paths the smaller plates never reach.

test_that("platesize_check(1536) geometry and row labels are correct", {
  p <- platesize_check(1536)
  expect_equal(p$rowmax, 32)
  expect_equal(p$colmax, 48)
  expect_equal(p$columns, 1:48)
  expect_length(p$rows, 32)
  expect_false(anyNA(p$rows))
  expect_equal(p$rows[1:2], c("A", "B"))
  expect_equal(p$rows[26:32], c("Z", "AA", "AB", "AC", "AD", "AE", "AF"))
})

test_that("convert_well round-trips every well on a 1536 plate, both directions", {
  for (wise in c("row", "col")) {
    labels <- convert_well(1:1536, "to_well", plate = 1536, wise = wise)
    expect_equal(convert_well(labels, "to_num", plate = 1536, wise = wise), 1:1536,
                 info = wise)
    # labels are zero-padded to two digits and use multi-letter rows past Z
    expect_true(all(grepl("^[A-Z]{1,2}[0-9]{2}$", labels)))
    expect_true(any(grepl("^A[A-F]", labels)))
  }
})

test_that("row-wise indices for 1536 follow (row - 1) * 48 + col", {
  expect_equal(convert_well("A01", "to_num", plate = 1536, wise = "row"), 1)
  expect_equal(convert_well("A48", "to_num", plate = 1536, wise = "row"), 48)
  expect_equal(convert_well("B01", "to_num", plate = 1536, wise = "row"), 49)
  # AA is row 27, AF is row 32 (the last)
  expect_equal(convert_well("AA01", "to_num", plate = 1536, wise = "row"), 26 * 48 + 1)
  expect_equal(convert_well("AF48", "to_num", plate = 1536, wise = "row"), 1536)
  expect_equal(convert_well(1536, "to_well", plate = 1536, wise = "row"), "AF48")
})

test_that("column-wise indices for 1536 follow (col - 1) * 32 + row", {
  expect_equal(convert_well("A01", "to_num", plate = 1536, wise = "col"), 1)
  expect_equal(convert_well("AF01", "to_num", plate = 1536, wise = "col"), 32)
  expect_equal(convert_well("A02", "to_num", plate = 1536, wise = "col"), 33)
  expect_equal(convert_well("AF48", "to_num", plate = 1536, wise = "col"), 1536)
  expect_equal(convert_well(1536, "to_well", plate = 1536, wise = "col"), "AF48")
})

test_that("convert_well rejects out-of-range 1536 wells", {
  expect_error(convert_well("AG01", "to_num", plate = 1536), "invalid")
  expect_error(convert_well("A49", "to_num", plate = 1536), "columns")
  expect_error(convert_well(1537, "to_well", plate = 1536), "out of bounds")
})

test_that("to_rows_columns resolves multi-letter 1536 rows", {
  out <- to_rows_columns(
    data.frame(w = c("A1", "Z48", "AA1", "AF48")), "w"
  )
  expect_equal(out$Row, c(1, 26, 27, 32))
  expect_equal(out$Column, c(1, 48, 1, 48))
})

test_that("a 1536 design round-trips through the layout pipeline", {
  d <- data.frame(rxn = 1:4) |>
    replicate_reactions("rxn", num_replicates = 2, priority = "Reaction",
                        inter_spacing = 1, intra_spacing = 1, start_position = 1) |>
    add_plates("position", plate_size = 1536) |>
    dplyr::mutate(well = convert_well(Well.Index, "to_well", plate = 1536))
  expect_equal(nrow(d), 8)
  expect_true(all(d$Plate.Index == 1))
  expect_equal(d$well, convert_well(1:8, "to_well", plate = 1536))
})
