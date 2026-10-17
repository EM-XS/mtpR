rx4 <- data.frame(rxn = 1:4)
wells <- function(out, plate = 96, wise = "row") {
  convert_well(out$position, "to_well", plate = plate, wise = wise)
}
quad <- function(df, reps, plate = 96, ...) {
  out <- replicate_reactions(df, "rxn", reps, pattern = "quadrants", plate = plate, ...)
  dots <- list(...)
  out$Well <- wells(out, plate, if (is.null(dots$fill_wise)) "row" else dots$fill_wise)
  out
}

test_that("4 replicates fill one square: replicate k in quadrant k", {
  out <- quad(rx4, 4)
  expect_equal(out$Well[out$rxn == 1], c("A01", "A02", "B01", "B02"))
  expect_equal(out$Well[out$rxn == 2], c("A03", "A04", "B03", "B04"))
  expect_equal(out$Quadrant, rep(1:4, 4))
  expect_equal(well_quadrant(out$Well, 96), out$Quadrant)
  expect_equal(attr(out, "pattern"), "quadrants")
  expect_equal(anyDuplicated(out$position), 0L)
})

test_that("fewer than 4 replicates leave the other positions empty", {
  for (r in 1:3) {
    out <- quad(rx4, r)
    expect_equal(nrow(out), 4 * r)
    expect_equal(out$Quadrant, rep(seq_len(r), 4))
    expect_equal(well_quadrant(out$Well, 96), out$Quadrant)
    # each reaction still uses its own square
    sq <- paste((match(substr(out$Well, 1, 1), LETTERS) - 1) %/% 2,
                (as.numeric(substr(out$Well, 2, 3)) - 1) %/% 2)
    expect_equal(length(unique(sq)), 4)
  }
  out <- quad(rx4, 3)
  expect_equal(out$Well[out$rxn == 1], c("A01", "A02", "B01"))
  expect_false("B02" %in% out$Well)
})

test_that("more than 4 replicates spill into further squares", {
  out <- quad(data.frame(rxn = 1:2), 5)
  expect_equal(out$Well[out$rxn == 1], c("A01", "A02", "B01", "B02", "A03"))
  expect_equal(out$Well[out$rxn == 2], c("A05", "A06", "B05", "B06", "A07"))
  out <- quad(data.frame(rxn = 1:2), 8)
  expect_equal(out$Well[out$rxn == 1], c("A01", "A02", "B01", "B02", "A03", "A04", "B03", "B04"))
  expect_equal(out$Well[out$rxn == 2][1], "A05")
  expect_equal(out$Quadrant[out$rxn == 1], rep(1:4, 2))
  # 6 replicates: 2 squares, positions 3-4 of the second square empty
  out <- quad(data.frame(rxn = 1:2), 6)
  expect_equal(out$Well[out$rxn == 1], c("A01", "A02", "B01", "B02", "A03", "A04"))
  expect_equal(out$Well[out$rxn == 2][1], "A05")
  expect_false(any(c("B03", "B04") %in% out$Well))
})

test_that("per-reaction replicate counts are laid out square by square", {
  out <- quad(data.frame(rxn = 1:3, n = c(1, 6, 2)), "n")
  expect_equal(out$Well, c("A01", "A03", "A04", "B03", "B04", "A05", "A06", "A07", "A08"))
  expect_equal(out$Quadrant, c(1, 1, 2, 3, 4, 1, 2, 1, 2))
  out2 <- quad(data.frame(rxn = 1:3), c(1, 6, 2))
  expect_equal(out2$Well, out$Well)
})

test_that("squares fill across rows of squares, or down columns with fill_wise = 'col'", {
  out <- quad(data.frame(rxn = 1:7), 1)
  # 96-well plate has 6 squares per row of squares: 7th reaction is in the next row of squares
  expect_equal(out$Well, c("A01", "A03", "A05", "A07", "A09", "A11", "C01"))
  out <- replicate_reactions(data.frame(rxn = 1:5), "rxn", 4, pattern = "quadrants",
                             plate = 96, fill_wise = "col")
  w <- convert_well(out$position, "to_well", plate = 96, wise = "col")
  expect_equal(w[out$rxn == 1], c("A01", "A02", "B01", "B02"))
  # 4 squares down a column of squares, then on to the next column of squares
  expect_equal(w[out$rxn == 2][1], "C01")
  expect_equal(w[out$rxn == 4][1], "G01")
  expect_equal(w[out$rxn == 5][1], "A03")
  expect_equal(out$Quadrant[1:4], 1:4)
})

test_that("quadrants work on 384, 24 and 48 wells; 12 and 6 error", {
  out <- quad(data.frame(rxn = 1:96), 4, plate = 384)
  # quadrant 1 of a 384 plate is the 96-well grid: odd rows and columns
  rows <- match(substr(out$Well, 1, 1), LETTERS)
  cols <- as.numeric(substr(out$Well, 2, 3))
  expect_true(all(rows[out$Quadrant == 1] %% 2 == 1 & cols[out$Quadrant == 1] %% 2 == 1))
  expect_equal(length(unique(out$Well)), 384)
  expect_equal(max(out$position), 384)
  expect_equal(max(quad(data.frame(rxn = 1:6), 4, plate = 24)$position), 24)
  expect_equal(max(quad(data.frame(rxn = 1:12), 4, plate = 48)$position), 48)
  expect_error(replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", plate = 12), "even")
  expect_error(replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", plate = 6), "even")
})

test_that("squares run on to a further plate", {
  out <- replicate_reactions(data.frame(rxn = 1:25), "rxn", 4, pattern = "quadrants", plate = 96)
  # 24 squares per 96-well plate
  expect_equal(out$position[out$rxn == 25], 96 + c(1, 2, 13, 14))
  expect_equal(out$Quadrant[out$rxn == 25], 1:4)
})

test_that("a start well that is not a square's top-left moves forward with a message", {
  expect_message(
    out <- replicate_reactions(rx4, "rxn", 2, pattern = "quadrants", plate = 96,
                               start_position = 2),
    "moved start from A02 to A03"
  )
  expect_equal(wells(out)[out$rxn == 1], c("A03", "A04"))
  expect_message(
    replicate_reactions(rx4, "rxn", 2, pattern = "quadrants", plate = 96, start_position = 13),
    "from B01 to A03"
  )
  expect_message(
    replicate_reactions(rx4, "rxn", 2, pattern = "quadrants", plate = 96, start_position = 14),
    "from B02 to A03"
  )
  # row C col 1 is a top-left: no message
  expect_message(
    out <- replicate_reactions(rx4, "rxn", 2, pattern = "quadrants", plate = 96,
                               start_position = 25),
    NA
  )
  expect_equal(wells(out)[1], "C01")
  # the last square of a row of squares: B/A 11-12 -> next row of squares
  expect_message(
    out <- replicate_reactions(rx4, "rxn", 2, pattern = "quadrants", plate = 96,
                               start_position = 12),
    "from A12 to C01"
  )
  expect_equal(wells(out)[1], "C01")
})

test_that("inter_spacing counts squares; intra_spacing is ignored with a warning", {
  out <- quad(rx4, 4, inter_spacing = 2)
  expect_equal(out$Well[out$Quadrant == 1], c("A01", "A05", "A09", "C01"))
  out3 <- quad(data.frame(rxn = 1:2), 5, inter_spacing = 3)
  # reaction 1 takes squares 1-2, then two empty squares, reaction 2 starts in square 5
  expect_equal(out3$Well[out3$rxn == 2][1], "A09")
  expect_warning(
    replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", plate = 96, intra_spacing = 2),
    "intra_spacing"
  )
  expect_warning(replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", plate = 96), NA)
  expect_error(replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", plate = 96,
                                   inter_spacing = 1.5), "whole")
})

test_that("squares containing a reserved well are skipped whole", {
  ctl <- place_controls(96, wells = c("A03", "D12"))
  out <- replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", reserved = ctl)
  w <- wells(out)
  # square 2 (A03) is skipped: reaction 2 takes the third square
  expect_equal(w[out$rxn == 1], c("A01", "A02", "B01", "B02"))
  expect_equal(w[out$rxn == 2][1], "A05")
  expect_false(any(c("A03", "A04", "B03", "B04") %in% w))
  expect_equal(out$Quadrant, rep(1:4, 4))
  expect_equal(well_quadrant(w, 96), out$Quadrant)
  # a reserved quadrant blocks every square
  q4 <- place_controls(96, "quadrant", quadrant = 4)
  expect_error(replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", reserved = q4),
               "Every square")
  # reserved wells as labels need the plate
  out2 <- replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", reserved = "A03", plate = 96)
  expect_equal(out2$position, out$position)
  expect_equal(wells(out2)[out2$rxn == 2][1], "A05")
})

test_that("plate is needed for quadrants and alias/other errors work", {
  expect_error(replicate_reactions(rx4, "rxn", 4, pattern = "quadrants"), "plate")
  out <- replicate_reactions(rx4, "rxn", 4, pattern = "Quadrant", plate = 96)
  expect_equal(attr(out, "pattern"), "quadrants")
  expect_error(replicate_reactions(rx4, "rxn", 4, pattern = "diag"), "quadrants")
  expect_error(replicate_reactions(rx4, "rxn", 4, pattern = "quadrants", plate = 96,
                                   controls_spread = 2), "quadrants")
  # other patterns do not get a Quadrant column
  expect_false("Quadrant" %in% names(replicate_reactions(rx4, "rxn", 4, pattern = "grouped")))
})

test_that("controls_spread interleaves controls among reactions in one call", {
  out <- replicate_reactions(data.frame(rxn = 1:40), "rxn", 1, pattern = "grouped",
                             plate = 96, controls_spread = 8)
  pm <- attr(out, "plate_map")
  expect_s3_class(pm, "mtpr_plate")
  pic <- paste(ifelse(pm$Role[1:48] == "control", "C", "R"), collapse = "")
  expect_equal(pic, strrep("RRRRRC", 8))
  expect_equal(sum(pm$Role == "control"), 8)
  expect_equal(sum(pm$Role == "reaction"), 40)
  expect_equal(sort(out$position), setdiff(1:48, seq(6, 48, by = 6)))
  # controls are named
  out2 <- replicate_reactions(data.frame(rxn = 1:40), "rxn", 1, pattern = "grouped",
                              plate = 96, controls_spread = 8, controls_name = "pos")
  expect_true(all(attr(out2, "plate_map")$Control[attr(out2, "plate_map")$Role == "control"] == "pos"))
  # same result as doing it in two steps with place_controls()
  ctl <- place_controls(96, "spread", n = 8, among = 40)
  two <- replicate_reactions(data.frame(rxn = 1:40), "rxn", 1, pattern = "grouped", reserved = ctl)
  expect_equal(two$position, out$position)
})

test_that("controls_spread respects start, replicates, reserved wells and spacing", {
  out <- replicate_reactions(data.frame(rxn = 1:10), "rxn", 2, pattern = "grouped",
                             controls_spread = 4, plate = 96, start_position = 13)
  pm <- attr(out, "plate_map")
  expect_equal(which(pm$Role == "control"), 12 + c(6, 12, 18, 24))
  expect_equal(min(out$position), 13)
  expect_equal(nrow(out), 20)
  # existing controls are kept and not counted as free wells
  base <- place_controls(96, wells = "A02", name = "blank")
  out <- replicate_reactions(data.frame(rxn = 1:4), "rxn", 1, pattern = "grouped",
                             reserved = base, controls_spread = 2)
  pm <- attr(out, "plate_map")
  expect_equal(pm$Control[pm$Well == "A02"], "blank")
  expect_equal(sum(pm$Role == "control"), 3)
  expect_equal(sum(pm$Role == "reaction"), 4)
  # spacing 2: reactions every other free well; the controls are spread over that span
  out <- replicate_reactions(data.frame(rxn = 1:5), "rxn", 1, pattern = "grouped",
                             inter_spacing = 2, controls_spread = 1, plate = 96)
  expect_equal(sum(attr(out, "plate_map")$Role == "control"), 1)
  expect_equal(anyDuplicated(out$position), 0L)
  # column-wise fill
  out <- replicate_reactions(data.frame(rxn = 1:10), "rxn", 1, pattern = "grouped",
                             controls_spread = 2, plate = 96, fill_wise = "col")
  pm <- attr(out, "plate_map")
  expect_setequal(pm$Well[pm$Role == "control"], c("F01", "D02"))
})

test_that("controls_spread errors when it cannot be done on one plate", {
  expect_error(replicate_reactions(data.frame(rxn = 1:100), "rxn", 1, pattern = "grouped",
                                   plate = 96, controls_spread = 8), "Cannot spread")
  expect_error(replicate_reactions(rx4, "rxn", 1, pattern = "grouped", controls_spread = 2),
               "plate")
  # scattered works too
  out <- replicate_reactions(data.frame(rxn = 1:20), "rxn", 1, pattern = "scattered",
                             seed = 1, plate = 96, controls_spread = 4)
  expect_equal(sum(attr(out, "plate_map")$Role == "reaction"), 20)
})
