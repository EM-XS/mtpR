test_that("a layout that fits reports used, reserved and left", {
  cap <- plate_capacity(100, replicates = 3, plate = 384, controls = 12)
  expect_true(cap$fits)
  expect_equal(c(cap$used, cap$reserved, cap$left), c(300, 12, 72))
  expect_equal(cap$plates_needed, 1)
})

test_that("controls can come from place_controls", {
  ctl <- place_controls(96, wells = c("A01", "A12", "H01", "H12"))
  cap <- plate_capacity(20, 3, plate = 96, controls = ctl)
  expect_equal(c(cap$used, cap$reserved, cap$left), c(60, 4, 32))
  expect_error(plate_capacity(20, 3, plate = 384, controls = ctl), "96-well")
})

test_that("a layout that does not fit errors informatively or returns FALSE", {
  expect_error(plate_capacity(100, 3, plate = 96), "does not fit.*300 wells.*4 plates|does not fit.*300 wells")
  expect_false(plate_capacity(100, 3, plate = 96, error = FALSE))
})

test_that("copies need their own plates", {
  expect_false(plate_capacity(100, 3, copies = 2, plate = 384, error = FALSE))
  cap <- plate_capacity(100, 3, copies = 2, plate = 384, plates = 2, controls = 8)
  expect_equal(c(cap$used, cap$reserved, cap$left, cap$plates_needed), c(600, 16, 152, 2))
})

test_that("controls take wells from every plate used", {
  # 384 - 90 = 294 free per plate, 300 wells needed: two plates, 180 reserved
  expect_false(plate_capacity(100, 3, plate = 384, controls = 90, error = FALSE))
  cap <- plate_capacity(100, 3, plate = 384, controls = 90, plates = 2)
  expect_equal(c(cap$reserved, cap$plates_needed), c(180, 2))
})

test_that("per-reaction replicates and spacing are counted", {
  cap <- plate_capacity(replicates = c(1, 2, 3), plate = 96)
  expect_equal(cap$used, 6)
  expect_false(plate_capacity(50, 1, plate = 96, spacing = 2, error = FALSE))
  expect_true(plate_capacity(48, 1, plate = 96, spacing = 2, error = FALSE)$fits)
  expect_error(plate_capacity(3, replicates = c(1, 2)), "entries")
})

test_that("bad input errors", {
  expect_error(plate_capacity(10, 0), "at least 1")
  expect_error(plate_capacity(10, 1, plate = 100), "Unsupported")
  expect_error(plate_capacity(10, 1, plate = 6, controls = 6), "reserved")
})

test_that("plate_capacity counts squares for quadrants", {
  cap <- plate_capacity(20, replicates = 3, plate = 96, pattern = "quadrants")
  expect_equal(c(cap$used, cap$skipped, cap$left, cap$plates_needed), c(60, 0, 36, 1))
  # 25 reactions x 4 replicates need 25 squares > 24 per plate
  expect_false(plate_capacity(25, 4, plate = 96, pattern = "quadrants", error = FALSE))
  cap <- plate_capacity(25, 4, plate = 96, pattern = "quadrants", plates = 2)
  expect_equal(cap$plates_needed, 2)
  # per-reaction counts: 1 + 2 + 1 squares
  cap <- plate_capacity(replicates = c(1, 6, 2), plate = 96, pattern = "quadrants")
  expect_equal(cap$used, 9)
  # spacing counts squares: 24 reactions at spacing 2 need 47 squares
  expect_false(plate_capacity(24, 1, plate = 96, pattern = "quadrants", spacing = 2, error = FALSE))
  expect_true(plate_capacity(12, 1, plate = 96, pattern = "quadrants", spacing = 2)$fits)
  expect_error(plate_capacity(30, 4, plate = 96, pattern = "quadrants"), "squares")
})

test_that("plate_capacity skips squares that hold controls", {
  ctl <- place_controls(96, wells = c("A03", "A04", "D12"))  # two squares blocked
  cap <- plate_capacity(22, 4, plate = 96, controls = ctl, pattern = "quadrants")
  expect_equal(cap$plates_needed, 1)
  expect_equal(cap$reserved, 3)
  expect_equal(cap$skipped, 2 * 4 - 3)
  expect_equal(cap$left, 96 - 88 - 3 - 5)
  # one square too many for the 22 free squares
  expect_false(plate_capacity(23, 4, plate = 96, controls = ctl, pattern = "quadrants", error = FALSE))
  expect_error(plate_capacity(5, 4, plate = 96, controls = 3, pattern = "quadrants"), "place_controls")
  expect_error(plate_capacity(5, 4, plate = 96, controls = place_controls(96, "quadrant", quadrant = 1),
                              pattern = "quadrants"), "Every square")
  # capacity agrees with what replicate_reactions does
  out <- replicate_reactions(data.frame(rxn = 1:22), "rxn", 4, pattern = "quadrants", reserved = ctl)
  expect_equal(max(out$Well.Index), 96)
  expect_gt(max(replicate_reactions(data.frame(rxn = 1:23), "rxn", 4, pattern = "quadrants",
                                    reserved = ctl)$Well.Index), 96)
})
