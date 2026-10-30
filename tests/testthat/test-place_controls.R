roles <- function(x) table(factor(x$Role, c("control", "reaction", "empty")))

test_that("fixed wells are reserved and the plate has one row per well", {
  p <- place_controls(96, wells = c("A1", "H12"))
  expect_s3_class(p, "mtpr_plate")
  expect_equal(nrow(p), 96)
  expect_equal(p$Well[p$Role == "control"], c("A01", "H12"))
  expect_equal(sum(p$Role == "empty"), 94)
  expect_equal(p$Well.Index[p$Well == "H12"], 96)
})

test_that("edge reserves the outer ring", {
  p <- place_controls(96, "edge")
  expect_equal(sum(p$Role == "control"), 2 * 12 + 2 * 6)
  expect_false(any(p$Role[p$Row %in% 2:7 & p$Column %in% 2:11] == "control"))
  expect_equal(sum(place_controls(384, "edge")$Role == "control"), 2 * 24 + 2 * 14)
})

test_that("every Nth column or row reserves whole lines", {
  p <- suppressWarnings(place_controls(96, "every", every = 4, by = "column"))
  expect_setequal(p$Column[p$Role == "control"], c(1, 5, 9))
  expect_equal(sum(p$Role == "control"), 3 * 8)
  p <- suppressWarnings(place_controls(96, "every", every = 3, by = "row", offset = 2))
  expect_setequal(p$Row[p$Role == "control"], c(2, 5, 8))
})

test_that("random is reproducible from the seed", {
  a <- place_controls(96, "random", n = 8, seed = 3)
  b <- place_controls(96, "random", n = 8, seed = 3)
  d <- place_controls(96, "random", n = 8, seed = 4)
  expect_equal(a$Well[a$Role == "control"], b$Well[b$Role == "control"])
  expect_false(identical(a$Well[a$Role == "control"], d$Well[d$Role == "control"]))
  expect_equal(sum(a$Role == "control"), 8)
  expect_equal(attr(a, "seed"), 3)
})

test_that("calls chain, names are kept, and bad input errors", {
  p <- place_controls(96, wells = "A01", name = "blank") |>
    place_controls("random", n = 2, seed = 1, name = "pos")
  expect_equal(sum(p$Role == "control"), 3)
  expect_equal(sum(p$Control == "pos", na.rm = TRUE), 2)
  expect_equal(p$Control[p$Well == "A01"], "blank")
  expect_error(place_controls(96, "fixed"), "wells")
  expect_error(suppressWarnings(place_controls(96, "every")), "every")
  expect_error(place_controls(96, "random"), "`n`")
  expect_error(place_controls(96, wells = "Z99"), "invalid")
  expect_error(place_controls(100, wells = "A01"), "Unsupported")
})

test_that("plate_roles marks reaction wells and refuses control wells", {
  p <- place_controls(96, wells = c("A01", "H12"))
  r <- plate_roles(p, c("A02", "A03"))
  expect_equal(as.vector(roles(r)), c(2L, 2L, 92L))
  r2 <- plate_roles(p, data.frame(Well.Position = "B01"))
  expect_equal(sum(r2$Role == "reaction"), 1)
  expect_error(plate_roles(p, "A01"), "reserved")
  expect_error(plate_roles(data.frame(), "A01"), "place_controls")
})

test_that("placed reactions and controls together fill roles consistently", {
  ctl <- place_controls(96, wells = c("A01", "A12", "H01", "H12"))
  out <- replicate_reactions(data.frame(rxn = 1:20), "rxn", 3, pattern = "scattered",
                             seed = 2, reserved = ctl)
  out$Well <- convert_well(out$Well.Index, "to_well", plate = 96)
  p <- plate_roles(ctl, out, "Well")
  expect_equal(sum(p$Role == "reaction"), 60)
  expect_equal(sum(p$Role == "control"), 4)
})

ctl_wells <- function(p) p$Well[p$Role == "control"]

test_that("fixed accepts ranges mixed with single wells and A1 style", {
  p <- place_controls(96, "fixed", wells = c("A01:H01", "a12", "B3"))
  expect_equal(sum(p$Role == "control"), 8 + 2)
  expect_true(all(sprintf("%s01", LETTERS[1:8]) %in% ctl_wells(p)))
  expect_true(all(c("A12", "B03") %in% ctl_wells(p)))
  p <- place_controls(96, "fixed", wells = "A01:B12")
  expect_equal(sum(p$Role == "control"), 24)
  expect_setequal(p$Row[p$Role == "control"], 1:2)
  # corners may come in any order
  expect_equal(ctl_wells(place_controls(96, wells = "B2:A1")),
               ctl_wells(place_controls(96, wells = "A1:B2")))
  expect_equal(ctl_wells(place_controls(96, wells = "A1:B2")), c("A01", "A02", "B01", "B02"))
  expect_error(place_controls(96, wells = "A01:B02:C03"), "range")
  expect_error(place_controls(96, wells = "A01:"), "range")
  expect_error(place_controls(96, wells = "A01:Z99"), "invalid")
})

test_that("lines reserves whole columns or rows", {
  p <- place_controls(384, "lines", lines = c(1, 24), by = "column")
  expect_equal(sum(p$Role == "control"), 32)
  expect_setequal(p$Column[p$Role == "control"], c(1, 24))
  p <- place_controls(96, "lines", lines = c("A", "H"), by = "row")
  expect_equal(sum(p$Role == "control"), 24)
  expect_setequal(p$Row[p$Role == "control"], c(1, 8))
  p2 <- place_controls(96, "lines", lines = c(1, 8), by = "row")
  expect_equal(ctl_wells(p), ctl_wells(p2))
  p3 <- place_controls(96, "lines", lines = c("a", "h"), by = "row")
  expect_equal(ctl_wells(p), ctl_wells(p3))
  expect_equal(sum(place_controls(96, "lines", lines = 3)$Role == "control"), 8)
  expect_error(place_controls(96, "lines"), "lines")
  expect_error(place_controls(96, "lines", lines = 13), "between")
  expect_error(place_controls(96, "lines", lines = "I", by = "row"), "between")
  expect_error(place_controls(96, "lines", lines = "A", by = "column"), "between")
})

test_that("edge depth reserves several rings", {
  p1 <- place_controls(96, "edge", depth = 1)
  expect_equal(sum(p1$Role == "control"), 36)
  p2 <- place_controls(96, "edge", depth = 2)
  # 96 = 8 x 12: inner 4 x 8 = 32 stay free
  expect_equal(sum(p2$Role == "control"), 96 - 4 * 8)
  expect_false(any(p2$Role[p2$Row %in% 3:6 & p2$Column %in% 3:10] == "control"))
  p384 <- place_controls(384, "edge", depth = 2)
  expect_equal(sum(p384$Role == "control"), 384 - 12 * 20)
  expect_equal(place_controls(96, "edge")$Role, p1$Role)
  expect_error(place_controls(96, "edge", depth = 0), "depth")
})

test_that("spread places control k after the floor(k * among / n)-th reaction", {
  p <- place_controls(96, "spread", n = 8, among = 40)
  # block of 48 wells from A01; a control after every 5th reaction
  expect_equal(which(p$Role == "control"), seq(6, 48, by = 6))
  # picture: R R R R R C  x 8
  pic <- paste(ifelse(p$Role[1:48] == "control", "C", "R"), collapse = "")
  expect_equal(pic, strrep("RRRRRC", 8))
  # uneven split is deterministic: 3 controls among 7
  q <- place_controls(96, "spread", n = 3, among = 7)
  expect_equal(paste(ifelse(q$Role[1:10] == "control", "C", "R"), collapse = ""),
               "RRCRRCRRRC")
  expect_equal(which(q$Role == "control"), c(3, 6, 10))
  # more controls than reactions: neighbouring controls
  r <- place_controls(96, "spread", n = 4, among = 2)
  expect_equal(which(r$Role == "control"), c(1, 3, 4, 6))
})

test_that("spread respects start, fill direction and wells already reserved", {
  p <- place_controls(96, "spread", n = 2, among = 4, start = "B01")
  # block of 6 from B01 (position 13): controls at block positions 3 and 6
  expect_equal(ctl_wells(p), c("B03", "B06"))
  p <- place_controls(96, "spread", n = 2, among = 4, start = 13)
  expect_equal(ctl_wells(p), c("B03", "B06"))
  # column-wise fill runs down the columns
  p <- place_controls(96, "spread", n = 2, among = 4, wise = "col")
  expect_equal(ctl_wells(p), c("C01", "F01"))
  # already-reserved wells are not counted as free positions
  base <- place_controls(96, wells = "A02")
  p <- place_controls(base, "spread", n = 2, among = 4)
  expect_equal(ctl_wells(p), c("A02", "A04", "A07"))
  expect_equal(sum(p$Control == "control", na.rm = TRUE), 3)
  expect_warning(place_controls(96, "spread", n = 2, among = 4), NA)
})

test_that("spread without among spreads over the whole plate", {
  p <- place_controls(96, "spread", n = 8)
  expect_equal(which(p$Role == "control"), seq(12, 96, by = 12))
  p <- place_controls(96, "spread", n = 4, start = "E01")
  expect_equal(which(p$Role == "control"), 48 + seq(12, 48, by = 12))
  expect_error(place_controls(96, "spread", n = 8, among = 90), "Cannot spread")
  expect_error(place_controls(96, "spread", among = 4), "`n`")
  expect_error(place_controls(96, "spread", n = 2, among = -1), "among")
})

test_that("quadrant reserves a whole quadrant, several allowed", {
  p <- place_controls(384, "quadrant", quadrant = 4)
  expect_equal(sum(p$Role == "control"), 96)
  expect_true(all(p$Row[p$Role == "control"] %% 2 == 0))
  expect_true(all(p$Column[p$Role == "control"] %% 2 == 0))
  expect_equal(unique(well_quadrant(ctl_wells(p), 384)), 4L)
  p <- place_controls(96, "quadrant", quadrant = 1)
  expect_equal(sum(p$Role == "control"), 24)
  expect_equal(ctl_wells(p)[1:3], c("A01", "A03", "A05"))
  p <- place_controls(96, "quadrant", quadrant = c(1, 4))
  expect_equal(sum(p$Role == "control"), 48)
  expect_setequal(well_quadrant(ctl_wells(p), 96), c(1L, 4L))
  expect_error(place_controls(96, "quadrant"), "quadrant")
  expect_error(place_controls(96, "quadrant", quadrant = 5), "quadrant")
  expect_error(place_controls(12, "quadrant", quadrant = 1), "even")
})

test_that("every still works but is deprecated", {
  rlang::reset_warning_verbosity("mtpR_place_controls_every")
  expect_warning(
    p <- place_controls(96, "every", every = 4, by = "column"),
    "deprecated"
  )
  expect_setequal(p$Column[p$Role == "control"], c(1, 5, 9))
  expect_warning(
    place_controls(96, "every", every = 4, by = "column"), NA
  )  # once per session
  rlang::reset_warning_verbosity("mtpR_place_controls_every")
  expect_warning(place_controls(96, "every", every = 2), "lines.*spread|spread")
  rlang::reset_warning_verbosity("mtpR_place_controls_every")
  suppressWarnings(place_controls(96, "every", every = 2))
})

test_that("a later call that takes a reserved well warns and wins", {
  p <- place_controls(96, wells = c("A01", "A02"), name = "blank")
  expect_warning(
    q <- place_controls(p, "lines", lines = 1, name = "pos"),
    "already reserved.*A01"
  )
  expect_equal(q$Control[q$Well == "A01"], "pos")
  expect_equal(q$Control[q$Well == "A02"], "blank")
  expect_warning(place_controls(96, wells = "A01") |> place_controls(wells = "B01"), NA)
})

test_that("well_quadrant numbers the wells of each 2x2 square", {
  expect_equal(well_quadrant(c("A01", "A02", "B01", "B02", "A03", "C04"), 96),
               c(1L, 2L, 3L, 4L, 1L, 2L))
  expect_equal(well_quadrant(c("A1", "p24"), 384), c(1L, 4L))
  expect_error(well_quadrant("A01", 12), "even")
})
