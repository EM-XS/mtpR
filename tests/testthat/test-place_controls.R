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
  p <- place_controls(96, "every", every = 4, by = "column")
  expect_setequal(p$Column[p$Role == "control"], c(1, 5, 9))
  expect_equal(sum(p$Role == "control"), 3 * 8)
  p <- place_controls(96, "every", every = 3, by = "row", offset = 2)
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
  expect_error(place_controls(96, "every"), "every")
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
  out$Well <- convert_well(out$position, "to_well", plate = 96)
  p <- plate_roles(ctl, out, "Well")
  expect_equal(sum(p$Role == "reaction"), 60)
  expect_equal(sum(p$Role == "control"), 4)
})
