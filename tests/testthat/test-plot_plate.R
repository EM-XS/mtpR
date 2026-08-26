test_that("plot_plate returns a ggplot for a simple layout", {
  d <- data.frame(
    well = convert_well(1:12, "to_well", plate = 96, wise = "row"),
    grp = rep(c("x", "y"), 6)
  )
  p <- plot_plate(d, fill = "grp", well_id = "well", plate = 96)
  expect_s3_class(p, "ggplot")
})

test_that("faceting arguments are accepted", {
  d <- data.frame(
    well = convert_well(1:8, "to_well", plate = 96, wise = "row"),
    grp = "a",
    block = rep(c("p1", "p2"), 4)
  )
  p <- plot_plate(d, fill = "grp", well_id = "well",
                  facet_cols = "block", plate = 96)
  expect_s3_class(p, "ggplot")
})

test_that("a missing fill column errors", {
  d <- data.frame(well = "A01")
  expect_error(plot_plate(d, fill = "nope", well_id = "well", plate = 96), "not found")
})
