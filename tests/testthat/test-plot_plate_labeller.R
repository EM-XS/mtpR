facet_labeller_data <- function() {
  data.frame(
    well = rep(convert_well(1:4, "to_well", plate = 96, wise = "row"), 4),
    grp = "a",
    copy = rep(1:2, each = 8),
    pl = rep(rep(1:2, each = 4), 2)
  )
}

test_that("labeller defaults to label_both for the facet strips", {
  p <- plot_plate(facet_labeller_data(), fill = "grp", well_id = "well",
                  facet_rows = "pl", facet_cols = "copy", plate = 96)
  expect_identical(p$facet$params$labeller, ggplot2::label_both)
  lay <- ggplot2::ggplot_build(p)$layout$layout
  out <- p$facet$params$labeller(lay[, "copy", drop = FALSE])
  expect_equal(out[[1]], c("copy: 1", "copy: 2", "copy: 1", "copy: 2"))
})

test_that("labeller reaches every facet branch", {
  lab <- ggplot2::labeller(
    copy = function(x) paste("Copy", x),
    pl = function(x) paste("Plate", x)
  )
  d <- facet_labeller_data()
  both <- plot_plate(d, fill = "grp", well_id = "well", facet_rows = "pl",
                     facet_cols = "copy", plate = 96, labeller = lab)
  rows <- plot_plate(d, fill = "grp", well_id = "well", facet_rows = "pl",
                     plate = 96, labeller = lab)
  cols <- plot_plate(d, fill = "grp", well_id = "well", facet_cols = "copy",
                     plate = 96, labeller = lab)
  for (p in list(both, rows, cols)) {
    expect_identical(p$facet$params$labeller, lab)
  }
  lay <- ggplot2::ggplot_build(both)$layout$layout
  expect_equal(unique(lab(lay[, "copy", drop = FALSE])[[1]]), c("Copy 1", "Copy 2"))
  expect_equal(unique(lab(lay[, "pl", drop = FALSE])[[1]]), c("Plate 1", "Plate 2"))
})
