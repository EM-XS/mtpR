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

# --- tiles ---------------------------------------------------------------

plate96 <- function(n = 36) {
  data.frame(
    well = convert_well(seq_len(n), "to_well", plate = 96, wise = "row"),
    grp = rep(c("x", "y", "z"), length.out = n),
    val = seq_len(n) / 2,
    stringsAsFactors = FALSE
  )
}
layer_classes <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")

test_that("tiles are the default and points remain available", {
  p <- plot_plate(plate96(), "grp", "well", plate = 96)
  expect_true("GeomTile" %in% layer_classes(p))
  expect_false("GeomPoint" %in% layer_classes(p))
  pp <- plot_plate(plate96(), "grp", "well", plate = 96, style = "points")
  expect_true("GeomPoint" %in% layer_classes(pp))
  expect_s3_class(ggplot2::ggplotGrob(pp), "gtable")
})

test_that("tiles draw the whole plate and render", {
  p <- plot_plate(plate96(), "grp", "well", plate = 96)
  expect_equal(nrow(p$layers[[1]]$data), 96)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  expect_equal(p$coordinates$ratio, 1)
})

test_that("legend = 'auto' hides the legend above 12 keys", {
  few <- plot_plate(plate96(), "grp", "well", plate = 96)
  expect_equal(few$theme$legend.position, "right")
  many <- plot_plate(plate96(), "well", "well", plate = 96)
  expect_equal(many$theme$legend.position, "none")
  expect_equal(plot_plate(plate96(), "well", "well", plate = 96, legend = TRUE)$theme$legend.position, "right")
  expect_equal(plot_plate(plate96(), "grp", "well", plate = 96, legend = FALSE)$theme$legend.position, "none")
  expect_error(plot_plate(plate96(), "grp", "well", plate = 96, legend = "maybe"), "legend")
})

test_that("numeric fill gets a continuous scale unless scale = 'discrete'", {
  p <- plot_plate(plate96(), "val", "well", plate = 96)
  expect_true(p$scales$get_scales("fill")$is_discrete() == FALSE)
  q <- plot_plate(plate96(), "val", "well", plate = 96, scale = "discrete")
  expect_true(is.factor(q$data$fill))
  expect_true(is.numeric(p$data$fill))
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("labels and highlights add layers", {
  base <- plot_plate(plate96(), "grp", "well", plate = 96)
  lab <- plot_plate(plate96(), "grp", "well", plate = 96, label = "val")
  expect_true("GeomText" %in% layer_classes(lab))
  expect_false("GeomText" %in% layer_classes(base))
  hl <- plot_plate(plate96(), "grp", "well", plate = 96, highlight = c("A1", "b02"))
  expect_equal(length(hl$layers), length(base$layers) + 1)
  expect_equal(nrow(hl$layers[[length(hl$layers)]]$data), 2)
  expect_s3_class(ggplot2::ggplotGrob(hl), "gtable")
  expect_error(plot_plate(plate96(), "grp", "well", plate = 96, label = "nope"), "not found")
})

test_that("interactive = TRUE returns a girafe object with data_id", {
  skip_if_not_installed("ggiraph")
  g <- plot_plate(plate96(), "grp", "well", plate = 96, interactive = TRUE)
  expect_s3_class(g, "girafe")
  expect_true(any(grepl("data-id", g$x$html, fixed = TRUE)) ||
                any(grepl("A01", g$x$html, fixed = TRUE)))
})

test_that("interactive = TRUE without ggiraph gives an informative error", {
  skip_if(requireNamespace("ggiraph", quietly = TRUE), "ggiraph is installed")
  expect_error(plot_plate(plate96(), "grp", "well", plate = 96, interactive = TRUE),
               "ggiraph")
})
