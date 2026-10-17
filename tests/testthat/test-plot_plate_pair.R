picklist <- function() {
  data.frame(
    `Source Plate Name` = "Src1",
    `Source Plate Type` = "384PP_AQ_BP",
    `Source Well` = rep(c("A1", "B1", "C01"), c(3, 2, 2)),
    `Destination Plate Name` = "Assay",
    `Destination Well` = c("A1", "A2", "A3", "A1", "B1", "B2", "B3"),
    `Transfer Volume` = c(25, 50, 25, 100, 25, 50, 75),
    check.names = FALSE
  )
}
big_picklist <- function(n) {
  data.frame(`Source Well` = rep("A1", n),
             `Destination Well` = convert_well(rep(1:384, length.out = n), "to_well", plate = 384),
             `Transfer Volume` = 25, check.names = FALSE)
}
html_of <- function(g) as.character(htmltools::renderTags(g)$html)

test_that("picklist_summary counts a hand-made picklist", {
  s <- picklist_summary(picklist())
  expect_equal(s$transfers, 7)
  expect_equal(s$source_wells, 3)
  expect_equal(s$dest_wells, 6)
  expect_equal(s$total_volume, 350)
  expect_equal(s$by_source_plate$source_plate, "Src1")
  expect_equal(s$by_source_plate$volume, 350)
})

test_that("picklist_summary works without a volume or plate columns", {
  pl <- picklist()[, c("Source Well", "Destination Well")]
  s <- picklist_summary(pl)
  expect_equal(s$transfers, 7)
  expect_true(is.na(s$total_volume))
  expect_equal(s$by_source_plate$source_plate, "(all)")
  expect_error(picklist_summary(picklist(), volume = "nope"), "not found")
})

test_that("picklist_summary splits volume by source plate", {
  pl <- picklist()
  pl$`Source Plate Name`[1:3] <- "Src2"
  s <- picklist_summary(pl)
  expect_equal(s$by_source_plate$source_plate, c("Src2", "Src1"))
  expect_equal(s$by_source_plate$volume, c(100, 250))
  expect_equal(s$source_wells, 3)
})

test_that("static pair is a ggplot with a summary attribute", {
  p <- plot_plate_pair(picklist(), source_plate = 384, dest_plate = 96)
  expect_s3_class(p, "ggplot")
  expect_equal(attr(p, "summary")$transfers, 7)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("A1 and A01 are the same well", {
  pl <- picklist()
  pl$`Source Well`[1] <- "A01"
  expect_equal(attr(plot_plate_pair(pl), "summary")$source_wells, 3)
  # but the plot shows 3 used source tiles
  p <- plot_plate_pair(pl)
  n <- vapply(p$layers, function(l) nrow(l$data), 1L)
  expect_true(3 %in% n)
})

test_that("interactive pair builds a girafe with linked data_ids", {
  skip_if_not_installed("ggiraph"); skip_if_not_installed("htmltools"); skip_if_not_installed("htmlwidgets")
  g <- plot_plate_pair(picklist(), source_plate = 384, dest_plate = 96,
                       interactive = TRUE)
  expect_s3_class(g, "girafe")
  expect_equal(attr(g, "summary")$transfers, 7)
  html <- html_of(g)
  expect_match(html, "data-id")
  expect_match(html, "s:Src1:A1", fixed = TRUE)
  expect_match(html, "d:Assay:B3", fixed = TRUE)
  expect_match(html, "l:Src1:A1>Assay:A1", fixed = TRUE)
  expect_match(html, "3 transfers, 100 nL", fixed = TRUE)
  rel <- g$jsHooks$render[[1]]$data
  expect_setequal(unlist(rel[["s:Src1:A1"]]),
                  c("d:Assay:A1", "d:Assay:A2", "d:Assay:A3", "l:Src1:A1>Assay:A1",
                    "l:Src1:A1>Assay:A2", "l:Src1:A1>Assay:A3"))
  expect_setequal(unlist(rel[["d:Assay:A1"]]),
                  c("s:Src1:A1", "s:Src1:B1", "l:Src1:A1>Assay:A1", "l:Src1:B1>Assay:A1"))
})

test_that("links modes", {
  skip_if_not_installed("ggiraph"); skip_if_not_installed("htmltools"); skip_if_not_installed("htmlwidgets")
  link_layers <- function(p) sum(vapply(p$layers, function(l) inherits(l$geom, "GeomCurve") ||
                                          inherits(l$geom, "GeomSegment"), TRUE))
  pl <- picklist()
  expect_equal(link_layers(plot_plate_pair(pl)), 1)
  expect_equal(link_layers(plot_plate_pair(pl, links = "all")), 1)
  expect_equal(link_layers(plot_plate_pair(pl, links = "none")), 0)
  p0 <- plot_plate_pair(pl, links = "all", link_curvature = 0)
  expect_true(any(vapply(p0$layers, function(l) inherits(l$geom, "GeomSegment"), TRUE)))
  # interactive: links exist but start invisible in "hover", none with "none"
  expect_match(html_of(plot_plate_pair(pl, interactive = TRUE)), "l:Src1:A1>Assay:A1", fixed = TRUE)
  expect_false(grepl("data-id=.l:", html_of(plot_plate_pair(pl, links = "none", interactive = TRUE))))
  expect_true(grepl("data-id=.l:", html_of(plot_plate_pair(pl, interactive = TRUE))))
  expect_error(plot_plate_pair(pl, links = "sometimes"))
})

test_that("more than 500 transfers draw no links by default and say so", {
  pl <- big_picklist(600)
  link_layers <- function(p) sum(vapply(p$layers, function(l) inherits(l$geom, "GeomCurve"), TRUE))
  p <- plot_plate_pair(pl)
  expect_equal(link_layers(p), 0)
  expect_match(p$labels$caption, "Links not drawn: more than 500 transfers")
  expect_equal(link_layers(plot_plate_pair(pl, links = "all")), 1)
  expect_equal(link_layers(plot_plate_pair(big_picklist(500))), 1)
})

test_that("fill options and plate-name selection", {
  pl <- picklist()
  pl$type <- c("a", "a", "b", "b", "b", "c", "c")
  expect_no_error(ggplot2::ggplot_build(
    plot_plate_pair(pl, source_fill = "count", dest_fill = "type")))
  expect_error(plot_plate_pair(pl, dest_fill = "nope"), "dest_fill")
  pl2 <- rbind(pl, transform(pl, `Source Plate Name` = "Src2"))
  # all plates are drawn by default; the id is only a filter
  expect_equal(attr(plot_plate_pair(pl2), "summary")$transfers, 14)
  expect_equal(attr(plot_plate_pair(pl2, source_plate_id = "Src2"), "summary")$transfers, 7)
  expect_error(plot_plate_pair(pl2, source_plate_id = "zzz"), "No source plate")
})

test_that("bad input errors clearly", {
  expect_error(plot_plate_pair(picklist(), source_well = "nope"), "source_well")
  bad <- picklist(); bad$`Source Well`[1] <- "J1"
  expect_error(plot_plate_pair(bad, source_plate = 96), "does not fit a 96")
  expect_error(plot_plate_pair(picklist(), dest_plate = 7), "Unsupported")
  expect_error(plot_plate_pair("x"), "data frame")
})

# --- several plates per side ---------------------------------------------------

multi_picklist <- function() {
  data.frame(
    `Source Plate Name` = rep(c("Res", "Src"), c(3, 4)),
    `Source Plate Type` = rep(c("6RES_AQ_GP3", "384PP_AQ_BP"), c(3, 4)),
    `Source Well` = c("A1", "A1", "B2", "A1", "A1", "B2", "P24"),
    `Destination Plate Name` = "Assay",
    `Destination Well` = c("A1", "A2", "A3", "B1", "B2", "B3", "P24"),
    `Transfer Volume` = c(100, 100, 50, 25, 25, 50, 75),
    check.names = FALSE
  )
}
tile_boxes <- function(p) {
  # bounding boxes (xmin, xmax, ymin, ymax) of the plate-shaped empty-well layers
  do.call(rbind, lapply(p$layers[seq_len(3)], function(l) {
    d <- l$data
    data.frame(xmin = min(d$x) - .5, xmax = max(d$x) + .5, ymin = min(d$y) - .5, ymax = max(d$y) + .5)
  }))
}

test_that("two source plates of different types share one picture without overlap", {
  p <- plot_plate_pair(multi_picklist())
  expect_s3_class(p, "ggplot")
  b <- tile_boxes(p)   # Res (6), Src (384), Assay (384)
  expect_equal(c(b$xmax[1] - b$xmin[1], b$ymax[1] - b$ymin[1]), c(3, 2))      # 6RES
  expect_equal(c(b$xmax[2] - b$xmin[2], b$ymax[2] - b$ymin[2]), c(24, 16))    # 384PP
  overlap <- function(i, j) b$xmin[i] < b$xmax[j] && b$xmin[j] < b$xmax[i] &&
    b$ymin[i] < b$ymax[j] && b$ymin[j] < b$ymax[i]
  expect_false(overlap(1, 2))
  expect_false(overlap(1, 3))
  expect_false(overlap(2, 3))
  expect_gt(b$xmin[3], b$xmax[2])   # destination is to the right of all sources
  s <- attr(p, "summary")
  expect_equal(s$by_source_plate$source_plate, c("Res", "Src"))
  expect_equal(s$by_dest_plate$dest_plate, "Assay")
  expect_equal(s$by_dest_plate$volume, 425)
})

test_that("explicit and named plate sizes work", {
  p <- plot_plate_pair(multi_picklist(), source_plate = c(Res = 6, Src = 384), dest_plate = 384)
  expect_equal(max(p$layers[[1]]$data$x), 3)
  expect_error(plot_plate_pair(multi_picklist(), source_plate = c(Res = 6)), "no size for plate 'Src'")
  expect_equal(max(plot_plate_pair(multi_picklist(), source_plate = 384)$layers[[1]]$data$x), 24)
})

test_that("plate_id filters plates", {
  p <- plot_plate_pair(multi_picklist(), source_plate_id = "Src")
  expect_equal(attr(p, "summary")$transfers, 4)
})

test_that("the same well on two plates gets distinct interactive ids", {
  skip_if_not_installed("ggiraph"); skip_if_not_installed("htmltools"); skip_if_not_installed("htmlwidgets")
  g <- plot_plate_pair(multi_picklist(), interactive = TRUE)
  html <- html_of(g)
  expect_match(html, "s:Res:A1", fixed = TRUE)
  expect_match(html, "s:Src:A1", fixed = TRUE)
  rel <- g$jsHooks$render[[1]]$data
  expect_setequal(unlist(rel[["s:Res:A1"]]),
                  c("d:Assay:A1", "d:Assay:A2", "l:Res:A1>Assay:A1", "l:Res:A1>Assay:A2"))
  expect_setequal(unlist(rel[["s:Src:A1"]]),
                  c("d:Assay:B1", "d:Assay:B2", "l:Src:A1>Assay:B1", "l:Src:A1>Assay:B2"))
  expect_setequal(unlist(rel[["d:Assay:A1"]]), c("s:Res:A1", "l:Res:A1>Assay:A1"))
})

test_that("picklist_summary has by_dest_plate", {
  s <- picklist_summary(multi_picklist())
  expect_equal(s$by_dest_plate$transfers, 7)
  expect_equal(s$by_dest_plate$dest_wells, 7)
  expect_equal(s$by_source_plate$volume, c(250, 175))
})
