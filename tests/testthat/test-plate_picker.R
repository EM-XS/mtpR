test_that("plate_picker() validates its arguments", {
  expect_error(plate_picker(100), "Unsupported plate size")
  expect_error(plate_picker(96, labels = c("a", "a")), "unique")
  expect_error(plate_picker(96, labels = ""), "non-empty")
  expect_error(plate_picker(96, labels = 1:2), "character")
  expect_error(plate_picker(96, c("a", "b"), active = "z"), "active")
  expect_error(plate_picker(96, multiple = NA), "multiple")
  expect_error(plate_picker(96, toolbar = "yes"), "toolbar")
  expect_error(plate_picker(96, theme = "blue"))
  expect_error(plate_picker(96, locked = "Z99"), "not on the plate")
  expect_error(plate_picker(96, c("a", "b"), colours = c(a = "red")), "colour for every label")
  expect_error(plate_picker(96, c("a", "b"), colours = c(a = "red", b = "notacolour")), "valid colour")
})

test_that("the palette goes on past eight labels", {
  many <- paste0("L", 1:20)
  p <- plate_picker(96, many)
  expect_length(unique(as.character(p$x$colours)), 20)
  expect_equal(as.character(p$x$colours)[1:8], .picker_palette)
  cols <- stats::setNames(rep(c("red", "blue", "green", "black"), 5), many)
  expect_equal(as.character(plate_picker(96, many, colours = cols)$x$colours)[1], "#FF0000")
})

test_that("labels can be a data frame with groups, colours and attributes", {
  d <- data.frame(label = c("S1", "S2", "Blank"), group = c("Std", "Std", NA),
                  colour = c(NA, "red", NA), conc_uM = c(100, 50, NA), lot = c("a", "b", NA))
  p <- plate_picker(96, d, fill = "conc_uM")
  expect_equal(as.character(p$x$labels), d$label)
  expect_equal(as.character(p$x$groups), d$group)
  expect_equal(as.character(p$x$colours)[2], "#FF0000")
  expect_equal(names(p$x$attributes), c("conc_uM", "lot"))
  expect_equal(p$x$fill, "conc_uM")
  json <- as.character(jsonlite::toJSON(p$x$attributes$conc_uM, na = "null"))
  expect_equal(json, "[100,50,null]")

  expect_error(plate_picker(96, data.frame(name = "x")), "`label` column")
  expect_error(plate_picker(96, data.frame(label = c("a", "a"))), "unique")
  expect_error(plate_picker(96, d, fill = "nope"), "`fill` must be one of: label, group, conc_uM, lot")
  expect_error(plate_picker(96, "a", fill = "group"), "`fill`")
  expect_error(plate_picker(96, editable = NA), "editable")
})

test_that("a value data frame can bring its own labels", {
  v <- data.frame(well = c("A1", "A2", "B1"), label = c("x", "x", "y"), group = "g", conc = c(1, 1, 2))
  p <- plate_picker(96, value = v)
  expect_equal(as.character(p$x$labels), c("x", "y"))
  expect_equal(as.character(p$x$attributes$conc), c("1", "2"))
  expect_equal(as.character(p$x$value$well), c("A01", "A02", "B01"))
  v$conc[2] <- 9
  expect_error(plate_picker(96, value = v), "belong to a label")
  # a saved plate with nothing painted
  empty <- plate_picker(96, value = data.frame(well = character(), label = character()))
  expect_equal(as.character(empty$x$labels), "selected")
})

test_that("locked accepts a plate from place_controls()", {
  controls <- place_controls(96, wells = c("A01", "H12"))
  p <- plate_picker(96, "x", locked = controls)
  expect_equal(as.character(p$x$locked), c("A01", "H12"))
  expect_error(plate_picker(384, "x", locked = controls), "does not match")
})

test_that("values are normalised and checked against the plate", {
  p <- plate_picker(96, c("Blank", "Standard"),
                    value = c(a1 = "Blank", B12 = "Standard"))
  expect_equal(as.character(p$x$value$well), c("A01", "B12"))
  expect_equal(as.character(p$x$value$label), c("Blank", "Standard"))

  d <- data.frame(well = c("H1", "A2"), label = c("Standard", "Blank"))
  p <- plate_picker(96, c("Blank", "Standard"), value = d)
  expect_equal(as.character(p$x$value$well), c("A02", "H01"))   # plate order

  expect_error(plate_picker(96, "Blank", value = c(A13 = "Blank")), "not on the plate")
  expect_error(plate_picker(96, "Blank", value = c(A01 = "Other")), "not in `labels`")
  expect_error(plate_picker(96, "Blank", value = list(1)), "`value` must be")
  expect_error(plate_picker(96, c("a", "b"), value = c(A01 = "a", A01 = "b")), "multiple = TRUE")
  expect_silent(plate_picker(96, c("a", "b"), value = c(A01 = "a", A01 = "b"), multiple = TRUE))
  # 1536 wells use two-letter rows
  p <- plate_picker(1536, value = "af48")
  expect_equal(as.character(p$x$value$well), "AF48")
  expect_equal(as.character(p$x$value$label), "selected")
})

test_that("the widget payload carries the plate, labels and colours", {
  p <- plate_picker(384, c("Blank", "Standard"), colours = c(Blank = "red", Standard = "#00ff00"),
                    locked = "p24", active = "Standard", theme = "dark", multiple = TRUE)
  x <- p$x
  expect_equal(c(x$nrow, x$ncol, x$plate), c(16, 24, 384))
  expect_equal(as.character(x$rowLabels), LETTERS[1:16])
  expect_equal(as.character(x$colours), c("#FF0000", "#00FF00"))
  expect_equal(as.character(x$locked), "P24")
  expect_equal(x$active, "Standard")
  expect_equal(x$theme, "dark")
  expect_true(x$multiple)
  expect_equal(x$exportButtons, "auto")
  # one-element vectors stay arrays in the JSON
  json <- as.character(jsonlite::toJSON(x$locked, auto_unbox = TRUE))
  expect_equal(json, "[\"P24\"]")

  p0 <- plate_picker()
  expect_equal(as.character(p0$x$labels), "selected")
  expect_equal(length(p0$x$colours), 1)
})

test_that("background is validated and normalised", {
  bg <- data.frame(well = c("a1", "A2"), fill = c("red", "#112233"),
                   text = c("x", "y"), tooltip = c("one", "two"))
  p <- plate_picker(96, background = bg)
  expect_equal(as.character(p$x$background$well), c("A01", "A02"))
  expect_equal(as.character(p$x$background$fill), c("#FF0000", "#112233"))
  expect_equal(as.character(p$x$background$tooltip), c("one", "two"))
  expect_error(plate_picker(96, background = data.frame(well = "A01")), "`fill`")
  expect_error(plate_picker(96, background = data.frame(well = "A99", fill = "red")), "not on the plate")
  expect_error(plate_picker(96, background = data.frame(well = c("A01", "A1"), fill = "red")), "more than once")
})

test_that("plate_picker_value() builds a sorted data frame from the Shiny input", {
  x <- list(well = list("B02", "A10", "A01"), label = list("b", "a", "a"))
  d <- plate_picker_value(x)
  expect_equal(d$well, c("A01", "A10", "B02"))
  expect_equal(d$label, c("a", "a", "b"))
  expect_equal(plate_picker_value(NULL), data.frame(well = character(), label = character()))
  expect_equal(nrow(plate_picker_value(list(well = list(), label = list()))), 0)
  # long rows sort after short ones, and wells are padded
  d <- plate_picker_value(list(well = c("AA01", "z1", "A1"), label = c("x", "y", "z")))
  expect_equal(d$well, c("A01", "Z01", "AA01"))
  expect_error(plate_picker_value(list(a = 1)), "`well` and `label`")
  expect_error(plate_picker_value(list(well = "A01", label = c("a", "b"))), "same length")
})

test_that("plate_picker_html() writes a self-contained page with the state block", {
  f <- tempfile(fileext = ".html")
  p <- plate_picker(96, c("Blank", "Standard"), value = c(A01 = "Blank", B02 = "Standard"))
  expect_equal(plate_picker_html(p, f, title = "My <run>"), f)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("<script[^>]*\\ssrc\\s*=", txt))
  expect_false(grepl("<link[^>]*href\\s*=", txt))
  expect_true(grepl("id=\"mtpr-picker-state\"", txt, fixed = TRUE))
  expect_true(grepl("HTMLWidgets.widget", txt, fixed = TRUE))
  expect_true(grepl("mtpr-picker", txt, fixed = TRUE))
  expect_true(grepl("<title>My &lt;run&gt;</title>", txt, fixed = TRUE))
  expect_error(plate_picker_html("x", f), "plate_picker")
  expect_error(plate_picker_html(p, f, title = 1), "title")
})

test_that("read_plate_picker() reads saved pages and exported CSV files", {
  f <- tempfile(fileext = ".html")
  v <- data.frame(well = c("B02", "A01", "A01"), label = c("Standard", "Blank", "Standard"))
  plate_picker_html(plate_picker(96, c("Blank", "Standard"), value = v, multiple = TRUE), f)
  d <- read_plate_picker(f)
  expect_equal(d$well, c("A01", "A01", "B02"))
  expect_equal(d$label, c("Blank", "Standard", "Standard"))

  csv <- tempfile(fileext = ".csv")
  writeLines(c("well,label", "B2,\"a, b\"", "A01,Blank"), csv)
  d <- read_plate_picker(csv)
  expect_equal(d$well, c("A01", "B02"))
  expect_equal(d$label, c("Blank", "a, b"))

  empty <- tempfile(fileext = ".csv")
  writeLines("well,label", empty)
  expect_equal(nrow(read_plate_picker(empty)), 0)

  bad <- tempfile(fileext = ".html")
  writeLines("<html><body>nothing</body></html>", bad)
  expect_error(read_plate_picker(bad), "No plate picker state")
  expect_error(read_plate_picker(tempfile()), "existing file")
  csv2 <- tempfile(fileext = ".csv"); writeLines(c("a,b", "1,2"), csv2)
  expect_error(read_plate_picker(csv2), "`well` and `label`")
})

test_that("groups and attributes come back from Shiny, saved pages and CSV files", {
  x <- list(well = list("B02", "A01"), label = list("b", "a"), group = list(NULL, "g"),
            attributes = list(conc = list(2, NULL), lot = list("x", "y")))
  d <- plate_picker_value(x)
  expect_equal(d$well, c("A01", "B02"))
  expect_equal(d$group, c("g", NA))
  expect_equal(d$conc, c(NA, 2))
  expect_equal(d$lot, c("y", "x"))

  v <- data.frame(label = c("S1", "S2"), group = "Std", conc = c(10, 5))
  f <- tempfile(fileext = ".html")
  plate_picker_html(plate_picker(96, v, value = c(A01 = "S1", B01 = "S2", C01 = "S2")), f)
  d <- read_plate_picker(f)
  expect_equal(d$group, rep("Std", 3))
  expect_equal(d$conc, c(10, 5, 5))
  again <- plate_picker(96, value = d)
  expect_equal(as.character(again$x$labels), c("S1", "S2"))

  # a page saved by 0.4.2 has only the painting
  old <- tempfile(fileext = ".html")
  writeLines(paste0("<html><script type=\"application/json\" id=\"mtpr-picker-state\">",
                    "{\"version\":1,\"plate\":96,\"labels\":[\"a\"],\"assignments\":",
                    "{\"well\":[\"A01\"],\"label\":[\"a\"]}}</script></html>"), old)
  expect_equal(read_plate_picker(old), data.frame(well = "A01", label = "a"))
})

test_that("update_plate_picker() sends the normalised changes", {
  sent <- NULL
  session <- list(ns = function(id) paste0("mod-", id),
                  sendCustomMessage = function(type, message) sent <<- list(type = type, msg = message))
  update_plate_picker(session, "p", value = c(a1 = "x"), active = "x", locked = character())
  expect_equal(sent$type, "mtpr_plate_picker")
  expect_equal(sent$msg$id, "mod-p")
  expect_equal(as.character(sent$msg$value$well), "A01")
  expect_equal(sent$msg$active, "x")
  expect_length(sent$msg$locked, 0)
  expect_null(sent$msg$background)
  update_plate_picker(session, "p", value = data.frame(well = character(), label = character()))
  expect_length(sent$msg$value$well, 0)
  expect_error(update_plate_picker(session, "p", active = 1:2), "single label")
  update_plate_picker(session, "p", plate = 384, fill = "conc",
                      labels = data.frame(label = "a", conc = 1))
  expect_equal(sent$msg$plate, 384)
  expect_equal(sent$msg$fill, "conc")
  expect_equal(as.character(sent$msg$labels$labels), "a")
  expect_error(update_plate_picker(session, "p", plate = 100), "Unsupported plate size")
})

test_that("the Shiny output and render functions exist", {
  expect_s3_class(plate_pickerOutput("x"), "shiny.tag.list")
  expect_true(is.function(renderPlate_picker(plate_picker())))
})
