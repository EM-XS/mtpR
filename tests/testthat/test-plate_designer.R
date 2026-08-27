# The Shiny GUI is experimental; these tests exercise the module server logic
# (not the browser) so regressions in the wiring are caught by `R CMD check`.

test_that("start_well_choices tracks plate size and fill direction", {
  row96 <- start_well_choices("96", "row")
  expect_length(row96, 96)
  expect_equal(row96[1:3], c("A01", "A02", "A03"))
  expect_equal(row96[96], "H12")

  col96 <- start_well_choices("96", "col")
  expect_length(col96, 96)
  expect_equal(col96[1:3], c("A01", "B01", "C01"))

  expect_length(start_well_choices("384", "row"), 384)
  expect_length(start_well_choices("1536", "row"), 1536)
})

test_that("plate_designer_server builds a plate map on demand", {
  shiny::testServer(plate_designer_server, args = list(id = "Setup"), {
    session$setInputs(
      destplate = "96", fillwise = "row", rxnCount = 8, replicatecount = 3,
      platereplicates = 2, deststartwell = "A01", replicatestyle = "Reaction",
      interwell = 1, intrawell = 1, createPlateMap = 1
    )
    pm <- plate_map()
    expect_s3_class(pm, "data.frame")
    expect_equal(nrow(pm), 8 * 3 * 2)
    expect_setequal(
      names(pm),
      c("reaction_count", "replicate", "position", "Plate.Index",
        "Well.Index", "Well.Position", "PlateReplicate.Number", "Plate Name")
    )
    # regression: Plate Name must be plain character, not a glue object
    expect_type(pm[["Plate Name"]], "character")
    expect_setequal(pm$PlateReplicate.Number, c(1, 2))
    expect_true(all(pm$Plate.Index == 1))

    # outputs render without error
    expect_true(nchar(output$PlateMap) > 100)
    expect_match(output$downloadCsv, "\\.csv$")
  })
})

test_that("plate_designer_server surfaces build failures as a validation message", {
  shiny::testServer(plate_designer_server, args = list(id = "Setup"), {
    # A negative reaction count (user typed past the input's min) makes
    # seq_len() throw inside the pipeline.
    session$setInputs(
      destplate = "96", fillwise = "row", rxnCount = -1, replicatecount = 3,
      platereplicates = 1, deststartwell = "A01", replicatestyle = "Reaction",
      interwell = 1, intrawell = 1, createPlateMap = 1
    )
    # eventReactive should raise a shiny.silent.error (validation), not a raw error
    err <- tryCatch(plate_map(), error = function(e) e)
    expect_s3_class(err, "shiny.silent.error")
  })
})
