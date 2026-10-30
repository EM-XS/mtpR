test_that("Reaction priority lays out replicates before advancing reactions", {
  out <- replicate_reactions(
    data.frame(rxn = 1:2), "rxn",
    num_replicates = 3, priority = "Reaction",
    inter_spacing = 10, intra_spacing = 1, start_position = 1
  )
  expect_equal(nrow(out), 6)
  expect_equal(out$Well.Index, c(1, 2, 3, 13, 14, 15))
  expect_equal(names(out)[1:3], c("rxn", "replicate", "Well.Index"))
})

test_that("Replicate priority cycles reactions within each replicate", {
  out <- replicate_reactions(
    data.frame(rxn = 1:2), "rxn",
    num_replicates = 3, priority = "Replicate",
    inter_spacing = 10, intra_spacing = 1, start_position = 1
  )
  expect_equal(out$Well.Index, c(1, 12, 23, 11, 22, 33))
})

test_that("extra input columns are carried through the join", {
  out <- replicate_reactions(
    data.frame(rxn = c("a", "b"), conc = c(5, 9)), "rxn",
    num_replicates = 2, priority = "Reaction",
    inter_spacing = 5, intra_spacing = 1, start_position = 1
  )
  expect_true("conc" %in% names(out))
  expect_equal(out$conc[out$rxn == "a"], c(5, 5))
  expect_equal(out$conc[out$rxn == "b"], c(9, 9))
})

test_that("bad arguments error", {
  expect_error(
    replicate_reactions(data.frame(rxn = 1), "rxn", 1, "nope", 1, 1, 1),
    "Priority"
  )
  expect_error(
    replicate_reactions(data.frame(rxn = 1), "missing", 1, "Reaction", 1, 1, 1),
    "does not exist"
  )
})

test_that("NA reaction identifiers are an error", {
  expect_error(
    replicate_reactions(data.frame(r = c(1, NA, 3)), "r", 1, pattern = "grouped"),
    "must not be NA"
  )
})

test_that("columns the result adds cannot already be in rxn_df", {
  rep_col <- data.frame(rxn = 1:2, replicate = c("a", "b"))
  expect_error(replicate_reactions(rep_col, "rxn", 2, pattern = "grouped"), "`replicate`. Rename")
  idx_col <- data.frame(rxn = 1:2, Well.Index = 5:6)
  expect_error(replicate_reactions(idx_col, "rxn", 2, pattern = "dealt"), "`Well.Index`. Rename")
  both <- data.frame(rxn = 1:2, replicate = 1, Well.Index = 2)
  expect_error(replicate_reactions(both, "rxn", 2, pattern = "grouped"), "`replicate`, `Well.Index`")
  # also when one of them holds the reaction identifiers
  expect_error(replicate_reactions(data.frame(Well.Index = 1:2), "Well.Index", 2, pattern = "grouped"), "`Well.Index`")

  quad <- data.frame(rxn = 1:2, Quadrant = 9)
  expect_error(replicate_reactions(quad, "rxn", 2, pattern = "quadrants", plate = 96), "`Quadrant`")
  # only quadrants adds that column
  expect_equal(replicate_reactions(quad, "rxn", 2, pattern = "grouped")$Quadrant, rep(9, 4))
})

test_that("the numbering is Well.Index, which add_plates() and convert_well() use", {
  r <- replicate_reactions(data.frame(rxn = 1:3), "rxn", 40, pattern = "grouped")
  expect_equal(names(r), c("rxn", "replicate", "Well.Index"))
  expect_equal(max(r$Well.Index), 120)
  p <- add_plates(r, "Well.Index", 96)
  expect_equal(max(p$Well.Index), 96)
  expect_equal(max(p$Plate.Index), 2)
  expect_equal(p$Well.Index[97], 1)
  expect_equal(convert_well(p$Well.Index[1:2], "to_well", plate = 96), c("A01", "A02"))
  # a column of the old name is just another column now
  old <- replicate_reactions(data.frame(rxn = 1:2, position = 5:6), "rxn", 1, pattern = "grouped")
  expect_equal(old$position, 5:6)
})

test_that("a replicated layout cannot be replicated again by accident", {
  once <- replicate_reactions(data.frame(rxn = 1:2), "rxn", 2, pattern = "grouped")
  expect_error(replicate_reactions(once, "rxn", 2, pattern = "grouped"), "Rename")
})

test_that("other rxn_df columns keep their names", {
  d <- data.frame(rxn = 1:2, reaction = c("x", "y"), note = c("a", "b"))
  r <- replicate_reactions(d, "rxn", 2, pattern = "grouped")
  expect_equal(names(r), c("rxn", "replicate", "Well.Index", "reaction", "note"))
  expect_equal(r$reaction, c("x", "x", "y", "y"))

  same <- data.frame(reaction = 1:2, note = c("a", "b"))
  r <- replicate_reactions(same, "reaction", 2, pattern = "grouped")
  expect_equal(names(r), c("reaction", "replicate", "Well.Index", "note"))
})
