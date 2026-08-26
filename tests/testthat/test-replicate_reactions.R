test_that("Reaction priority lays out replicates before advancing reactions", {
  out <- replicate_reactions(
    data.frame(rxn = 1:2), "rxn",
    num_replicates = 3, priority = "Reaction",
    inter_spacing = 10, intra_spacing = 1, start_position = 1
  )
  expect_equal(nrow(out), 6)
  expect_equal(out$position, c(1, 2, 3, 13, 14, 15))
  expect_equal(names(out)[1:3], c("rxn", "replicate", "position"))
})

test_that("Replicate priority cycles reactions within each replicate", {
  out <- replicate_reactions(
    data.frame(rxn = 1:2), "rxn",
    num_replicates = 3, priority = "Replicate",
    inter_spacing = 10, intra_spacing = 1, start_position = 1
  )
  expect_equal(out$position, c(1, 12, 23, 11, 22, 33))
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
