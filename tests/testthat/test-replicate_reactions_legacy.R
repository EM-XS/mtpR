# These pin the wells produced by the original `priority` interface (the
# layouts igoR calls "Together" and "EveryOther"), so that adding the
# `pattern` argument cannot change existing layouts.

test_that("priority = 'Reaction' (Together) positions are pinned", {
  d <- data.frame(rxn = c(3, 1, 2), k = c("c", "a", "b"))
  out <- replicate_reactions(d, "rxn", 3, "Reaction",
                             inter_spacing = 2, intra_spacing = 1,
                             start_position = 5)
  expect_equal(out$position, c(5, 6, 7, 9, 10, 11, 13, 14, 15))
  expect_equal(out$rxn, rep(1:3, each = 3))
  expect_equal(out$k, rep(c("a", "b", "c"), each = 3))

  out <- replicate_reactions(data.frame(rxn = 1:4), "rxn", 2, "Reaction",
                             inter_spacing = 1, intra_spacing = 1,
                             start_position = 1)
  expect_equal(out$position, 1:8)
})

test_that("priority = 'Replicate' (EveryOther) positions are pinned", {
  out <- replicate_reactions(data.frame(rxn = c(3, 1, 2)), "rxn", 3, "Replicate",
                             inter_spacing = 2, intra_spacing = 2,
                             start_position = 5)
  expect_equal(out$position, c(5, 11, 17, 7, 13, 19, 9, 15, 21))

  out <- replicate_reactions(data.frame(rxn = 1:4), "rxn", 2, "Replicate",
                             inter_spacing = 1, intra_spacing = 1,
                             start_position = 1)
  expect_equal(out$position, c(1, 5, 2, 6, 3, 7, 4, 8))

  out <- replicate_reactions(data.frame(rxn = 1:4), "rxn", 3, "Replicate",
                             inter_spacing = 1, intra_spacing = 7,
                             start_position = 10)
  expect_equal(out$position,
               c(10, 20, 30, 11, 21, 31, 12, 22, 32, 13, 23, 33))

  out <- replicate_reactions(data.frame(rxn = 1:3), "rxn", 3, "Replicate", 1, 1, 1)
  expect_equal(out$position, c(1, 4, 7, 2, 5, 8, 3, 6, 9))
})

test_that("row order and replicate labels are pinned", {
  out <- replicate_reactions(data.frame(rxn = c(3, 1, 2)), "rxn", 2,
                             "Reaction", 1, 1, 1)
  expect_equal(out$rxn, c(1, 1, 2, 2, 3, 3))
  expect_equal(out$replicate, rep(1:2, 3))
})
