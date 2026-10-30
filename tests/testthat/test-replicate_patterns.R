rx <- data.frame(rxn = 1:3)

test_that("pattern = 'grouped' and 'dealt' equal the old priority values", {
  for (sp in list(c(1, 1), c(2, 1), c(1, 7))) {
    g_old <- replicate_reactions(rx, "rxn", 3, "Reaction", sp[1], sp[2], 4)
    g_new <- replicate_reactions(rx, "rxn", 3, pattern = "grouped",
                                 inter_spacing = sp[1], intra_spacing = sp[2],
                                 start_position = 4)
    expect_equal(g_new$Well.Index, g_old$Well.Index)
    d_old <- replicate_reactions(rx, "rxn", 3, "Replicate", sp[1], sp[2], 4)
    d_new <- replicate_reactions(rx, "rxn", 3, pattern = "dealt",
                                 inter_spacing = sp[1], intra_spacing = sp[2],
                                 start_position = 4)
    expect_equal(d_new$Well.Index, d_old$Well.Index)
  }
})

test_that("Together and EveryOther are accepted as patterns", {
  expect_equal(
    replicate_reactions(rx, "rxn", 3, pattern = "Together")$Well.Index,
    replicate_reactions(rx, "rxn", 3, "Reaction")$Well.Index
  )
  expect_equal(
    replicate_reactions(rx, "rxn", 3, pattern = "EveryOther")$Well.Index,
    replicate_reactions(rx, "rxn", 3, "Replicate")$Well.Index
  )
})

test_that("grouped reads 111 222 333 and dealt reads 123 123 123", {
  g <- replicate_reactions(rx, "rxn", 3, pattern = "grouped")
  expect_equal(g$rxn[order(g$Well.Index)], rep(1:3, each = 3))
  d <- replicate_reactions(rx, "rxn", 3, pattern = "dealt")
  expect_equal(d$rxn[order(d$Well.Index)], rep(1:3, times = 3))
})

test_that("scattered is a reproducible permutation of the same positions", {
  a <- replicate_reactions(rx, "rxn", 3, pattern = "scattered", seed = 11)
  b <- replicate_reactions(rx, "rxn", 3, pattern = "scattered", seed = 11)
  c2 <- replicate_reactions(rx, "rxn", 3, pattern = "scattered", seed = 12)
  expect_equal(a$Well.Index, b$Well.Index)
  expect_false(identical(a$Well.Index, c2$Well.Index))
  expect_setequal(a$Well.Index, 1:9)
  expect_equal(attr(a, "seed"), 11)
  # a seed is recorded even when none is given
  expect_true(is.numeric(attr(replicate_reactions(rx, "rxn", 3, pattern = "scattered"), "seed")))
})

test_that("scattered leaves the caller's random stream alone", {
  set.seed(5); x <- runif(1)
  set.seed(5)
  replicate_reactions(rx, "rxn", 3, pattern = "scattered", seed = 1)
  expect_equal(runif(1), x)
})

test_that("replicates can differ per reaction", {
  d <- data.frame(rxn = 1:3, n = c(2, 3, 1))
  by_col <- replicate_reactions(d, "rxn", "n", pattern = "grouped")
  by_vec <- replicate_reactions(d, "rxn", c(2, 3, 1), pattern = "grouped")
  by_syn <- replicate_reactions(d, "rxn", replicates = c(2, 3, 1), pattern = "grouped")
  expect_equal(by_col$Well.Index, 1:6)
  expect_equal(by_col$rxn, c(1, 1, 2, 2, 2, 3))
  expect_equal(by_vec$Well.Index, by_col$Well.Index)
  expect_equal(by_syn$Well.Index, by_col$Well.Index)

  dl <- replicate_reactions(d, "rxn", "n", pattern = "dealt")
  expect_equal(dl$rxn[order(dl$Well.Index)], c(1, 2, 3, 1, 2, 2))
  expect_equal(nrow(replicate_reactions(d, "rxn", "n", pattern = "scattered", seed = 1)), 6)
})

test_that("a vector is matched to reactions, not to sorted order", {
  d <- data.frame(rxn = c(3, 1), n = c(1, 2))
  out <- replicate_reactions(d, "rxn", c(1, 2), pattern = "grouped")
  expect_equal(as.integer(table(out$rxn)), c(2L, 1L))  # reaction 1 has n = 2
})

test_that("bad pattern and replicate arguments error", {
  expect_error(replicate_reactions(rx, "rxn", 3, pattern = "zigzag"), "pattern")
  expect_error(replicate_reactions(rx, "rxn", 3), "pattern")
  expect_error(replicate_reactions(rx, "rxn", c(1, 2), pattern = "grouped"), "entries")
  expect_error(replicate_reactions(rx, "rxn", 0, pattern = "grouped"), "at least 1")
  expect_error(replicate_reactions(rx, "rxn", "nope", pattern = "grouped"), "does not exist")
  expect_error(replicate_reactions(data.frame(rxn = c(1, 1)), "rxn", 2, pattern = "grouped"),
               "unique")
})

test_that("reserved wells are skipped and the layout flows around them", {
  ctl <- place_controls(96, wells = c("A02", "A04"))
  out <- replicate_reactions(rx, "rxn", 2, pattern = "grouped", reserved = ctl)
  expect_equal(out$Well.Index, c(1, 3, 5, 6, 7, 8))
  out <- replicate_reactions(rx, "rxn", 2, pattern = "dealt", reserved = c("A02", "A04"),
                             plate = 96)
  expect_false(any(out$Well.Index %in% c(2, 4)))
  expect_equal(nrow(out), 6)
})

test_that("reserved wells repeat on the next plate", {
  ctl <- place_controls(6, wells = "A01")   # 6-well plate: 2 rows x 3 columns
  out <- replicate_reactions(data.frame(rxn = 1:7), "rxn", 1, pattern = "grouped",
                             reserved = ctl)
  expect_equal(out$Well.Index, c(2, 3, 4, 5, 6, 8, 9))
})

test_that("reserved changes nothing when NULL", {
  expect_equal(
    replicate_reactions(rx, "rxn", 3, pattern = "dealt", reserved = NULL)$Well.Index,
    replicate_reactions(rx, "rxn", 3, "Replicate")$Well.Index
  )
})

test_that("a fully reserved plate errors", {
  expect_error(
    replicate_reactions(rx, "rxn", 1, pattern = "grouped", reserved = 1:6, plate = 6),
    "reserved"
  )
})
