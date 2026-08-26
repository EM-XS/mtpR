test_that("each reaction is duplicated once per plate replicate", {
  out <- add_plate_replicates(data.frame(ReactionID = 1:10), 3)
  expect_equal(nrow(out), 30)
  expect_setequal(unique(out$PlateReplicate.Number), 1:3)
  expect_equal(sort(out$ReactionID), sort(rep(1:10, 3)))
  expect_equal(as.integer(table(out$ReactionID)), rep(3L, 10))
})

test_that("non-data-frame / non-numeric input errors", {
  expect_error(add_plate_replicates(list(a = 1), 2))
  expect_error(add_plate_replicates(data.frame(a = 1), "three"))
})
