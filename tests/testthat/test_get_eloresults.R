context("Get matchups for Elo scoring")
library(bwsTools)

test_that("it returns data.frame of winners and losers", {
  res <- get_eloresults(indiv[indiv$id == 2, ], "block", "label", "value")
  expect_true("data.frame" %in% class(res))
  expect_equal(ncol(res), 2)
  expect_named(res, c("winner", "loser"))
})
