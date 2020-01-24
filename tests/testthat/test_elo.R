context("Calculate aggregate Elo scores")
library(bwsTools)

res1 <- elo(agg, "pid", "trial", "character", "decision", iter = 1)
res2 <- elo(indiv, "id", "block", "label", "value", iter = 100)

test_that("function works on BIBD and non-BIBD data", {
  expect_equal(nrow(res1), length(unique(agg$character)))
  expect_equal(nrow(res2), length(unique(indiv$label)))
  
  expect_equal(ncol(res1), 2)
  expect_equal(ncol(res2), 2)
})

test_that("function outputs scores that are not constant", {
  expect_true(length(unique(round(res1$elo))) > 1)
  expect_true(length(unique(round(res2$elo))) > 1)
})

test_that("scores between ae_mnl and elo correlate positively", {
  totals <- unique(table(indiv$label))
  bests <- with(indiv, tapply(value, label, function(x) sum(x == 1)))
  worsts <- with(indiv, tapply(value, label, function(x) sum(x == -1)))
  res3 <- ae_mnl(data.frame(totals, bests, worsts), "totals", "bests", "worsts")
  
  expect_true(cor(res3$b, res2$elo) > .9)
})

rm(res1, res2)
