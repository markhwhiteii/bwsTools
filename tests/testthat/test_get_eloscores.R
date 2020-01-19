context("Calculate Elo score for an individual")
library(bwsTools)

test_that("K is working as it should", {
  res <- get_eloresults(indiv[indiv$id == 2, ], "block", "label", "value")
  set.seed(1839); res1 <- get_eloscores(res, K = 1, iter = 1)
  set.seed(1839); res2 <- get_eloscores(res, K = 100, iter = 1)
  expect_true(var(res1$elo) < var(res2$elo))
})
