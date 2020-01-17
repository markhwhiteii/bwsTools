context("Calculate Elo score for an individual")
library(bwsTools)

test_that("computation is working as it should", {
  res <- get_eloresults(indiv[indiv$id == 2, ], "block", "label", "value")
  set.seed(1839); res <- get_eloscores(res, iter = 5)
  expect_equal(round(res[res$item == "abortion", "elo", drop = TRUE]), 861)
  expect_equal(round(res[res$item == "education", "elo", drop = TRUE]), 999)
  expect_equal(round(res[res$item == "taxes", "elo", drop = TRUE]), 1038)
  
  res <- get_eloresults(indiv[indiv$id == 8, ], "block", "label", "value")
  set.seed(1839); res <- get_eloscores(res, iter = 1)
  expect_equal(round(res[res$item == "abortion", "elo", drop = TRUE]), 927)
  expect_equal(round(res[res$item == "education", "elo", drop = TRUE]), 1000)
  expect_equal(round(res[res$item == "taxes", "elo", drop = TRUE]), 961)
})

test_that("K is working as it should", {
  res <- get_eloresults(indiv[indiv$id == 2, ], "block", "label", "value")
  set.seed(1839); res1 <- get_eloscores(res, K = 1, iter = 1)
  set.seed(1839); res2 <- get_eloscores(res, K = 100, iter = 1)
  expect_true(var(res1$elo) < var(res2$elo))
})
