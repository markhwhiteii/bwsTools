context("Do random walks in a matrix from get_M")
library(bwsTools)

M <- get_M(indiv[indiv$id == 2, ], "w", "block", "label", "value")
set.seed(1839); res <- get_walkscores(M, 15)

test_that("there is a score for every item", {
  expect_equal(length(res), ncol(M))
  expect_equal(length(res), nrow(M))
  expect_equal(length(res), length(unique(indiv$label)))
})

test_that("the names match", {
  expect_equal(setdiff(names(res), colnames(M)), character(0))
  expect_equal(setdiff(names(res), rownames(M)), character(0))
  expect_equal(setdiff(names(res), unique(indiv$label)), character(0))
})

test_that("scores sum to one", {
  expect_equal(sum(res), 1)
})

rm(res, M)
