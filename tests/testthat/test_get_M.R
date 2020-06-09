context("Get square matrix for network-based measures")
library(bwsTools)

tdat <- indiv[indiv$id == 2, ]
res1 <- get_M(tdat, "w", "block", "label", "value")
res2 <- get_M(tdat, "b", "block", "label", "value")
res3 <- get_M(tdat, "w", "block", "label", "value", FALSE)
res4 <- get_M(tdat, "b", "block", "label", "value", FALSE)

test_that("a square matrix of unique topics by unique topics is generated", {
  expect_true(identical(ncol(res1), nrow(res1)))
  expect_equal(ncol(res1), length(unique(tdat$label)))
  
  expect_true(identical(ncol(res2), nrow(res2)))
  expect_equal(ncol(res2), length(unique(tdat$label)))
})

test_that("rows all sum to one", {
  expect_true(all(sapply(rowSums(res1), function(x) isTRUE(all.equal(x, 1)))))
  expect_true(all(sapply(rowSums(res2), function(x) isTRUE(all.equal(x, 1)))))
})

test_that("best and worst matrices differ", {
  expect_false(all(round(res1 - res2, 2) == 0))
})

test_that("row sums of non-normalized best matrix is row columns of worst", {
  expect_true(all(rowSums(res3) == colSums(res4)))
  expect_true(all(rowSums(res4) == colSums(res3)))
})

test_that("computation is done correctly", {
  expect_true(round(res1["natsecurity", "corruption"], 2) == 0.33)
  expect_true(round(res2["natsecurity", "corruption"], 2) == 0)
  expect_true(res3["race", "abortion"] == 2)
  expect_true(res4["race", "abortion"] == 0)
})

rm(tdat, res1, res2, res3, res4)
