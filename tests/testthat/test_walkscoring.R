context("Compute walkscores")
library(bwsTools)

res1 <- walkscoring(indiv, "id", "block", "label", "value", 10)
res2 <- walkscoring(indiv, "id", "block", "label", "value", 10, TRUE)

test_that("wide argument does what it is supposed to do", {
  expect_equal(ncol(res1), 3)
  expect_equal(ncol(res2), length(unique(indiv$label)) + 1)
})

test_that("the result is not a grouped df", {
  expect_false(dplyr::is_grouped_df(res1))
  expect_false(dplyr::is_grouped_df(res2))
})

rm(res1, res2)
