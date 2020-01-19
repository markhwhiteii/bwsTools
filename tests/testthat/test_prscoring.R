context("Compute page rank scores")
library(bwsTools)

res1 <- prscoring(indiv, "id", "block", "label", "value")
res2 <- prscoring(indiv, "id", "block", "label", "value", wide = TRUE)

test_that("wide argument does what it is supposed to do", {
  expect_equal(ncol(res1), 3)
  expect_equal(ncol(res2), length(unique(indiv$label)) + 1)
})

test_that("the result is not a grouped df", {
  expect_false(dplyr::is_grouped_df(res1))
  expect_false(dplyr::is_grouped_df(res2))
})

rm(res1, res2)
