context("Compute individual difference scores")
library(bwsTools)

test_that("std keeps everything between -1 and +1", {
  res <- diffscoring(indiv, "id", "block", "label", "value", std = TRUE)
  
  expect_true(all(res$bws <= 1 & res$bws >= -1))
})

test_that("wide argument does what it is supposed to do", {
  res1 <- diffscoring(indiv, "id", "block", "label", "value")
  res2 <- diffscoring(indiv, "id", "block", "label", "value", wide = TRUE)
  
  expect_equal(ncol(res1), 3)
  expect_equal(ncol(res2), length(unique(indiv$label)) + 1)
})

test_that("computation is correct", {
  res <- diffscoring(indiv, "id", "block", "label", "value")
  
  expect_equal(
    res[res$id == 10 & res$label == "crime", "bws", drop = TRUE], 
    -2
  )
  expect_equal(
    res[res$id == 1 & res$label == "healthcare", "bws", drop = TRUE], 
    -3
  )
})

test_that("the result is not a grouped df", {
  expect_false(
    dplyr::is_grouped_df(diffscoring(indiv, "id", "block", "label", "value"))
  )
})
