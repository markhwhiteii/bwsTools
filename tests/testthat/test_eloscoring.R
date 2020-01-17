context("Calculate individual Elo scores")
library(bwsTools)

test_that("computation is working as it should", {
  set.seed(1839)
  res <- eloscoring(indiv, "id", "block", "label", "value", iter = 1)
  expect_equal(
    round(res[res$id == 2 & res$label == "crime", "elo", drop = TRUE]),
    1043
  )
  expect_equal(
    round(res[res$id == 10 & res$label == "natsecurity", "elo", drop = TRUE]),
    992
  )
})

test_that("wide argument does what it is supposed to do", {
  res1 <- eloscoring(indiv, "id", "block", "label", "value", iter = 1)
  res2 <- eloscoring(indiv, "id", "block", "label", "value", 
                     iter = 1, wide = TRUE)
  
  expect_equal(ncol(res1), 3)
  expect_equal(ncol(res2), length(unique(indiv$label)) + 1)
})

test_that("the result is not a grouped df", {
  expect_false(dplyr::is_grouped_df(
    eloscoring(indiv, "id", "block", "label", "value", iter = 1)
  ))
})
