context("Make sure data are formatted correctly")
library(bwsTools)

test_that("the column check works", {
  expect_error(get_checks(indiv, "id", "block", "item", "choice"))
})

test_that("-1, 0, 1 check works", {
  tmp <- indiv
  tmp$value <- ifelse(tmp$value == -1, "w", ifelse(tmp$value == 1, "b", "ns"))
  expect_error(get_checks(tmp, "id", "block", "label", "value"))
})

test_that("one best and one worst check works", {
  tmp <- indiv
  tmp$value[2] <- -1
  expect_error(get_checks(tmp, "id", "block", "label", "value"))
})

test_that("number of items per block check works", {
  tmp <- indiv
  expect_error(get_checks(tmp[-2, ], "id", "block", "label", "value"))
})

test_that("item appearing more than once in a block throws error", {
  tmp <- indiv
  tmp$label[2] <- "abortion"
  expect_error(get_checks(tmp, "id", "block", "label", "value"))
})

test_that("error thrown when a respondent has more trials than another", {
  tmp <- indiv[indiv$id %in% 1:2, ]
  tmp2 <- tmp[1:4, ]
  tmp2$block <- 14
  tmp <- rbind(tmp, tmp2)
  expect_error(get_checks(tmp, "id", "block", "label", "value"))
})

test_that("rating different items throws error", {
  tmp <- agg
  expect_error(get_checks(tmp, "pid", "trial", "character", "decision"))
})

test_that("pairwise check works", {
  tmp <- indiv
  tmp <- tmp[tmp$id == 1 & tmp$block != 10, ]
  expect_error(get_checks(tmp, "id", "block", "label", "value"))
})

test_that("pairwise warnings thrown when needed", {
  tmp <- agg
  tmp <- tmp[tmp$pid == 1, ]
  expect_warning(
    get_checks(tmp, "pid", "trial", "character", "decision", nonbibd = TRUE)
  )
})

test_that("pairwise errors thrown when needed", {
  tmp <- agg
  tmp <- tmp[tmp$pid == 1, ]
  expect_error(
    get_checks(tmp, "pid", "trial", "character", "decision")
  )
  
  tmp <- indiv
  tmp <- tmp[tmp$id == 1, ]
  tmp2 <- tmp[1:4, ]
  tmp2$block <- 14
  tmp <- rbind(tmp, tmp2)
  expect_error(get_checks(tmp, "id", "block", "label", "value"))
})

test_that("success returns nothing", {
  expect_null(get_checks(indiv, "id", "block", "label", "value"))
})
