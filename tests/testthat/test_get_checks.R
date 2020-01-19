context("Make sure data is formatted correctly")
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
  expect_error(get_checks(tmp[-2, ], "id", "block", "label", "value"))
})

test_that("pairwise check works", {
  tmp <- indiv
  tmp <- tmp[tmp$id == 1 & tmp$block != 10, ]
  expect_error(get_checks(tmp, "id", "block", "label", "value"))
})

test_that("success returns nothing", {
  expect_null(get_checks(indiv, "id", "block", "label", "value"))
})
