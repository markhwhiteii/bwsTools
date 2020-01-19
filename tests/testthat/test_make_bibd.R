context("Make BIBD")
library(bwsTools)

test_that("error is thrown at the correct time", {
  expect_error(make_bibd(33))
  expect_error(make_bibd(0))
})
