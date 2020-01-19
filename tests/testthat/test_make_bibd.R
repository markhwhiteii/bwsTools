context("Make BIBD")
library(bwsTools)

test_that("error is thrown at the correct time", {
  expect_error(make_bibd(33))
  expect_error(make_bibd(0))
})

test_that("the correct design is returned", {
  res1 <- make_bibd(5)
  des1 <- bibds[5, ]
  res2 <- make_bibd(29)
  des2 <- bibds[29, ]
  
  expect_equal(des1[["t"]], length(unique(unlist(res1[, -1]))))
  expect_equal(des1[["k"]], ncol(res1[, -1]))
  expect_equal(des1[["r"]], unique(table(unlist(res1[, -1]))))
  expect_equal(des1[["b"]], nrow(res1))
  
  expect_equal(des2[["t"]], length(unique(unlist(res2[, -1]))))
  expect_equal(des2[["k"]], ncol(res2[, -1]))
  expect_equal(des2[["r"]], unique(table(unlist(res2[, -1]))))
  expect_equal(des2[["b"]], nrow(res2))
})
