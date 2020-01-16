context("Compute one b and SE from counts")
library(bwsTools)

test_that("calculations are being done correctly", {
  expect_equal(
    round(get_b0(7145, 1733, 1324), 3), 
    c(b = .115, se = .017)
  )
  expect_equal(
    round(get_b0(100, 40, 10), 3), 
    c(b = .619, se = .148)
  )
})

test_that("correct inputs return named double vector of length two", {
  expect_length(get_b0(7145, 1733, 1324), 2)
  expect_length(get_b0(1000, 333, 333), 2)
  expect_length(get_b0(1000, 900, 1), 2)
  
  expect_type(get_b0(7145, 1733, 1324), "double")
  expect_type(get_b0(1000, 333, 333), "double")
  expect_type(get_b0(1000, 900, 1), "double")
  
  expect_named(get_b0(7145, 1733, 1324), c("b", "se"))
  expect_named(get_b0(1000, 333, 333), c("b", "se"))
  expect_named(get_b0(1000, 900, 1), c("b", "se"))
})

test_that("NAs return NA", {
  expect_true(all(is.na(get_b0(7145, 1733, NA))))
  expect_true(all(is.na(get_b0(1000, NA, 333))))
  expect_true(all(is.na(get_b0(NA, 900, 1))))
  expect_true(all(is.na(get_b0(NA, 1733, NA))))
  expect_true(all(is.na(get_b0(NA, NA, NA))))
})

test_that("errors or warnings are thrown when wrong types are input", {
  expect_error(get_b0("7145", 1733, 1324))
  expect_error(get_b0(7145, "1733", 1324))
  expect_error(get_b0(7145, "1733", 1324))
  expect_warning(get_b0(factor("7145"), factor("1733"), factor("1324")))
})

test_that("warning thrown when totals < bests + worsts", {
  expect_error(get_b0(1, 100, 900))
  expect_error(get_b0(1000, 100, 901))
})

test_that("error is thrown when negative inputs given", {
  expect_error(get_b0(-7145, 1733, 1324))
  expect_error(get_b0(-7145, -1733, -1324))
  expect_error(get_b0(7145, -1733, -1324))
  expect_error(get_b0(7145, 1733, -1324))
})
