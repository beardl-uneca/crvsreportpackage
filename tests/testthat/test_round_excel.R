# unit test for the 'round_excel' function

library(testthat)


source("../../R/utils/round_excel.R")

test_that("round_excel function works correctly", {
  expect_equal(round_excel(1.14, 1), 1.1)  # Test round down
  expect_equal(round_excel(1.15, 1), 1.2) # Test round up
  expect_equal(round_excel(1.15, 0), 1.0)  # Test with zero
  expect_equal(round_excel(-1.4,0), -1)  # Test round down with negative number
  expect_equal(round_excel(-1.5, 0), -2) # Test round up with negative number
  expect_error(round_excel("a", 1))  # Test with a non-numeric input
})
