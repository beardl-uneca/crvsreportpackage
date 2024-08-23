test_that("create_t4.2 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data
  bth_data <- data.frame(
    dobyr = c(2022, 2022, 2022, 2022, 2022, 2022),
    sbind = c(NA, NA, NA, NA, NA, NA),
    rgn = c("Region1", "Region1", "Region2", "Region2", "Region2", "Region1"),
    sex = c("male", "female", "male", "female", "male", "not stated")
  )

  bth_est <- data.frame(
    year = c(2022, 2022),
    rgn = c("Region1", "Region2"),
    total = c(200, 300)
  )

  # Call the function
  result <- create_t4.2(bth_data, bth_est, date_var = "dobyr", data_year = 2022, tablename = "Table_4_2")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the file is written
  expect_true(file.exists("outputs/Table_4_2.csv"))

  # Check for expected columns in the result
  expected_cols <- c("rgn", "male", "female", "not stated", "Total", "ratio", "completeness")
  expect_equal(colnames(result), expected_cols)

  # Clean up the generated file after the test
  unlink("outputs/Table_4_2.csv")

  setwd(working_dir)
})
