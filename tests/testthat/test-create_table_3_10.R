test_that("create_t3.10 function works correctly", {
  # Sample data
  dth_data <- data.frame(
    dodyr = c(2022, 2022, 2022, 2021, 2021),
    age_grp_wide = c("0-4", "5-9", "10-14", "15-19", "20-24"),
    sex = c("male", "female", "male", "female", "male")
  )

  dth_est <- data.frame(
    year = c(2022, 2022, 2022, 2022),
    age_grp = c("0-4", "5-9", "10-14", "15-19"),
    female = c(100, 200, 300, 400),
    male = c(150, 250, 350, 450)
  )

  result <- create_t3.10(dth_data, dth_est, "dodyr", data_year = 2022, tablename = "Table_3_10")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the file is written
  expect_true(file.exists("outputs/Table_3_10.csv"))

  # Check for expected columns in the result
  expected_cols <- c("age_grp", "reg_deaths_total", "reg_deaths_male", "reg_deaths_female",
                     "completeness_total", "completeness_male", "completeness_female",
                     "adjusted_total", "adjusted_male", "adjusted_female")
  expect_equal(colnames(result), expected_cols)

  # Clean up the generated file after the test
  unlink("outputs/Table_3_1.csv")
})
