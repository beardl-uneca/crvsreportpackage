test_that("create_t4.1 function works correctly", {
  # Sample data
  bth_data <- data.frame(
    dobyr = c(2018, 2018, 2019, 2019, 2020),
    fert_age_grp = c("15-19", "15-19", "15-19", "15-19", "15-19"),
    sbind = c(NA, NA, NA, NA, NA),
    sex = c("male", "female", "male", "female", "male")
  )

  est_data <- data.frame(
    year = c(2018, 2019, 2020),
    female = c(100, 200, 300),
    male = c(150, 250, 350)
  )

  pops <- data.frame(
    population_2018 = c(5000, 6000),
    population_2019 = c(5200, 6200),
    population_2020 = c(5400, 6400),
    sex = c("male", "female"),
    fert_age_grp = c("15-19", "15-19")
  )

  # Call the function
  result <- create_t4.1(bth_data, est_data, pops, date_var = "dobyr", tablename = "Table_4_1")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the file is written
  expect_true(file.exists("./outputs/Table_4_1.csv"))

  # Check for expected columns in the result
  expected_cols <- c("Indicator", "2018", "2019", "2020")
  expect_equal(colnames(result), expected_cols)

  # Clean up the generated file after the test
  unlink("./outputs/Table_4_1.csv")
})
