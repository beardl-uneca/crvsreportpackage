test_that("create_table_4_9_and_4_10 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data
  bth_data <- data.frame(
    dobyr = c(2022, 2022, 2022, 2022, 2022),
    ruind = c("urban", "urban", "rural", "rural", "urban"),
    fert_age_grp = c("15-19", "20-24", "15-19", "20-24", "25-29"),
    sbind = c(NA, NA, NA, NA, NA)
  )

  bth_est <- data.frame(
    year = c(2022, 2022),
    fert_age_grp = c("15-19", "20-24"),
    total = c(100, 200)
  )

  pops <- data.frame(
    fert_age_grp = c("15-19", "20-24", "25-29"),
    sex = c("F", "F", "F"),
    population_2022 = c(1000, 800, 600)
  )

  # Call the function
  result4_9 <- create_table_4_9_and_4_10(bth_data, bth_est, pops, data_year = 2022, ruindicator = "urban", tablename = "Table_4_9")
  result4_10 <- create_table_4_9_and_4_10(bth_data, bth_est, pops, data_year = 2022, ruindicator = "rural", tablename = "Table_4_10")

  # Check if result is a data frame
  expect_s3_class(result4_9, "data.frame")
  expect_s3_class(result4_10, "data.frame")

  # Check for expected columns in the result
  expected_cols <- c("fert_age_grp", "total", "adjusted", "total_pop", "asfr")
  expect_equal(colnames(result4_9), expected_cols)
  expect_equal(colnames(result4_10), expected_cols)

  # Check if the file is written
  expect_true(as.logical(prod(file.exists(c("outputs/Table_4_9.csv", "outputs/Table_4_10.csv")))))

  # Clean up the generated file after the test
  unlink(c("outputs/Table_4_9.csv", "outputs/Table_4_10.csv"))
  # Reset working dir
  setwd(working_dir)

})
