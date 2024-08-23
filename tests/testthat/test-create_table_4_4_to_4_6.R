test_that("create_t4.4_to_4_6 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data
  bth_data <- data.frame(
    doryr = c(2022, 2022, 2022, 2022, 2022),
    sbind = c(NA, NA, NA, NA, NA),
    fert_age_grp = c("15-19", "20-24", "25-29", "30-34", "35-39"),
    multbth = c("Single", "Multiple", "Single", "Single", "Multiple"),
    marstat = c("Married", "Single", "Married", "Single", "Married"),
    ruind = c("urban", "urban", "rural", "rural", "urban")
  )


  result <- create_t4.4_to_4_6(bth_data, data_year = 2022, col_var = "fert_age_grp", by_var = "multbth", rural_urban = "no", tablename = "Table_4_4")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check for expected columns in the result
  expected_cols <- c("fert_age_grp", "Single", "Multiple", "Total")
  expect_equal(colnames(result), expected_cols)

  # Check if the file is written
  ### NOTE ONLY ONE OF THREE IS CHECKED CURRENTLY
  expect_true(file.exists("outputs/Table_4_4.csv"))

  # Clean up the generated file after the test
  unlink("outputs/Table_4_4.csv")
  #unlink(c("outputs/Table_4_4.csv",
  #         "outputs/Table_4_5.csv",
  #         "outputs/Table_4_6.csv"))

  # Reset working dir
  setwd(working_dir)
})
