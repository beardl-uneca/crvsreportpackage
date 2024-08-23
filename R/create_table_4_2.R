#' Creates Table 4.2
#'
#'Table 4.2 Live births by place of occurrence and sex of newborn
#'
#' @param data data frame being used
#' @param est_data data frame of estimated data being used
#' @param date_var name of date variable
#' @param data_year year the data is for
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frame of tabulated counts
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t4.2 <- create_t4.2(data = bth_data, est_data = bth_est, date_var = dobyr,
#'                              data_year = 2022, tablename = "Table_4_2")
create_t4.2 <- function(data, est_data, date_var, data_year = NA, tablename = "Table_4_2"){

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }

  output <- data |>
    filter(!!sym(date_var) == data_year & is.na(sbind)) |>
    group_by(rgn, sex) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total, values_fill = 0) |>
    adorn_totals(c("row","col")) |>
    mutate(ratio = round_excel(male/female,1))

  est <- est_data |>
    filter(year == data_year) |>
    group_by(rgn) |>
    summarise(total = sum(total))

  output <- left_join(output, est, by = c("rgn")) |>
    mutate(completeness = round_excel(Total/total*100, 1)) |>
    select("rgn", "male", "female", "not stated", "Total", "ratio", "completeness")

  write.csv(output, paste0("outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}

