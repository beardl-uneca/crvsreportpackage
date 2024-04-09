
#' Calculates Tables 5.6 and 5.7 Deaths by age and sex of decedent, Urban/Rural
#'
#' @param data dataframe being used
#' @param datayear year of data
#' @param ru_filter whether rural or urban
#' @param date_var year of data
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return Table 5.6 & 5.7
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t5.6 <- create_t5.6_and_t5.7(data = dth_data, ru_filter = "urban",
#'          date_var = dodyr, datayear = 2022, tablename = "Table_5_6")
#'           t5.7 <- create_t5.6_and_t5.7(data = dth_data, ru_filter = "rural",
#'           date_var = dodyr, datayear = 2022, tablename = "Table_5_7")
#'
create_t5.6_and_t5.7 <- function(data, ru_filter, date_var, datayear = 2022, tablename = NA){
  output <- data |>
    filter(ruind == ru_filter & {{date_var}} == datayear) |>
    group_by(sex, age_grp_80) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total, values_fill = 0) |>
    arrange(age_grp_80) |>
    adorn_totals(c("col", "row")) |>
    rename(`Age of decedent (years)` = age_grp_80,
           `Total number of deaths` = Total) |>
    select(`Age of decedent (years)`, male, female, `Total number of deaths`)

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
