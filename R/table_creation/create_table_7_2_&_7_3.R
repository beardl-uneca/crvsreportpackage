#' Create Tables 7.2 and 7.3
#'
#' @param data data frame being used
#' @param data_year year the data is for
#' @param ruindicator whether the data is for urban or rural
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frame of tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t7.2 <- create_t7.2_and_7.3(marr_data, data_year = 2020, ruindicator = "urban", tablename = "Table_7_1")
#' t7.3 <- create_t7.2_and_7.3(marr_data, data_year = 2020, ruindicator = "rural", tablename = "Table_7_2")
#' 
create_t7.2_and_7.3 <- function(data, data_year, ruindicator = "urban", tablename = NA){

output <- data |>
  filter(year == data_year & ruind == "urban") |>
  group_by(g_age_grp, b_age_grp) |>
  summarise(total = n()) |>
  pivot_wider(names_from = b_age_grp, values_from = total, values_fill = 0) |>
  adorn_totals(c("col", "row"))

return(output)
}
