
#' creates table 4.8
#'
#' @param data data frame being used
#' @param est_data data frame for estimate data
#' @param date_var occurrence data variable being used
#' @param data_year year of data
#' @param by_var variable the data is grouped by
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated result
#' @export
#' 
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t4.8 <- create_t4.8(bth_data, bth_est, dobyr, data_year = 2022, by_var = rgn, tablename = "Table_4_8")

create_t4.8 <- function(data, est_data, date_var, data_year = 2022, by_var = NA, tablename = "Table_4_8"){
output <- data |>
  filter({{date_var}} == data_year) |>
  group_by({{by_var}}) |>
  summarise(total = n()) 

est <- est_data |>
  filter(year == data_year) |>
  group_by(rgn) |>
  summarise(est_total = sum(total))

pop <- pops |>
  select(rgn, paste0("population_", data_year)) |>
  group_by(rgn) |>
  summarise(total_pop = sum(!!sym(paste0("population_",data_year))))

output <- left_join(output, est, by = "rgn") |>
  mutate(completeness = round_excel(total/est_total*100, 2)) |>
  mutate(adjusted = floor(total/(completeness/100)))

output <- left_join(output, pop, by = "rgn") |>
  mutate(cbr = round_excel(adjusted/total_pop*1000, 1)) |>
  select(rgn, total, adjusted, cbr)

write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE) 

return(output)
}
