#' Creates Table 4.2
#'
#' @param data data frame being used
#' @param est_data data frmae of estimated data being used
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
#' @examples t4.2 <- create_t4.2(bth_data, bth_est, data_year = 2022, tablename = "Table_4_2")
create_t4.2 <- function(data, est_data, data_year = 2022, tablename = "Table_4_2"){
  output <- data |>
  filter(dobyr == data_year & is.na(sbind)) |>
  group_by(rgn, sex) |>
  summarise(total = n()) |>
  pivot_wider(names_from = sex, values_from = total, values_fill = 0) |>
  adorn_totals(c("row","col")) |>
  mutate(ratio = round_excel(male/female,1))

est <- est_data |>
  filter(year == data_year) |>
  select(rgn, total)

output <- merge(output, est, by = "rgn") |>
  mutate(completeness = round_excel(Total/total*100, 1)) |>
  select("rgn", "male", "female", "not stated", "Total", "ratio", "completeness")

write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
return(output)
}

