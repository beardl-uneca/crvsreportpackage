
#' Creates Tables 4.9 and 4.10
#'
#' @param data data frame being used
#' @param est_data data frame for estimate data
#' @param data_year year of data
#' @param ruindicator whether table is for urban or rural data
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated result
#' @export
#' 
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t4.9 <- create_table_4_9_and_4_10(data = bth_data, est_data = bth_est, data_year = 2022,
#'                                  ruindicator = "urban", tablename = "Table_4_9")
#' t4.10 <- create_table_4_9_and_4_10(data = bth_data, est_data = bth_est, data_year = 2022,
#'                                  ruindicator = "rural", tablename = "Table_4_10")

create_table_4_9_and_4_10 <- function(data, est_data, data_year = 2022,
                                      ruindicator = "urban", tablename = NA){
output <- data |>
  filter(is.na(sbind) & dobyr == data_year) |>
  group_by(fert_age_grp) |>
  summarise(total = n())

outputb <- est_data |>
  filter(year == data_year) |>
  group_by(fert_age_grp) |>
  summarise(total_est = sum(total)) |>
  rename(fert_age_est = fert_age_grp)

output <- cbind(output, outputb) |>
  select(-fert_age_est) |>
  mutate(completeness = total/total_est) |>
  mutate(adjusted = floor(total/completeness)) |>
  select(-c(total_est, completeness))

popn <- pops |>
  filter(sex == "F") |>
  group_by(fert_age_grp) |>
  summarise(total_pop = sum(population_2022))

output <- merge(output, popn, by = "fert_age_grp", all.x = TRUE) |>
  mutate(asfr = round_excel(adjusted/total_pop*1000, 2))

write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE) 
return(output)
}



