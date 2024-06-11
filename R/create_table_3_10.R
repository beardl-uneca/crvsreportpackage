#' Create Table 3.10
#'
#' @param data dataframe being used
#' @param date_var occurrence data being used e.g. dobyr, dodyr etc
#' @param data_year year the data is for
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frame of tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.10 <- create_t3.10(dth_data, date_var = dodyr, data_year = 2022, tablename = "Table_3_10")
#'
create_t3.10 <- function(data, date_var, data_year, tablename = NA){
output <- data |>
  filter({{date_var}} == data_year, sex != "not stated") |>
  group_by(age_grp_wide, sex) |>
  summarise(reg_deaths = n()) |>
  rename(age_grp = age_grp_wide)

output_all <- output  |>
   mutate(sex = "total") |>
   group_by(age_grp, sex) |>
   summarise(reg_deaths = sum(reg_deaths))

output <- rbind(output_all, output)

d_est_prop <- dth_est  |>
  filter(year == data_year ) |>
  group_by(age_grp) |>
  summarise(female = sum(female), male = sum(male)) |>
  pivot_longer(cols = c(female, male) , names_to = "sex", values_to = "est_count")

d_est_prop_all <- d_est_prop |>
  mutate(sex = "total") |>
  group_by(age_grp, sex) |>
  summarise(est_count = sum(est_count))

d_est_prop <- rbind(d_est_prop_all, d_est_prop)

output <- merge(output, d_est_prop, by.x = c("age_grp", "sex"), by.y = c("age_grp","sex"), all.x = TRUE) |>
  mutate(completeness = round_excel((reg_deaths/est_count),2)) |>
  mutate(adjusted = floor((reg_deaths/ completeness)),
         completeness = completeness * 100) |>
  select(-c(est_count)) |>
  pivot_wider(names_from = sex, values_from = c(reg_deaths, completeness, adjusted)) |>
  adorn_totals("row", name = "Grand total") |>
  select("age_grp", "reg_deaths_male", "adjusted_male", "reg_deaths_female", "adjusted_female", "reg_deaths_total", "adjusted_total") |>
  arrange(match(age_grp, c("0-4", "5-24", "25-74", "75+", "Grand total")))


write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
return(output)
}


