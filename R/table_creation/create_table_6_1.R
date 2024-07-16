#' Create Table 6.1
#'
#' @param data data frame being used
#' @param data_year year of data
#'
#' @return Returns data frame with tabulated results
#' @export
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t6.1 <- create_t6.1(data = dth_data, date_var = dodyr, data_year = 2022)
#'
create_t6.1 <- function(data, date_var, data_year){
output <- data |>
  filter({{date_var}} == data_year & fic10und != "" & sex %in% c("male", "female")) |>
  group_by(sex, substr(fic10und,1,3), age_grp_lead) |>
  summarise(total = n()) |>
  pivot_wider(names_from = sex, values_from = total, values_fill = 0) |>
  arrange(age_grp_lead) |>
  rename(causecd = `substr(fic10und, 1, 3)`)

output <- left_join(output, cause, by = c("causecd" = "code"))

outputm <- output |>
  select(description, age_grp_lead, male) |>
  group_by(age_grp_lead, description) |>
  summarise(total = sum(male)) |>
  group_by(age_grp_lead) |>
  slice_max(order_by = total, n = 5, with_ties = FALSE)

outputf <- output |>
  select(description, age_grp_lead, female) |>
  group_by(age_grp_lead, description) |>
  summarise(total = sum(female)) |>
  group_by(age_grp_lead) |>
  slice_max(order_by = total, n = 5, with_ties = FALSE)

output <- cbind(outputm, outputf)

colnames(output) <- c("Male_Age_Group", "Cause_Male", "countm", "Female_Age_Group", "Cause_Female", "countf")
output <- output |>
  select(Male_Age_Group, Cause_Male, Female_Age_Group, Cause_Female)

return(output)
}
