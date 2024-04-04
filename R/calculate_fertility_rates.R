#' calcualtes age specific and total fertility rates per year
#'
#' @param data births data frame
#'
#' @return tablulated age specific and total fertility rates per year
#' @export
#' 
#' @import stringr
#'
#' @examples fertility_rates <- calculate_fertility_rates(bth_data)
#' 
calculate_fertility_rates <- function(data){

generate_year_sequence <- function(curr_year) {
  seq(curr_year - 5, curr_year - 1)
}

curr_year <- max(bth_data$dobyr, na.rm = TRUE)

# Generate the year sequence
year_sequence <- generate_year_sequence(curr_year)
output <- bth_data |>
  filter(is.na(sbind) & !is.na(fert_age_grp) & dobyr %in% year_sequence) |>
  group_by(fert_age_grp, dobyr) |>
  summarise(total = n(), .groups = "drop_last") |>
  pivot_wider(names_from = dobyr, values_from = total)

# Aggregate population data dynamically for each year
gfr_pops <- pops |>
  filter(sex == "F") |>
  group_by(fert_age_grp) |>
  summarise(across(starts_with("population"), sum))

# Merge fertility rate and population data
output <- merge(output, gfr_pops, by = "fert_age_grp") |>
  mutate(across(starts_with("20"), 
                ~round(. / get(paste0("population_", str_sub(cur_column(), -4))) * 1000, 1))) |>
  select(fert_age_grp, starts_with("20"))

total_fertility_rates <- output |>
  summarise(across(starts_with("20"), sum)) |>
  mutate(fert_age_grp = 'total') |>
  mutate(round_excel(across(starts_with("20"), ~ . * 5 / 1000),2)) |>
  select(fert_age_grp, starts_with("20"))

output <- rbind(output, total_fertility_rates)


return(output)
}


