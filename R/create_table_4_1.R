
#' Calculates Table 4.1 Births summary table
#'
#' @param data births data frame
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @examples t4.1 <- create_t4.1(bth_data, tablename = "Table_4_1")
#'
create_t4.1 <- function(data, tablename = "Table_4_1"){
output <- data |>
  filter(is.na(sbind) & dobyr %in% c((max(dobyr, na.rm = TRUE) - 4):max(dobyr, na.rm = TRUE) - 1) & sex != "not stated")|>
  group_by(sex, dobyr) |>
  rename(Indicator = sex) |>
  summarise(total = n())


t4.1_counts <- output |>
  pivot_wider(names_from = Indicator, values_from = total)

t4.1_comp <- bth_est |>
  group_by(year) |>
  summarise(ftotal = sum(female), mtotal = sum(male))

t4.1_comp <- cbind(t4.1_counts, t4.1_comp) |>
  mutate(male_comp = round_excel(male/mtotal*100, 1),
         female_comp = round_excel(female/ftotal*100, 1)) |>
  select(year, male_comp, female_comp) |>
  pivot_longer(cols = c(male_comp, female_comp), names_to = "Indicator", values_to =  "counts") |>
  pivot_wider(names_from = year, values_from = counts)

t4.1_counts <- t4.1_counts |>
  pivot_longer(cols = c(male, female), names_to = "Indicator", values_to =  "counts") |>
  pivot_wider(names_from = dobyr, values_from = counts)

population <- pops |>
  select(starts_with("popu"), sex) |>
  pivot_longer(cols = starts_with("popu"), names_to = "year", values_to = "count" ) |>
  mutate(year = gsub("population_", "", year)) |>
  group_by(year, sex) |>
  summarise(total_pop = sum(count)) |>
  arrange(sex)

t4.1_cbr <- cbind(output, population) |>
  select(year, Indicator, total, total_pop ) |>
  group_by(year) |>
  summarise(total = sum(total), total_pop = sum(total_pop)) |>
  mutate(cbr = round_excel((total/total_pop)*1000,2)) |>
  select(year, cbr) |>
  mutate(Indicator = "CBR") |>
  pivot_wider(names_from = year, values_from = cbr)


t4.1_ratio <- output |>
  pivot_wider(names_from = Indicator, values_from = total) |>
  mutate(total = male + female,
         Ratio = round((male/female),2)) |>
  select(dobyr, Ratio) |>
  pivot_wider(names_from = dobyr, values_from = Ratio) |>
  mutate(Indicator = "Ratio") |>
  select(Indicator, starts_with("20"))
t4.1_ratio <- t4.1_ratio[1,]

fertility_rates <- calculate_fertility_rates(bth_data) |>
  rename(Indicator = fert_age_grp) |>
  filter(Indicator == "total")



output <- rbind(t4.1_counts, t4.1_comp, t4.1_ratio, t4.1_cbr, fertility_rates)

write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)

return(output)

}


