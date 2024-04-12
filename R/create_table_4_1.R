
#' Calculates Table 4.1 Births summary table
#'
#' @param data births data frame
#' @param date_var variable for year
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t4.1 <- create_t4.1(bth_data, date_var = dobyr, tablename = "Table_4_1")
#'
create_t4.1 <- function(data, date_var, tablename = "Table_4_1"){
  by_var2 <- enquo(date_var)
  by_var_name <- quo_name(by_var2)

  curr_year <- max(bth_data[[by_var_name]], na.rm = TRUE) - 1
  start_year <- max(curr_year, na.rm = TRUE) - 4


  output <- data |>
    filter(is.na(sbind) & {{date_var}} %in% c(start_year:curr_year) & sex != "not stated")|>
    group_by(sex, {{date_var}}) |>
    rename(Indicator = sex) |>
    summarise(total = n())

  output_counts <- output |>
    pivot_wider(names_from = Indicator, values_from = total)

  output_comp <- est_data |>
    filter(year %in% c(start_year:curr_year)) |>
    group_by(year) |>
    summarise(ftotal = sum(female), mtotal = sum(male))


  output_comp <- cbind(output_counts, output_comp) |>
    mutate(male_comp = round_excel(male/mtotal*100, 1),
           female_comp = round_excel(female/ftotal*100, 1)) |>
    select(year, male_comp, female_comp) |>
    pivot_longer(cols = c(male_comp, female_comp), names_to = "Indicator", values_to =  "counts") |>
    pivot_wider(names_from = year, values_from = counts)

  output_counts <- output_counts |>
    pivot_longer(cols = c(male, female), names_to = "Indicator", values_to =  "counts") |>
    pivot_wider(names_from = dobyr, values_from = counts)

  population <- pops |>
    select(starts_with("popu"), sex) |>
    pivot_longer(cols = starts_with("popu"), names_to = "year", values_to = "count" ) |>
    mutate(year = gsub("population_", "", year)) |>
    group_by(year, sex) |>
    summarise(total_pop = sum(count)) |>
    arrange(sex)

  output_cbr <- cbind(output, population) |>
    select(year, Indicator, total, total_pop ) |>
    group_by(year) |>
    summarise(total = sum(total), total_pop = sum(total_pop)) |>
    mutate(cbr = round_excel((total/total_pop)*1000,2)) |>
    select(year, cbr) |>
    mutate(Indicator = "CBR") |>
    pivot_wider(names_from = year, values_from = cbr)

  output_ratio <- output |>
    pivot_wider(names_from = Indicator, values_from = total) |>
    mutate(total = male + female,
           Ratio = round((male/female),2)) |>
    select({{date_var}}, Ratio) |>
    pivot_wider(names_from = {{date_var}}, values_from = Ratio) |>
    mutate(Indicator = "Ratio") |>
    select(Indicator, starts_with("20"))
  output_ratio <- output_ratio[1,]

  fertility_rates <- calculate_fertility_rates(bth_data) |>
    rename(Indicator = fert_age_grp) |>
    filter(Indicator == "total")

  output <- rbind(output_counts, output_comp, output_ratio, output_cbr, fertility_rates)

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)

}
