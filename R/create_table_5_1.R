
#' Calculates Table 5.1 Deaths summary table
#'
#' Table 5.1 Summary statistics on mortality by year of occurrence
#'
#' @param data deaths data frame
#' @param num_yrs number of years to report on
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t5.1 <- create_t5.1(dth_data, tablename = "Table_5_1")
#'
create_t5.1 <- function(data, num_yrs=5, tablename = "Table_5_1"){

  max_year <- data %>% pull(dodyr) %>% max(na.rm = TRUE)
  years <- generate_year_sequence(max_year, num_yrs = num_yrs)

  output <- data |>
    filter(dodyr %in% years & sex != "not stated")|>
    group_by(sex, dodyr) |>
    rename(Indicator = sex) |>
    summarise(total = n())

output_counts <- output |>
    pivot_wider(names_from = Indicator, values_from = total)

output_comp <- dth_est |>
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
    pivot_wider(names_from = dodyr, values_from = counts)

  population <- pops |>
    select(starts_with("popu"), sex) |>
    pivot_longer(cols = starts_with("popu"), names_to = "year", values_to = "count" ) |>
    mutate(year = gsub("population_", "", year)) |>
    group_by(year, sex) |>
    summarise(total_pop = sum(count)) |>
    arrange(sex)

  output_cdr <- cbind(output, population) |>
    select(year, Indicator, total, total_pop ) |>
    group_by(year) |>
    summarise(total = sum(total), total_pop = sum(total_pop)) |>
    mutate(cdr = round_excel((total/total_pop)*1000,2)) |>
    select(year, cdr) |>
    mutate(Indicator = "CDR") |>
    pivot_wider(names_from = year, values_from = cdr)


  output <- rbind(output_counts, output_comp, output_cdr)

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
