#' Creates Tables 3.8 and 3.9
#'
#' @description
#' Table 3.8 Adjustment of live births by place of usual residence of mother and sex of newborn
#' @description
#' Table 3.9 Adjustment of deaths by place of usual residence and sex of decedent
#'
#'
#' @param data name of data frame being used
#' @param date_var occurrence data variable being used
#' @param data_year year of data
#' @param est_data data frame of estimated data being used
#' @param by_var the occurrence year being used e.g. dobyr or dodyr
#' @param topic whether births or deahts is being run
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.8 <- create_t3.8_and_t3.9(data = bth_data, est_data = bth_est,
#' date_var = dobyr, data_year = 2022, by_var = rgn,
#' topic = "births", tablename = "Table_3_8")

#' t3.9 <- create_t3.8_and_t3.9(data = dth_data, est_data = dth_est,
#'                             date_var = dodyr, data_year = 2022, by_var = rgn,
#'                             topic = "deaths", tablename = "Table_3_9")
#'
create_t3.8_and_t3.9 <- function(data, est_data, date_var, data_year=NA, by_var, topic = NA, tablename = NA){
  # by_var <- enquo(by_var)
  # by_var_name <- quo_name(by_var)

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull({{date_var}}) %>% max(na.rm = TRUE)
  }

  # filter data based on topic

  if(topic == 'births'){
    data <- data %>% filter(!!sym(date_var) == data_year, is.na(sbind))
  } else {
    data <- data %>% filter(!!sym(date_var) == data_year)
  }

  counts <- data |>
   # filter({{date_var}} == data_year & if (topic == "births") is.na(sbind) else TRUE) |>
   # group_by(!!by_var, sex) |>
    group_by(rgn, sex) |>
    summarise(total = n()) |>
    pivot_wider(names_from = sex, values_from = total, values_fill = 0) |>
    select(-`not stated`) |>
    adorn_totals("col")

  ests <- est_data |>
    filter(year == data_year) |>
    group_by(rgn) |>
    summarise(female_est = sum(female), male_est = sum(male)) |>
    adorn_totals("col") |>
    rename(total_est = Total)

  output <- left_join(counts, ests, by = c("rgn")) |>
    mutate(f_comp = round_excel(female/female_est, 2),
           m_comp = round_excel(male/male_est, 2),
           t_comp = round_excel(Total/total_est, 2)) |>
    mutate(f_adj = ceiling(female/f_comp),
           m_adj = ceiling(male/m_comp),
           t_adj = ceiling(Total/t_comp)) |>
    select(rgn, male, m_adj, female, f_adj, Total, t_adj)|>
    adorn_totals("row")


  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}

