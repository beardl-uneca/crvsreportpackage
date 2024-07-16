#' Creates Tables 3.5 or 3.7
#'
#' @description Table 3.5 Birth registration completeness by place of usual residence of mother and sex of newborn
#' @description Table 3.7 Death registration completeness by place of usual residence and sex of decedent
#'
#' @param data dataframe being used
#' @param est_data data frame of estimated data being used
#' @param date_var occurrence data being used e.g. dobyr, dodyr etc
#' @param data_year year the data is for
#' @param topic whether the data is for births or deaths
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frame of tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.5 <- create_t3.5_and_3.7(bth_data, bth_est, dobyr, 2022, topic = "births", tablename = "Table_3_5")
#'
create_t3.5_and_3.7 <- function(data, est_data, date_var, data_year = NA, topic = NA, tablename = NA) {
   # date_var <- enquo(date_var)
   # date_var_name <- quo_name(date_var)

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull({{date_var}}) %>% max(na.rm = TRUE)
  }

  # filter data based on topic

  if(topic == 'births'){
    data <- data %>% filter(!!sym(date_var) == data_year , tolower(sex) %in% c("male", "female") , is.na(sbind))
  } else {
    data <- data %>% filter(!!sym(date_var) == data_year , tolower(sex) %in% c("male", "female") )
  }

  counts <- data |>
    # filter({{date_var}} == data_year & tolower(sex) %in% c("male", "female") &
    #          if (topic == "births") is.na(sbind) else TRUE) |>
    group_by(rgn, sex) |>
    summarise(total = n())

  ests <- est_data |>
    filter(year == data_year) |>
    pivot_longer(cols = c("male", "female"), names_to = "sex", values_to = "count" ) |>
    group_by(rgn, sex) |>
    summarise(total_est = sum(count))

  #output <- left_join(counts, ests, by.x = c("rgn", "sex"), by.y = c("rgn", "sex"), all.x = TRUE)
  output <- left_join(counts, ests, by = c("rgn", "sex"))

  output2 <- output |>
    group_by(rgn) |>
    summarise(total = sum(total), total_est = sum(total_est)) |>
    mutate(sex = "total", .after = "rgn")

  output <- rbind(output, output2) |>
    mutate(completeness := round((total / total_est) * 100, 2)) |>
    pivot_wider(names_from = sex, values_from = c(total, total_est, completeness), names_sep = " ") |>
    mutate(rgn = ifelse(rgn %in% c("", " ", NA), "not stated", rgn)) |>
    arrange(rgn) %>%
    adorn_totals("row")

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)

  return(output)
}
