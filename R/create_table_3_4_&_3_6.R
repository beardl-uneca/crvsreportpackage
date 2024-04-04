#' Creates Table 3.4 and 3.6
#'
#' @param data data frame being used
#' @param est_data data frmae of estimated data being used
#' @param by_var the occurrence year being used e.g. dobyr or dodyr
#' @param topic whether the table is births or deaths data
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frames for tabulated versions of Table 3.4 and 3.6
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.4 <- create_t3.4_to_3.7(bth_data, bth_est, dobyr, topic = "births", tablename = "Table_3_4")
create_t3.4_and_3.6 <- function(data, est_data, by_var, topic = NA, tablename = NA) {
  by_var <- enquo(by_var)
  by_var_name <- quo_name(by_var)

  max_value <- data %>% pull(!!by_var) %>% max(na.rm = TRUE)

  counts <- data |>
    filter((!!by_var) %in% c((max_value - 5):max_value) &
             if (topic == "births") is.na(sbind) else TRUE) |>
    group_by(!!by_var, sex) |>
    summarise(total = n())

  ests <- est_data |>
    pivot_longer(cols = c("male", "female"), names_to = "sex", values_to = "count" ) |>
    group_by(year, sex) |>
    summarise(total_est = sum(count))

  output <- merge(counts, ests, by.x = c(by_var_name, "sex"), by.y = c("year", "sex"), all.x = TRUE)

  output <- output |>
    mutate(completeness = round((total / total_est) * 100, 2)) |>
    pivot_wider(names_from = sex, values_from = c(total, total_est, completeness))

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
